## app.R
library(shiny)
source("R/bibliotheque_jeu.R")

ui <- fluidPage(
  titlePanel("Slitherlink - Projet R"),
  sidebarLayout(
    sidebarPanel(
      # On utilise masques_niveaux au lieu de banque_puzzles
      selectInput("choix_niveau", "Difficulté :", choices = names(masques_niveaux)),
      actionButton("btn_nouveau", "Générer une autre grille", class = "btn-success", style = "width:100%; margin-bottom:10px;"),
      hr(),
      actionButton("btn_verifier", "Vérifier mon tracé", class = "btn-primary", style = "width:100%; margin-bottom:10px;"),
      actionButton("btn_solution", "Voir la solution", class = "btn-warning", style = "width:100%; margin-bottom:10px;"),
      actionButton("btn_effacer", "Effacer", class = "btn-danger", style = "width:100%;"),
      hr(),
      textOutput("message_resultat")
    ),
    mainPanel(
      plotOutput("grille_plot", height = "600px", width = "600px", click = "clic_souris")
    )
  )
)

server <- function(input, output, session) {
  etat <- reactiveValues(puzzle = NULL, traits_h = NULL, traits_v = NULL, sol_h = NULL, sol_v = NULL, message = "")
  
  # Fonction pour charger une grille en utilisant le nouveau générateur
  charger_grille <- function() {
    nouveau_puzzle <- generer_puzzle_dynamique(input$choix_niveau)
    
    etat$puzzle <- nouveau_puzzle$grille
    etat$sol_h <- nouveau_puzzle$sol_h
    etat$sol_v <- nouveau_puzzle$sol_v
    
    lig <- nrow(etat$puzzle)
    col <- ncol(etat$puzzle)
    etat$traits_h <- matrix(0, nrow = lig + 1, ncol = col)
    etat$traits_v <- matrix(0, nrow = lig, ncol = col + 1)
    etat$message <- "Nouvelle grille générée. Tracez vos lignes."
  }
  
  # Déclencheurs
  observeEvent(input$choix_niveau, { charger_grille() })
  observeEvent(input$btn_nouveau, { charger_grille() })
  
  observeEvent(input$btn_effacer, {
    etat$traits_h[] <- 0
    etat$traits_v[] <- 0
    etat$message <- "Grille effacée."
  })
  
  observeEvent(input$btn_solution, {
    etat$traits_h <- etat$sol_h
    etat$traits_v <- etat$sol_v
    etat$message <- "Voici la solution de cette grille."
  })
  
  observeEvent(input$btn_verifier, {
    if (verifier_grille(etat$puzzle, etat$traits_h, etat$traits_v)) {
      etat$message <- "Félicitations ! Les contraintes sont respectées."
    } else {
      etat$message <- "Il y a des erreurs. Vérifiez vos traits."
    }
  })
  
  # Dessin
  observeEvent(input$clic_souris, {
    x <- input$clic_souris$x; y <- input$clic_souris$y
    col_p <- round(x); lig_p <- round(y)
    dist_h <- abs(y - lig_p); dist_v <- abs(x - col_p)
    lig <- nrow(etat$puzzle); col <- ncol(etat$puzzle)
    
    if (dist_h < 0.3 && x >= 1 && x < col + 1 && lig_p >= 1 && lig_p <= lig + 1) {
      c_idx <- floor(x)
      etat$traits_h[lig_p, c_idx] <- 1 - etat$traits_h[lig_p, c_idx]
    } else if (dist_v < 0.3 && y >= 1 && y < lig + 1 && col_p >= 1 && col_p <= col + 1) {
      l_idx <- floor(y)
      etat$traits_v[l_idx, col_p] <- 1 - etat$traits_v[l_idx, col_p]
    }
    etat$message <- "" 
  })
  
  # Affichage texte
  output$message_resultat <- renderText({ etat$message })
  
  # Affichage Graphique
  output$grille_plot <- renderPlot({
    req(etat$puzzle)
    lig <- nrow(etat$puzzle); col <- ncol(etat$puzzle)
    par(mar = c(1,1,1,1))
    plot(0, 0, type = "n", xlim = c(0.5, col + 1.5), ylim = c(lig + 1.5, 0.5), axes = F, asp = 1)
    
    for (i in 1:(lig+1)) for (j in 1:(col+1)) points(j, i, pch = 19, cex = 1.5)
    for (i in 1:lig) for (j in 1:col) if (!is.na(etat$puzzle[i,j])) text(j+0.5, i+0.5, etat$puzzle[i,j], cex=2, font=2)
    
    for (i in 1:(lig+1)) for (j in 1:col) if (etat$traits_h[i,j] == 1) segments(j, i, j+1, i, lwd=5, col="blue")
    for (i in 1:lig) for (j in 1:(col+1)) if (etat$traits_v[i,j] == 1) segments(j, i, j, i+1, lwd=5, col="blue")
  })
}

shinyApp(ui, server)