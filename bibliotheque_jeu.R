# R/bibliotheque_jeu.R

# 1. Formes de base (1 = intérieur, 0 = extérieur)
# Formes complexes : serpents, spirales, zigzag, U, T...
# Ces matrices sont garanties sans carré et sans croix.

formes_3x3 <- list(
  matrix(c(1,1,0, 0,1,1, 0,0,1), 3, 3, byrow=TRUE), # Escalier
  matrix(c(1,1,1, 1,0,0, 1,1,1), 3, 3, byrow=TRUE), # Forme C
  matrix(c(1,0,1, 1,0,1, 1,1,1), 3, 3, byrow=TRUE), # Forme U
  matrix(c(1,1,1, 0,1,0, 0,1,0), 3, 3, byrow=TRUE), # Forme T
  matrix(c(0,1,1, 0,1,0, 1,1,0), 3, 3, byrow=TRUE)  # Forme S
)

formes_4x4 <- list(
  matrix(c(1,1,1,0, 0,0,1,0, 0,1,1,0, 0,1,1,1), 4, 4, byrow=TRUE), # Serpent
  matrix(c(1,1,0,0, 1,1,1,0, 0,1,1,1, 0,0,1,1), 4, 4, byrow=TRUE), # Escalier épais
  matrix(c(1,1,1,1, 1,0,0,0, 1,1,1,0, 0,0,1,0), 4, 4, byrow=TRUE), # Spirale
  matrix(c(0,1,1,0, 1,1,1,1, 1,1,1,1, 0,1,1,0), 4, 4, byrow=TRUE), # Losange
  matrix(c(1,0,0,0, 1,0,0,0, 1,1,1,1, 0,0,0,1), 4, 4, byrow=TRUE)  # Manivelle
)

formes_5x5 <- list(
  matrix(c(1,1,1,1,1, 0,0,0,0,1, 1,1,1,1,1, 1,0,0,0,0, 1,1,1,1,1), 5, 5, byrow=TRUE), # Long serpent
  matrix(c(1,1,0,1,1, 1,1,0,1,1, 1,1,1,1,1, 1,1,1,1,1, 0,1,1,1,0), 5, 5, byrow=TRUE), # Forme en T épais
  matrix(c(0,0,1,0,0, 0,1,1,1,0, 1,1,1,1,1, 0,1,1,1,0, 0,0,1,0,0), 5, 5, byrow=TRUE), # Grand losange
  matrix(c(1,1,1,1,1, 1,0,0,0,0, 1,0,1,1,1, 1,0,1,0,1, 1,1,1,0,1), 5, 5, byrow=TRUE), # Grosse spirale
  matrix(c(0,1,1,1,0, 0,1,0,1,0, 1,1,0,1,1, 1,0,0,0,1, 1,1,0,1,1), 5, 5, byrow=TRUE)  # Arche
)

masques_niveaux <- list("Niveau 1 (3x3)" = formes_3x3, "Niveau 2 (4x4)" = formes_4x4, "Niveau 3 (5x5)" = formes_5x5)

# 2. Générateur de grilles
generer_puzzle_dynamique <- function(niveau_nom) {
  masques <- masques_niveaux[[niveau_nom]]
  masque <- masques[[sample(length(masques), 1)]]
  lignes <- nrow(masque)
  colonnes <- ncol(masque)
  
  # Calcul des traits de la solution
  sol_h <- matrix(0, nrow = lignes + 1, ncol = colonnes)
  sol_v <- matrix(0, nrow = lignes, ncol = colonnes + 1)
  
  for (i in 1:(lignes + 1)) {
    for (j in 1:colonnes) {
      haut <- if (i == 1) 0 else masque[i - 1, j]
      bas <- if (i == lignes + 1) 0 else masque[i, j]
      if (haut != bas) sol_h[i, j] <- 1
    }
  }
  
  for (i in 1:lignes) {
    for (j in 1:(colonnes + 1)) {
      gauche <- if (j == 1) 0 else masque[i, j - 1]
      droite <- if (j == colonnes + 1) 0 else masque[i, j]
      if (gauche != droite) sol_v[i, j] <- 1
    }
  }
  
  # Calcul des chiffres
  grille <- matrix(NA, nrow = lignes, ncol = colonnes)
  for (i in 1:lignes) {
    for (j in 1:colonnes) {
      grille[i, j] <- sol_h[i, j] + sol_h[i+1, j] + sol_v[i, j] + sol_v[i, j+1]
    }
  }
  
  # Masquage partiel des chiffres
  for (i in 1:lignes) {
    for (j in 1:colonnes) {
      if (runif(1) > 0.40) {  
        grille[i, j] <- NA
      }
    }
  }
  
  return(list(grille = grille, sol_h = sol_h, sol_v = sol_v))
}

# 3. Fonction pour vérifier le tracé du joueur
verifier_grille <- function(puzzle, traits_horizontaux, traits_verticaux) {
  lignes <- nrow(puzzle); colonnes <- ncol(puzzle); erreurs <- 0
  for (i in 1:lignes) {
    for (j in 1:colonnes) {
      valeur <- puzzle[i, j]
      if (!is.na(valeur)) {
        total <- sum(c(traits_horizontaux[i,j], traits_horizontaux[i+1,j], 
                       traits_verticaux[i,j], traits_verticaux[i,j+1]))
        if (total != valeur) erreurs <- erreurs + 1
      }
    }
  }
  return(erreurs == 0)
}