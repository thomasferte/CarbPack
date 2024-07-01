#### application de l'outil d'empreinte carbone ####


#### récupérer les informations matérielles ####
source(file = "./hardware.R", encoding = "utf-8")



# # début du décompte
# start_time <- Sys.time() # à lancer si vous ne relancez pas hardware.R

#### début du programme ici ####

# Calcul lourd : effectuer de nombreuses opérations mathématiques
for (i in 1:10^6) {
  x <- rnorm(1000)  # Générer un grand nombre de variables aléatoires
  y <- matrix(x, nrow=100, ncol=10)
  z <- svd(y)  # Calculer la décomposition en valeurs singulières
}

# Ajouter une pause explicite
Sys.sleep(60)  # Pause d'une minute



#### fin du programme ici ####


source(file = "./elapsed_time.R", encoding = "utf-8")

