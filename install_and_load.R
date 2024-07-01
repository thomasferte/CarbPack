### script test


# Liste des packages à installer ou charger
packages <- c("tidyverse", "readxl", "gtsummary", "lubridate", "epiR",
              "ggmap")



# Fonction pour installer et charger les packages
install_and_load_package <- function(pkg) {
  # Définition du miroir CRAN
  cran.mirror <- "http://cran.r-project.org"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = cran.mirror, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Installer et charger les packages
for (pkg in packages) {
  install_and_load_package(pkg)
}
