
# fonction d'import des processeurs

#' Title
#'
#' @return un df avec les TDP des processeurs AMD
#' @import readxl
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @examples #non
import_CPU<-function() {
  # corriger la note (ligne inutile)
  `Processor Number`<-NULL

# chemin vers les fichiers excels
  chemin_fichier<-system.file("extdata", "cpu_amd.rds",
                              package = "CarbPack")

  # Check if the file exists
  if (chemin_fichier == "") {
    stop("The file does not exist or wrong pathway")
  }

  #### importation donnees de processeurs intel et AMD ####
  cpu_amd<-readRDS(chemin_fichier)


  # chemin vers les fichiers excels
  chemin_fichier<-system.file("extdata", "cpu_intel_fixe.rds",
                              package = "CarbPack")

  # Check if the file exists
  if (chemin_fichier == "") {
    stop("The file does not exist or wrong pathway")
  }

  cpu_intel_fixe<-readRDS(chemin_fichier)

  # chemin vers les fichiers excels
  chemin_fichier<-system.file("extdata", "cpu_intel_portable.rds",
                              package = "CarbPack")

  # Check if the file exists
  if (chemin_fichier == "") {
    stop("The file does not exist or wrong pathway")
  }

  cpu_intel_portable<-readRDS(chemin_fichier)




#### fusionner les 3 fichiers en un seul ####

data<-rbind(cpu_intel_fixe,
            cpu_intel_portable,
            cpu_amd)

  # retirer les lignes NA
  data<-data %>%
    dplyr::filter(!is.na(`Processor Number`))

  return(data)
}
