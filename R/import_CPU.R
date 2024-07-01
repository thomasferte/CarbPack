
# fonction d'import des processeurs

#' Title
#'
#' @return un df avec les TDP des processeurs AMD
#' @import readxl
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @examples #non
import_CPU<-function() {
# chemin vers les fichiers excels
  chemin_fichier<-system.file("extdata", "AMD_processors.xlsx",
                              package = "CarbPack")

  # Check if the file exists
  if (!file.exists(chemin_fichier)) {
    stop("The file does not exist or wrong pathway")
  }

  #### importation données de processeurs intel et AMD ####
  cpu_amd<-readxl::read_excel(chemin_fichier,
                              sheet = 1)


  # chemin vers les fichiers excels
  chemin_fichier<-system.file("extdata", "Intel-Core-Desktop-Boxed-Processors-Comparison-Chart.xlsx",
                              package = "CarbPack")

  # Check if the file exists
  if (!file.exists(chemin_fichier)) {
    stop("The file does not exist or wrong pathway")
  }

  cpu_intel_fixe<-readxl::read_excel(
    path = chemin_fichier,
    sheet = 1,
    skip = 6)


  # chemin vers les fichiers excels
  chemin_fichier<-system.file("extdata", "Intel-Core-Comparsion.xlsx",
                              package = "CarbPack")

  # Check if the file exists
  if (!file.exists(chemin_fichier)) {
    stop("The file does not exist or wrong pathway")
  }

  cpu_intel_portable<-readxl::read_excel(
    path = chemin_fichier,
    sheet = 1,
    skip = 5)


  # on conserve que 2 colonnes utiles
  cpu_intel_fixe<-cpu_intel_fixe %>%
    dplyr::select(
      `Processor Number`,
      "TDP" = `Processor Base Power (previously Thermal Design Power (TDP)) \r\n(W)`
    )
  cpu_intel_portable<-cpu_intel_portable %>%
    dplyr::select(
      `Processor Number`,
      "TDP" = `Processor Base Power (previously known as TDP)`
    )
  cpu_amd<-cpu_amd %>%
    dplyr::select(
      "Processor Number" = Name,
      "TDP" = `Default TDP`
    )
  cpu_intel_portable<-cpu_intel_portable %>%
    dplyr::mutate(TDP = sub(pattern = " W",
                            replacement = "",
                            x = cpu_intel_portable$TDP))

  cpu_intel_portable<-cpu_intel_portable %>%
    dplyr::mutate(TDP = as.numeric(cpu_intel_portable$TDP))

  ## penser à ne garder que le chiffre dans la table AMD et à convertir en num
  cpu_amd<-cpu_amd %>%
    dplyr::mutate(TDP = sub(pattern = "W",
                            replacement = "",
                            x = cpu_amd$TDP))

  cpu_amd<-cpu_amd %>%
    dplyr::mutate(TDP = as.numeric(cpu_amd$TDP))

  ## modifier le nom des processeurs AMD, retirer le ™ ##
  cpu_amd<-cpu_amd %>%
    dplyr::mutate(`Processor Number` = sub(pattern = "™",
                                           replacement = "",
                                           x = cpu_amd$`Processor Number`))


#### fusionner les 3 fichiers en un seul ####

data<-rbind(cpu_intel_fixe,
            cpu_intel_portable,
            cpu_amd)

  # retirer les lignes NA
  data<-data %>%
    dplyr::filter(!is.na(`Processor Number`))

  return(data)
}
