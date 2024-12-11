# Function to import processor data

#' @title
#' import_CPU
#'
#' @description
#' Function to import TDP data for AMD and Intel processors
#' from the package's internal files.
#'
#' @return A dataframe containing the TDP of AMD and Intel processors.
#' @import readxl
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @examples #do not run
import_CPU <- function() {
  # Prevent note (useless line)
  `Processor Number` <- NULL

  # Path to the AMD processors file
  chemin_fichier <- system.file("extdata", "cpu_amd.rds",
                                package = "CarbPack")

  # Check if the file exists
  if (chemin_fichier == "") {
    stop("The file does not exist or wrong pathway")
  }

  # Import AMD processors data
  cpu_amd <- readRDS(chemin_fichier)

  # Path to the Intel fixed processors file
  chemin_fichier <- system.file("extdata", "cpu_intel_fixe.rds",
                                package = "CarbPack")

  # Check if the file exists
  if (chemin_fichier == "") {
    stop("The file does not exist or wrong pathway")
  }

  # Import Intel fixed processors data
  cpu_intel_fixe <- readRDS(chemin_fichier)

  # Path to the Intel portable processors file
  chemin_fichier <- system.file("extdata", "cpu_intel_portable.rds",
                                package = "CarbPack")

  # Check if the file exists
  if (chemin_fichier == "") {
    stop("The file does not exist or wrong pathway")
  }

  # Import Intel portable processors data
  cpu_intel_portable <- readRDS(chemin_fichier)

  #### Merge the three datasets into a single one ####
  data <- rbind(cpu_intel_fixe,
                cpu_intel_portable,
                cpu_amd)

  # Remove rows with NA
  data <- data %>%
    dplyr::filter(!is.na(`Processor Number`))

  return(data)
}
