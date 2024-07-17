
#' Title
#'
#'
#' @param hardware la liste creee par la fonction 'detect_hardware'
#' @param start_time un objet POSIXct, de preference l'objet cree par la
#' fonction 'tracker_start'
#'
#' @return des choses dans la console
#' @export
#'
#' @examples #tracker_stop<-function(start = tracker)
tracker_stop<-function(hardware,
                       start_time) {


  # Verifier si l'argument est manquant
  if (missing(hardware)) {
    stop("L'argument 'hardware' est requis et doit etre la liste creee par 'detect_hardware'.")
  }




  # Verifier si l'argument est manquant
  if (missing(start_time)) {
    stop("L'argument 'start_time' est requis et doit etre de type POSIXct de longueur 1.")
  }

  # Verifier si l'argument est un objet POSIXct
  if (!inherits(start_time, "POSIXct")) {
    stop("L'argument 'start_time' doit etre un objet de type POSIXct.")
  }


  # Calcul du temps ecoule
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time

  # Conversion du temps ecoule en differentes unites
  elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
  elapsed_minutes <- as.numeric(elapsed_time, units = "mins")
  elapsed_hours <- as.numeric(elapsed_time, units = "hours")

  # Affichage du temps ecoule
  cat(sprintf("Temps ecoule : %.2f secondes\n", elapsed_seconds))
  cat(sprintf("Temps ecoule : %.2f minutes\n", elapsed_minutes))
  cat(sprintf("Temps ecoule : %.2f heures\n", elapsed_hours))

  #### calcul de l'energie consommee et de l'empreinte carbone ####

  #### carbon intensity de la France ####
  # valeurs juillet 2023
  carbon_intensity<-0.06207

  #### calcul de l'energie consommee en Wh ####
  energy_needed<-
    elapsed_hours*(hardware$ram_gb*0.3725+hardware$cpu_TDP)



#### calcul de l'empreinte carbone ####
carbon_footprint<-energy_needed*carbon_intensity

  #### phrase a afficher avec les metriques ####
  elapsed_hours_rounded<-round(elapsed_hours,digits = 2)
  elapsed_minutes_rounded<-round(elapsed_minutes, digits = 2)
  energy_needed_rounded<-round(energy_needed, digits = 2)
  carbon_footprint_rounded<-round(carbon_footprint, digits = 2)
  print(paste0("Ce programme s'est execute en ",
               elapsed_hours_rounded, " heures, soit ",
               elapsed_minutes_rounded, " minutes, ",
               "sur ", hardware$number_of_cores, " CPUs ",
               hardware$cpu_data,
               " et a necessite ",
               energy_needed_rounded, " Wh. En France, ceci correspond a",
               " une empreinte carbone de ",
               carbon_footprint_rounded, " g CO2e."))

#### suppression du start_time de l'environnement ####
  rm(start_time, envir = .GlobalEnv)


}
