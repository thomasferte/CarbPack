
#' @title
#' tracker_stop
#'
#' @description
#' Calcule l'energie requise et l'empreinte carbone du programme depuis
#' le moment ou le tracker a ete lance. Peut egalement cumuler ces
#' indicateurs en fonction du nombre de fois ou le programme a ete lance.
#' A lancer apres l'execution du programme ou a sa fin.
#'
#'
#' @param hardware la liste creee precedemment
#' par la fonction 'detect_hardware'
#' @param start_time un objet POSIXct, de preference l'objet cree par la
#' fonction 'tracker_start'
#' @param path chemin vers un fichier .rds, utilise pour stocker et
#' recuperer le temps d'execution total. Laisser vide pour ne produire
#' qu'une iteration. Renseigner la chaine de caractere du chemin vers un
#' fichier .rds deja existant ou non pour cumuler la duree d'execution
#' avec les fois precedentes et suivantes
#'
#' @return Renvoie un rapport dans la console, et si l'argument 'path'
#' est renseigne, sauvegarde un fichier .rds vers ce chemin.
#' @export
#'
#' @examples #tracker_stop(hardware = detect_hardware(),
#' #start_time = Sys.time(),
#' #path = "./chemin/vers/fichier.rds")
tracker_stop<-function(hardware,
                       start_time,
                       path = NULL) {


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

  # Verifier le statut du 'path'
  if (!is.null(path)) {
    if(file.exists(path)) {
      loaded_time<-readRDS(path)
      # Verifier que l'objet est bien un difftime
      if (!inherits(loaded_time, "difftime")) {
        stop("L'objet charge n'est pas de classe 'difftime'.")
      }
    } else { # sinon creer le repertoire et l'objet
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      # Creer un objet difftime egal a 0
      loaded_time <- as.difftime(0, units = "secs")
      # Sauvegarder l'objet difftime egal a 0 sur le chemin 'path'
      saveRDS(loaded_time, file = path)
      message("Le chemin n'existait pas.\nUn objet de duree egale a 0 a ete sauvegarde dans ", path)
    }
  }


  # Calcul du temps ecoule
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time

  # ajout a elapsed_time l'objet difftime si existe
  if (!is.null(path)) {
    elapsed_time<-elapsed_time + loaded_time
  }

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
  # utiliser substitute pour recuperer le nom de l'objet
  suppr_temps<-as.character(substitute(start_time))
  rm(list = suppr_temps, envir = .GlobalEnv)

  # sauvegarder l'objet difftime si demande

  if (!is.null(path)) {
    saveRDS(object = elapsed_time, file = path)
    message("Le temps total d'execution a ete sauvegarde dans ", path)


  }
}
