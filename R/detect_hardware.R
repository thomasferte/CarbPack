#' @title
#' detect_hardware
#'
#' @description
#' Function to detect the hardware on which the program is running.
#' This function currently works only on Windows operating systems.
#' Unexpected results may occur with other operating systems at the moment.
#'
#' @param OS NULL by default. A character string of length 1
#' to specify the name of the operating system.
#' @param TDP NULL by default. A numeric (integer) value of length 1
#' to specify the TDP value if it is not available in the database.
#' If the parameter is NULL and the value is not found, the TDP value
#' is approximated to 15W per core.
#' @param tracker FALSE by default. If TRUE, the tracker is launched
#' along with this function. If FALSE, the tracker must be launched separately.
#' Do not use at the moment, keep it as FALSE.
#'
#' @return Returns a list with several parameters related to the hardware
#' of the PC on which the program is running.
#' @export
#'
#' @examples #do not run
#' #result_hardware <- detect_hardware()
#' #print(result_hardware)
detect_hardware<-function(OS = NULL,
                          TDP = NULL,
                          tracker = FALSE) {

  #### verification des arguments fournis le cas echeant ####
  # Verifier si OS est fourni et valide
  if (!is.null(OS)) {
    if (!is.character(OS)) {
      stop("L'argument 'OS' doit etre une chaine de caracteres.")
    }
    if (length(OS) != 1) {
      stop("L'argument 'OS' doit etre de longueur 1.")
    }
  }

  # Verifier si TDP est fourni et valide
  if (!is.null(TDP)) {
    if (!is.numeric(TDP) || TDP %% 1 != 0) {
      stop("L'argument 'TDP' doit etre un entier.")
    }
    if (length(TDP) != 1) {
      stop("L'argument 'TDP' doit etre de longueur 1.")
    }
  }



  #### identifier systeme d'exploitation ####
  # Obtenir les informations sur le systeme
  sys_info<-Sys.info()


  # Verifier le nom du systeme d'exploitation si force
  if(!is.null(OS)) {
    os_name<-OS
    message("Le programme tourne sous ", os_name)

  } else {
    # sinon on le recherche manuellement
    os_name<-sys_info["sysname"]
    # Afficher le systeme d'exploitation
    if(os_name == "Windows") {
      message("Le programme tourne sous Windows.")
    } else if (os_name == "Darwin") {
      message("Le programme tourne sous macOS.")
    } else if (os_name == "Linux") {
      message("Le programme tourne sous Linux")
    } else {
      message("Le programme tourne sous un autre systeme d'exploitation : ", os_name)
    }
  }

  #### processeur CPU ####
  # il faut recuperer le nombre de coeurs du CPU
  if(os_name == "Windows"){
    cpu_info<-system(command = "wmic cpu get name,numberofcores",
                     intern = TRUE) # intern = T pour pouvoir stocker le resultat
    cat("Informations sur le CPU :\n", cpu_info, "\n")

    # Nettoyer la sortie pour extraire le nombre de coeurs
    cpu_data<-cpu_info[-1]  # Ignorer la premiere ligne (entete)
    cpu_data<-gsub("\\s+", " ", cpu_data)  # Remplacer les espaces multiples par un espace unique
    cpu_data<-trimws(cpu_data)  # Supprimer les espaces blancs en debut et fin de ligne
    cpu_data<-cpu_data[cpu_data != ""]  # Supprimer les lignes vides

    # on reutilise cpu_data pour recuperer par regex le nom du processeur


    # Extraire le nombre de coeurs
    # on tokenise la chaine de caracteres par les espaces
    # puis on conserve uniquement le dernier token qui est le nombre de coeurs
    number_of_cores<-as.numeric(sapply(strsplit(cpu_data, " "),
                                       function(x) x[length(x)]))

    ## extraire les infos de cpu mais sans le nombre de coeurs a la fin
    # on remplace le chiffre a la fin de la chaine par ""
    cpu_data_report<-sub(pattern = "\\s*\\d+$",
                         replacement = "",
                         x = cpu_data)

    #### memoire vive RAM ####
    # il faut recuperer la valeur en GB
    ram_info<-system("wmic memorychip get capacity",
                     intern = TRUE) # intern = T pour pouvoir stocker le resultat
    # Extraire et nettoyer les valeurs de RAM en octets
    # Enlever les espaces blancs et convertir en numerique
    ram_values<-as.numeric(gsub("\\s", "", ram_info[-1]))
    # Retirer les valeurs non numeriques
    ram_values<-ram_values[!is.na(ram_values)]
    # Convertir les valeurs de RAM de octets a GB et les additionner
    ram_gb<-sum(ram_values)/1024^3 # sum pour additionner les valeurs

  } else if(os_name == "Linux"){
    ### CPU info
    cpu_info <- system("lscpu", intern = TRUE)
    number_of_cores <- parallel::detectCores()
    cpu_data <- strsplit(cpu_info[14], ":\\s")[[1]][2] |>
      trimws()
    cpu_data_report <- cpu_data

    ### RAM Infor
    ram_gb <- system("free -h | grep Mem: | awk '{print $2}'",
                       intern = TRUE) |>
      sub(pattern = "Gi", replacement = "") |>
      as.numeric()
  }




  # a partir de cpu_data, recuperer le nom de version du CPU
  cpu_version <- NULL
  # en fonction de si on travaille avec intel ou AMD : executer 2 fonctions
  # Utiliser une expression reguliere pour extraire
  # la version du processeur (comme "i7-7700")
  if (grepl("^Intel| Intel", cpu_data)) {
    cpu_version<-regmatches(
      cpu_data, regexpr("i[0-9]-[0-9]+[A-Za-z]{0,3}", cpu_data))
  }

  # pour AMD, regex qui garde tout avant le numero de processeur
  if (grepl("^AMD| AMD", cpu_data)) {
    cpu_version<-sub(pattern = "(.*?\\d{3,4}[a-zA-Z0-9]{0,3})\\s.*",
                     replacement = "\\1", # le premier groupe capturant
                     x = cpu_data)
  }

  #### importation donnees de processeurs intel et AMD ####
  data<-import_CPU()

  #### identifier le TDP (total) du processeur ####
  # sinon estimer la valeur a 15W par coeur
  # verifier si le vecteur est non nul
  if(length(cpu_version)>0) {
    if(cpu_version %in% data$`Processor Number`) {
      cpu_TDP<-
        data$TDP[data$`Processor Number` == cpu_version]
      cat("La version du processeur est presente. Valeur du TDP :", cpu_TDP, "W\n")
    } else {
      cpu_TDP<-number_of_cores*15
      cat("La version du processeur n'est pas presente dans le dataframe.\n",
          "La valeur estimee du TDP est", cpu_TDP, "W\n")
    }
  } else {
    # verifier si une valeur a ete forcee
    if(is.null(TDP)) {
      cpu_TDP<-number_of_cores*15
      cat("La version du processeur n'est pas presente dans le dataframe.\n",
          "La valeur estimee du TDP est", cpu_TDP, "W\n")
    } else {
      cpu_TDP<-TDP
      cat("La version du processeur n'est pas presente dans le dataframe.\n",
          "La valeur estimee du TDP est", cpu_TDP, "W\n")
    }
  }
  ## Creer une liste qui retourne tous les elements utiles pour plus tard
  return(list(sys_info = sys_info,
              os_name = os_name,
              cpu_info = cpu_info,
              cpu_data = cpu_data,
              number_of_cores = number_of_cores,
              cpu_data_report, cpu_data_report,
              ram_gb = ram_gb,
              cpu_TDP = cpu_TDP))

  # fin
}

