
#' Title
#'
#' @param OS NULL par defaut, chaine de caracteres
#' de longueur 1 pour forcer le nom de l'OS
#' @param TDP NULL par defaut, valeur numerique (entier)
#' de longueur 1 pour forcer
#' la valeur du TDP, si elle n'est pas dispo dans la base de donnees
#' si le parametre est NULL et que la valeur n'est pas retrouvee, alors
#' la valeur du TDP est approximee a 15 fois le nombre de coeurs
#' @param tracker FALSE par defaut. Si TRUE, le tracker se lance en meme temps
#' que cette fonction. si FALSE, il faut lancer le tracker separement
#' ne pas utiliser pour l'instant
#'
#' @return Retourne une liste avec plusieurs parametres lies au materiel
#' du PC sur lequel l'analyse tourne
#' @export
#'
#' @examples #ne pas lancer
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
    } else {
      message("Le programme tourne sous un autre systeme d'exploitation : ", os_name)
    }
  }

  #### processeur CPU ####
  # il faut recuperer le nombre de coeurs du CPU
  cpu_info<-system(command = "wmic cpu get name,numberofcores",
                   intern = TRUE) # intern = T pour pouvoir stocker le resultat
  cat("Informations sur le CPU :\n", cpu_info, "\n")
  cpu_info

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


  # a partir de cpu_data, recuperer le nom de version du CPU

  # en fonction de si on travaille avec intel ou AMD : executer 2 fonctions
  # Utiliser une expression reguliere pour extraire
  # la version du processeur (comme "i7-7700")
  if (grepl("^Intel", cpu_data)) {
    cpu_version<-regmatches(
      cpu_data, regexpr("i[0-9]-[0-9]+[A-Za-z]{0,3}", cpu_data))
  }

  # pour AMD, regex qui garde tout avant le numero de processeur
  if (grepl("^AMD", cpu_data)) {
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
              ram_info = ram_info,
              ram_values = ram_values,
              ram_gb = ram_gb,
              cpu_TDP = cpu_TDP))

  # fin
}

