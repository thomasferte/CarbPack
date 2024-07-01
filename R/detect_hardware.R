
detect_hardware<-function(OS = NULL,
                          TDP = NULL,
                          tracker = FALSE) {
  #### identifier système d'exploitation ####
  # Obtenir les informations sur le système
  sys_info<-Sys.info()

  # Vérifier le nom du système d'exploitation
  os_name<-sys_info["sysname"]

  # Afficher le système d'exploitation
  if(os_name == "Windows") {
    message("Le programme tourne sous Windows.")
  } else if (os_name == "Darwin") {
    message("Le programme tourne sous macOS.")
  } else {
    message("Le programme tourne sous un autre système d'exploitation : ", os_name)
  }

  #### processeur CPU ####
  # il faut récupérer le nombre de coeurs du CPU
  cpu_info<-system(command = "wmic cpu get name,numberofcores",
                   intern = TRUE) # intern = T pour pouvoir stocker le résultat
  cat("Informations sur le CPU :\n", cpu_info, "\n")
  cpu_info

  # Nettoyer la sortie pour extraire le nombre de cœurs
  cpu_data<-cpu_info[-1]  # Ignorer la première ligne (en-tête)
  cpu_data<-gsub("\\s+", " ", cpu_data)  # Remplacer les espaces multiples par un espace unique
  cpu_data<-trimws(cpu_data)  # Supprimer les espaces blancs en début et fin de ligne
  cpu_data<-cpu_data[cpu_data != ""]  # Supprimer les lignes vides

  # on réutilise cpu_data pour récupérer par regex le nom du processeur


  # Extraire le nombre de cœurs
  # on tokenise la chaine de caractères par les espaces
  # puis on conserve uniquement le dernier token qui est le nombre de coeurs
  number_of_cores<-as.numeric(sapply(strsplit(cpu_data, " "),
                                     function(x) x[length(x)]))

  ## extraire les infos de cpu mais sans le nombre de coeurs à la fin
  # on remplace le chiffre à la fin de la chaine par ""
  cpu_data_report<-sub(pattern = "\\s*\\d+$",
                       replacement = "",
                       x = cpu_data)


  #### mémoire vive RAM ####
  # il faut récupérer la valeur en GB
  ram_info<-system("wmic memorychip get capacity",
                   intern = TRUE) # intern = T pour pouvoir stocker le résultat
  # Extraire et nettoyer les valeurs de RAM en octets
  # Enlever les espaces blancs et convertir en numérique
  ram_values<-as.numeric(gsub("\\s", "", ram_info[-1]))
  # Retirer les valeurs non numériques
  ram_values<-ram_values[!is.na(ram_values)]
  # Convertir les valeurs de RAM de octets à GB et les additionner
  ram_gb<-sum(ram_values)/1024^3 # sum pour additionner les valeurs


  # à partir de cpu_data, récupérer le nom de version du CPU

  # en fonction de si on travaille avec intel ou AMD : exécuter 2 fonctions
  # Utiliser une expression régulière pour extraire
  # la version du processeur (comme "i7-7700")
  if (grepl("^Intel", cpu_data)) {
    cpu_version<-regmatches(
      cpu_data, regexpr("i[0-9]-[0-9]+[A-Za-z]{0,3}", cpu_data))
  }

  # pour AMD, regex qui garde tout avant le numéro de processeur
  if (grepl("^AMD", cpu_data)) {
    cpu_version<-sub(pattern = "(.*?\\d{3,4}[a-zA-Z0-9]{0,3})\\s.*",
                     replacement = "\\1", # le premier groupe capturant
                     x = cpu_data)
  }

  #### importation données de processeurs intel et AMD ####
  data<-import_CPU()

  #### identifier le TDP (total) du processeur ####
  # sinon estimer la valeur à 15W par coeur
  # vérifier si le vecteur est non nul
  if(length(cpu_version)>0) {
    if(cpu_version %in% data$`Processor Number`) {
      cpu_TDP<-
        data$TDP[data$`Processor Number` == cpu_version]
      cat("La version du processeur est présente. Valeur du TDP :", cpu_TDP, "W\n")
    } else {
      cpu_TDP<-number_of_cores*15
      cat("La version du processeur n'est pas présente dans le dataframe.\n",
          "La valeur estimée du TDP est", cpu_TDP, "W\n")
    }
  } else {
    cpu_TDP<-number_of_cores*15
    cat("La version du processeur n'est pas présente dans le dataframe.\n",
        "La valeur estimée du TDP est", cpu_TDP, "W\n")
  }




} # fin
