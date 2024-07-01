#### pour calculer le temps d'exécution ####
# 

# Début du chronométrage
# start_time <- Sys.time()

## mettre le code à exécuter ici ##
# # Pause de 5 secondes
# cat("Début de la pause de 5 secondes...\n")
# Sys.sleep(5)
# cat("Fin de la pause de 5 secondes.\n")

# Calcul du temps écoulé
end_time <- Sys.time()
elapsed_time <- end_time - start_time

# Conversion du temps écoulé en différentes unités
elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
elapsed_minutes <- as.numeric(elapsed_time, units = "mins")
elapsed_hours <- as.numeric(elapsed_time, units = "hours")

# Affichage du temps écoulé
cat(sprintf("Temps écoulé : %.2f secondes\n", elapsed_seconds))
cat(sprintf("Temps écoulé : %.2f minutes\n", elapsed_minutes))
cat(sprintf("Temps écoulé : %.2f heures\n", elapsed_hours))

#### calcul de l'énergie consommée et de l'empreinte carbone

#### carbon intensity de la France ####
# valeurs juillet 2023
carbon_intensity<-0.06207

#### calcul de l'énergie consommée en Wh ####
energy_needed<-
  elapsed_hours*(ram_gb*0.3725+cpu_TDP)

#### calcul de l'empreinte carbone ####
carbon_footprint<-energy_needed*carbon_intensity

#### phrase à afficher avec les métriques ####
elapsed_hours_rounded<-round(elapsed_hours,digits = 2)
elapsed_minutes_rounded<-round(elapsed_minutes, digits = 2)
energy_needed_rounded<-round(energy_needed, digits = 2)
carbon_footprint_rounded<-round(carbon_footprint, digits = 2)
print(paste0("Ce programme s'est exécuté en ",
             elapsed_hours_rounded, " heures, soit ",
             elapsed_minutes_rounded, " minutes, ",
             "sur ", number_of_cores, " CPUs ",
             cpu_data_report, 
             " et a nécessité ",
             energy_needed_rounded, " Wh. En France, ceci correspond à",
             " une empreinte carbone de ",
             carbon_footprint_rounded, " g CO2e."))

#### phrase du total à afficher ####
# paramètre d'une fonction pourrait mettre en jeu cette partie
# le tracker doit tout de même être en mesure de conserver ces métriques
# même si on ne souhaite que la consommation de l'exécution actuelle
# ainsi on pourrait demander la consommation totale a posteriori si 
# l'environnement R est sauvegardé systématiquement

#### stockage de l'énergie TOTALE du programme ####
if(exists("energy_needed_total")) {
  energy_needed_total<-energy_needed_total+energy_needed
} else {
  energy_needed_total<-energy_needed
}

#### stockage carbone total ####
if(exists("carbon_footprint_total")) {
  carbon_footprint_total<-carbon_footprint_total+carbon_footprint
} else {
  carbon_footprint_total<-carbon_footprint
}

#### stockage du temps total d'exécution ####
if(exists("elapsed_hours_total")) {
  elapsed_hours_total<-elapsed_hours_total+elapsed_hours
} else {
  elapsed_hours_total<-elapsed_hours
}

#### 
#### formatage des résultats totaux ####
elapsed_hours_total_rounded<-round(elapsed_hours_total,digits = 2)
energy_needed_total_rounded<-round(energy_needed_total, digits = 2)
carbon_footprint_total_rounded<-round(carbon_footprint_total, digits = 2)



#### nettoyage ####
rm(start_time)
rm(end_time)
rm(elapsed_hours,elapsed_minutes,elapsed_seconds,
   elapsed_hours_rounded,elapsed_minutes_rounded,elapsed_time)
rm(energy_needed,energy_needed_rounded,carbon_footprint,carbon_footprint_rounded)




