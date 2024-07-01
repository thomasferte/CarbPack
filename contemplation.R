#### Contemplation des fonctions ####

#### fonction pour définir le hardware ####
# ici, il faudrait avoir des arguments :
# OS = NULL, à renseigner si différent de windows ou mac ? ex Linux
# TDP = NULL, à renseigner si on veut utiliser une valeur définie de TDP plutôt
# que celle du constructeur du processeur
# à remplir également si le processeur ne figure pas (?) dans la database 
# disponible
# tracker = TRUE, si T le tracker se lance après la récupération du hardware,
# si FALSE, le tracker ne se lance pas automatiquement avec cette fonction



#### fonction pour définir lancer le tracker ####
# aucun paramètre nécessaire, c'est juste un Sys.Time ?
# peut être ajouter un paramètre sur le %d'utilisation du processeur 
# par défaut = 1, compris entre 0 et 1 mais ça peut être complexe ?


#### fonction pour arrêter le tracker ####
# surement gérer les exceptions : erreur, le tracker n'est pas lancé par exemple
# il faudrait donc que le starttime soit supprimé après cette fonction ?
# un argument pour le pragmatic scale factor : si TRUE, utiliser le PSF et 
# rendre la valeur totale de consommation
# si FALSE, rendre la valeur uniquement de l'analyse actuelle
# à noter que FALSE est censé continuer de comptabiliser pour la valeur totale


