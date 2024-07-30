# Estimation empreinte carbone des programmes R

## Work In Progress

Ce projet consiste en la création d'un package R permettant d'estimer l'empreinte carbone d'un programme qui tourne sous R.
L'idée est de lancer un tracker avant le programme, puis d'avoir une sortie après le programme qui estime l'empreinte carbone.

# Installation 

Cliquer sur "Code", "download source code", et choisir l'extension tar.gz.
Dans la console sur R, exécuter le code suivant en indiquant le chemin vers le fichier tar.gz téléchargé :
`install.packages("chemin/vers/CarbPack.tar.gz", repos = NULL, type = "source")`
`library(CarbPack)`
