## script test des fonctions de CarbPack

## hardware
a<-CarbPack::detect_hardware()

## débuter le tracker
b<-CarbPack::tracker_start()

## insérer le code principal ici

## résultats
CarbPack::tracker_stop(hardware = a,
                       start_time = b,
                       path = "./test.rds")

