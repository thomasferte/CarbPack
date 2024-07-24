
#' @title
#' tracker_start
#'
#' @description
#' demarre le tracker, sauvegarde un objet temporel a la date du systeme.
#' A lancer avant l'execution du programme ou a son debut.
#'
#'
#' @return retourne un objet date
#' @export
#'
#' @examples #start_time<-tracker_start()
tracker_start<-function() {
  start_time<-Sys.time()
  return(start_time)
}
