
#' Title
#'
#' @param start_time un objet POSIXct, de preference l'objet cree par la
#' fonction 'tracker_start'
#'
#' @return des choses dans la console
#' @export
#'
#' @examples #tracker_stop<-function(start = tracker)
tracker_stop<-function(start_time) {

  # Verifier si l'argument est manquant
  if (missing(start_time)) {
    stop("L'argument 'date_arg' est requis et doit etre de type POSIXct de longueur 1.")
  }

  # Vérifier si l'argument est un objet POSIXct
  if (!inherits(start_time, "POSIXct")) {
    stop("L'argument 'start_time' doit etre un objet de type POSIXct.")
  }





}
