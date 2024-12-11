#' @title
#' tracker_start
#'
#' @description
#' Starts the tracker, saving a timestamp at the current system date and time.
#' To be called before program execution or at the beginning.
#'
#' @return Returns a date-time object.
#' @export
#'
#' @examples
#' start_time <- tracker_start()
tracker_start <- function() {
  start_time <- Sys.time()
  return(start_time)
}
