## fonction test pour save en rds

#' Title
#'
#' @param object_name une chaîne de caractères qui est le nom du fichier rds
#' à importer et exporter
#'
#' @return un fichier rds stocke localement, test du compteur
#' @export
#'
#' @examples #non
increment_and_save <- function(object_name) {
  # Define the path to the RDS file where the object will be stored
  rds_file <- paste0(object_name, ".rds")

  # Check if the file exists
  if (file.exists(rds_file)) {
    # Load the existing object
    obj <- readRDS(rds_file)
    # Increment the object
    obj <- obj + 1
  } else {
    # Initialize the object
    obj <- 1
  }

  # Save the object back to the file
  saveRDS(obj, rds_file)

  # Return the updated object
  return(obj)
}
