#' @title
#' tracker_stop
#'
#' @description
#' Calculates the energy consumption and carbon footprint of the program from the moment the tracker was started. It can also accumulate these indicators depending on how many times the program has been run. To be called after program execution or at the end.
#'
#' @param hardware The list created previously by the 'detect_hardware' function.
#' @param start_time A POSIXct object, preferably the one created by the 'tracker_start' function.
#' @param carbon_intensity Electricity Generation Emissions Factor. Default is France October 2024 = 0.0705... (source: https://www.carbonfootprint.com/international_electricity_factors.html)
#' @param path Path to a .rds file, used to store and retrieve the total execution time. Leave empty to only produce a single iteration. Provide the file path to an existing or non-existing .rds file to accumulate execution time from previous and subsequent runs.
#'
#' @return Outputs a report to the console, and if the 'path' argument is provided, saves a .rds file at that path.
#' @export
#'
#' @examples
#' # tracker_stop(hardware = detect_hardware(),
#' # start_time = Sys.time(),
#' # path = "./path/to/file.rds")
tracker_stop <- function(hardware,
                         start_time,
                         carbon_intensity = 0.0705521359148881,
                         path = NULL) {

  # Check if the 'hardware' argument is missing
  if (missing(hardware)) {
    stop("The 'hardware' argument is required and should be the list created by 'detect_hardware'.")
  }

  # Check if the 'start_time' argument is missing
  if (missing(start_time)) {
    stop("The 'start_time' argument is required and should be a POSIXct object of length 1.")
  }

  # Check if the 'start_time' argument is a POSIXct object
  if (!inherits(start_time, "POSIXct")) {
    stop("The 'start_time' argument must be a POSIXct object.")
  }

  # Check the status of the 'path' argument
  if (!is.null(path)) {
    if (file.exists(path)) {
      loaded_time <- readRDS(path)
      # Ensure the loaded object is of class 'difftime'
      if (!inherits(loaded_time, "difftime")) {
        stop("The loaded object is not of class 'difftime'.")
      }
    } else { # If the path does not exist, create it and initialize the difftime object
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      loaded_time <- as.difftime(0, units = "secs")
      saveRDS(loaded_time, file = path)
      message("The path did not exist. A difftime object with a value of 0 has been saved to ", path)
    }
  }

  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time

  # Add the loaded difftime if it exists
  if (!is.null(path)) {
    elapsed_time <- elapsed_time + loaded_time
  }

  # Convert elapsed time to different units
  elapsed_seconds <- as.numeric(elapsed_time, units = "secs")
  elapsed_minutes <- as.numeric(elapsed_time, units = "mins")
  elapsed_hours <- as.numeric(elapsed_time, units = "hours")

  # Display elapsed time
  cat(sprintf("Elapsed time: %.2f seconds\n", elapsed_seconds))
  cat(sprintf("Elapsed time: %.2f minutes\n", elapsed_minutes))
  cat(sprintf("Elapsed time: %.2f hours\n", elapsed_hours))

  # Calculate energy consumption and carbon footprint

  # Energy consumption in Wh
  energy_needed <- elapsed_hours * (hardware$ram_gb * 0.3725 + hardware$cpu_TDP)

  # Carbon footprint
  carbon_footprint <- energy_needed * carbon_intensity

  # Print the metrics
  elapsed_hours_rounded <- round(elapsed_hours, 2)
  elapsed_minutes_rounded <- round(elapsed_minutes, 2)
  energy_needed_rounded <- round(energy_needed, 2)
  carbon_footprint_rounded <- round(carbon_footprint, 2)
  print(paste0("This program executed in ",
               elapsed_hours_rounded, " hours, which is ",
               elapsed_minutes_rounded, " minutes, ",
               "on ", hardware$number_of_cores, " CPUs ",
               hardware$cpu_data,
               " and required ",
               energy_needed_rounded, " Wh. In France, this corresponds to ",
               carbon_footprint_rounded, " g CO2e."))

  # Remove the start_time from the global environment
  suppr_temps <- as.character(substitute(start_time))
  rm(list = suppr_temps, envir = .GlobalEnv)

  # Save the difftime object if requested
  if (!is.null(path)) {
    saveRDS(object = elapsed_time, file = path)
    message("The total execution time has been saved to ", path)
  }
}
