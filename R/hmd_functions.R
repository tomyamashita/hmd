# Functions for use with human mobility data

## This script contains the following functions:
### flagAssignment()
### flagRemoval()
### classifyHMD()
### distToFeatures()
### trackFun()
### flaggedToRaw()
### splitByMonth()
### thinHMD()


################################################################################

# Flag assignment function (added 2026-05-15) ####
##' @description Convert integer objects in Raw HMD data to readable numeric and character vectors and assign and name flags.
##'
##' @title Assign flags to HMD
##'
##' @param in.dir character. Directory containing the raw HMD data
##' @param out.dir character. Directory where flagged data should be saved
##' @param move.file logical. Should completed files be moved to a sub-directory in the the in.dir. This called "flagged" by default.
##' @param pp  logical. Should the function run using parallel processing?
##' @param cores.left numeric. How many cores should be reserved? Ignored when pp = FALSE
##'
##' @details This function takes raw HMD and assigns flags and converts an integer date into a readable POSIXct date.
##' The local time is based on the timezone field in the raw data.
##'
##' @return A list of file paths leading to the newly created flagged data.
##'
##' @note There currently is no function in this package that creates the raw data...
##'
##' @section {Warning}:
##' Working with HMD can be very memory intensive.
##' When using parallel processing, be careful how many cores you use.
##' The function will give a warning message if you use more than 5 cores.
##'
##' @section {Disclaimer}:
##' The functions in this package are in active development and likely has bugs.
##' Additionally, the functions in this package likely will not work properly if this package was not used to create the data.
##'
##' @seealso \code{\link{flagRemoval}}, \code{\link{classifyHMD}}
##'
##' @importFrom fs dir_exists dir_ls path_ext_remove dir_create file_move
##' @importFrom parallel detectCores makeCluster clusterExport stopCluster
##' @importFrom pbapply pblapply
##' @importFrom data.table as.data.table
##' @importFrom lubridate with_tz
##' @importFrom bit64 as.double.integer64 is.integer64 as.bitstring
##'
##' @keywords manip
##' @keywords files
##'
##' @concept HMD
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
flagAssignment <- function(in.dir, out.dir, move.file = TRUE, pp = FALSE, cores.left = NULL){
  #in.dir <-file.path(getwd(), "data_1_raw")       # Directory containing raw data
  #out.dir <- file.path(getwd(), "data_2_flagged") # Directory where flagged data should be saved
  #move.file <- TRUE
  #pp <- TRUE                                      # Should parallel processing be enabled?
  #cores.left <- 20                                 # How many processing cores should be reserved for additional use. Suggest leaving at least 1.
  # Initial file path checks
  if(!fs::dir_exists(in.dir)){
    stop("Your in directory does not exist. The inputted file path was ", in.dir)
  }
  if(!fs::dir_exists(out.dir)){
    stop("Your out directory does not exist. The inputted file path was ", out.dir)
  }

  # Load in the raw files
  files.raw <- fs::dir_ls(in.dir, type = "file")

  # Check that there are raw files in the folder
  if(length(files.raw) == 0){
    stop("There are no files in the in.dir. Is this step complete?")
  }

  if(pp == TRUE){
    message("Parallel Processing Enabled")
    if(is.null(cores.left)){
      cores.left <- 20
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    if(parallel::detectCores() - cores.left > 5){
      message("Using more than 5 cores is likely to overuse the computer's RAM.")
    }
    cl1 <- parallel::makeCluster(parallel::detectCores() - cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("in.dir", "files.raw", "out.dir", "move.file"), envir = environment())
  }else{
    cl1 <- NULL
  }

  # Run flag assignment on each file individually
  out1 <- pbapply::pblapply(files.raw, cl = cl1, function(x){
    # For testing purposes, untext this field
    #x <- files.raw[1]

    # The file name (without the extension) for the raw data
    name <- fs::path_ext_remove(basename(x))

    # Load in the raw data
    message(paste("\nStarting flag assignment for ", name, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sep = ""))
    fs1 <- data.table::as.data.table(readRDS(x))
    fs1$OID <- 1:nrow(fs1)

    # Set the time in UTC from an integer64 object
    fs1$timestamp_numeric <- bit64::as.double.integer64(fs1$timestamp) * 1000
    fs1$timestamp_POSIXct.UTC <- lubridate::as_datetime(fs1$timestamp_numeric/1000000, origin = "1970-01-01", tz = "UTC")

    # Convert UTC time to local time
    fs1[, `:=` (timestamp_POSIXct.local = lubridate::with_tz(timestamp_POSIXct.UTC, tzone = unique(timezone))), by = timezone]

    # Now identify the flags
    ## First, determine the bitstring for the forensicflag column
    if(bit64::is.integer64(fs1$forensicflag)){
      # If the forensicflag column is a integer64 object, do this:
      ## NEED TO TEST TO MODIFY TO data.table METHODS
      bits <- bit64::as.bitstring.integer64(fs1$forensicflag)
      ## Identify which bits have flags
      fs1$flag <- pbapply::pblapply(bits, function(x){
        x1 <- unlist(strsplit(x, split = ""))
        x1 <- rev(x1)
        x2 <- which(x1 == 1)
        x3 <- formatC(x2, width = 2, flag = "0")
        paste(x3, collapse = " ")
        #rm(x, x1, x2, x3)
      })
    }else if(is.integer(fs1$forensicflag)){
      # If the forensicflag column is a regular integer, do this:
      fs1 <- fs1[, `:=` (flag = paste(formatC(which(as.numeric(intToBits(forensicflag)) == 1), width = 2, flag = "0"), collapse = " ")), by = OID]
    }else{
      stop("The forensicflag column should be an integer64 or integer data type")
    }

    # Assign the flag names to the flags
    message("Flags identified for ", name, ". Assigning flag values...")
    ## Flags use a 0-based index and R uses an index base-1 so need to add 1 to flag to align with documentation
    fs1[,`:=` (flag_name = gsub(" ", ", ", flag))]
    ## MISSING FLAGS 1-7 ##
    fs1[,flag_name := sub("08", "LAT_GRID_LOCATION", flag_name)]
    fs1[,flag_name := sub("09", "TOO_MANY_DEVICES_AT_LOCATION", flag_name)]
    fs1[,flag_name := sub("10", "SPOOF_LOCATION", flag_name)]
    fs1[,flag_name := sub("11", "RADIO_DERIVED", flag_name)]
    ## MISSING FLAG 12 ##
    fs1[,flag_name := sub("13", "EU", flag_name)]
    fs1[,flag_name := sub("14", "OVER_CAPACITY_DEVICE", flag_name)]
    fs1[,flag_name := sub("15", "LIKELY_DRIVING", flag_name)]
    fs1[,flag_name := sub("16", "HIGH_ACCURACY", flag_name)]     # 0 - 35 m accuracy
    fs1[,flag_name := sub("17", "MODERATE_ACCURACY", flag_name)] # 50 - 220 m accuracy
    fs1[,flag_name := sub("18", "LOW_ACCURACY", flag_name)]      # 250 - 1000 m accuracy
    ## Moderate-High accuracy = both High and moderate accuracy flags = 35 - 50 m accuracy
    ## Moderate-Low accuracy = both Moderate and Low accuracy flags = 220 - 250 m accuracy
    ## MISSING FLAG 19 ##
    fs1[,flag_name := sub("20", "MOBILE_NETWORK", flag_name)]
    fs1[,flag_name := sub("21", "HYPERV_OUTSIDE_CLUSTER", flag_name)]
    fs1[,flag_name := sub("22", "HYPERV_WITHIN_CLUSTER", flag_name)]
    fs1[,flag_name := sub("23", "HYPERV_CLUSTER_INTERLEAVE", flag_name)]
    fs1[,flag_name := sub("24", "TIMESHIFT", flag_name)]
    ## MISSING FLAG 25 ##
    fs1[,flag_name := sub("26", "US", flag_name)]
    ## MISSING FLAG 27 ##
    fs1[,flag_name := sub("28", "IMPLAUSIBLE_MOVEMENT or BI_LOCATION", flag_name)]
    fs1[,flag_name := sub("29", "APPROXIMATED_SIGNAL", flag_name)]
    fs1[,flag_name := sub("30", "REPLAY", flag_name)]
    ## MISSING FLAGS 31-32 ##
    ## Missing flags are company-internal flags and have not been published

    # Save the output to the output directory
    out.path <- file.path(out.dir, paste(name, "_flagged_", format(Sys.Date(), "%Y%m%d"), ".RDS", sep = ""))
    saveRDS(fs1, file = out.path)

    # Remove the flagged file to a new folder inside the directory called "flagged"
    if(move.file == TRUE){
      if(!fs::dir_exists(file.path(dirname(x), "flagged"))){
        message("Creating the flagged folder inside ", in.dir)
        fs::dir_create(file.path(dirname(x), "flagged"))
      }
      fs::file_move(x, file.path(dirname(x), "flagged", basename(x)))
    }

    message(paste("\nCompleted flag assignment for ", name, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sep = ""))
    return(out.path)
  })

  # Stop parallel processing
  if(pp == TRUE){
    parallel::stopCluster(cl1)
  }

  # Close the function
  return(out1)
  rm(files.raw, out1)
  #rm(in.dir, out.dir)
}


################################################################################

# Flag removal function (added 2026-05-15) ####
##' @description Remove locations containing bad data, locations with inaccurate fixes, or unreasonable movements
##'
##' @title Remove data from flagged HMD
##'
##' @param in.dir character. Directory containing the flagged HMD data
##' @param out.dir character. Directory where cleaned data should be saved
##' @param FF.remove NULL or character. Names of flags that should be removed. Default is NULL.
##' When NULL, the function will remove flags identified by the provider as bad flags (SPOOF_LOCATIONS, REPLAY).
##' When character, the function will remove the selected flags in addition to the default.
##' @param FF.suspect NULL or character. Names of flags that should be included as suspect. Default is NULL.
##' When NULL, the function will assign the following flags to suspect: OVER_CAPACITY_DEVICE, TOO_MANY_DEVICES_AT_LOCATION, HYPERV_OUTSIDE_CLUSTER, HYPERV_CLUSTER_INTERLEAVE, HYPERV_WITHIN_CLUSTER, and IMPLAUSIBLE_MOVEMENT or BI_LOCATION.
##' @param method character. What cleaning method should be used on the data? Many options exist. The default is 'none'. See details for other options.
##' @param all.keep logical. Should we keep all the data regardless of cleaning method? If a cleaning method is set to 'all' or an invalid cleaning method is selected, then this is overridden to TRUE
##' @param move.files logical. Should completed files be moved to a sub-directory in the the in.dir. This called "cleaned" by default.
##'
##' @inheritParams flagAssignment pp cores.left
##'
##' @details Accuracy is calculated based on company-provided values.
##' Accuracy is assigned a number from 0-5 where
##' 0 = Unknown (no accuracy information from flags),
##' 1 = Low accuracy (low accuracy flag (250 - 1000 m)),
##' 2 = Medium-low accuracy (both low and medium accuracy flags (220 - 250 m)),
##' 3 = Medium accuracy (medium accuracy flag (50 - 220 m)),
##' 4 = Medium-high accuracy (both medium and high accuracy flags (35 - 50 m)),
##' and 5 = High accuracy (high accuracy flag (0 - 35 m)).
##'
##' Methods for cleaning data: Many options are currently implemented for cleaning data depending on how conservative you need the data to be:
##' none: Only bad flags and duplicates will be removed.
##' all: Keeps all cleaning methods. Should only be used to compare the results of the various cleaning methods.
##' BaseHigh: Heather's original cleaning method which only removed high speeds (>190 km/h ~ > 120 mph) from suspect flags
##' BaseMed: A modification of the BaseHigh method removing lower speed locations (> 130 km/h ~ 80 mph) from suspect flags
##' HighSpeeds: Removes high speeds regardless of suspect flags
##' MedSpeeds: Removes medium speeds regardless of suspect flags
##' Accurate: Removes locations with unknown, low, or medium-low accuracy
##' SinglePoints: Removes locations where only one location in a day was detected
##' AccHigh: Removes locations that satisfies both the HighSpeeds and Accurate options
##' AccMed: Removes locations that satisfies both the MedSpeeds and Accurate options
##' HighSingle: Removes locations that satisfies both the HighSpeeds and SinglePoints options
##' MedSingle: Removes locations that satisfies both the MedSpeeds and SinglePoints options
##' AccHighSingle: Removes locations that satisfies the HighSpeeds, Accurate, and SinglePoints options
##' AccMedSingle: Removes locations that satisfies the MedSpeeds, Accurate, and SinglePoints options
##'
##' @return A list of file paths leading to the newly created cleaned data.
##'
##' @note There currently is no function in this package that creates the raw data...
##'
##' @inheritSection flagAssignment {Warning}
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @seealso \code{\link{flagAssignment}}
##'
##' @importFrom fs dir_ls dir_exists dir_create file_move
##' @importFrom parallel detectCores makeCluster clusterExport stopCluster
##' @importFrom pbapply pblapply
##' @importFrom lubridate as_date
##' @importFrom data.table as.data.table shift
##'
##' @keywords manip
##' @keywords files
##'
##' @concept HMD
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
flagRemoval <- function(in.dir, out.dir, FF.remove = NULL, FF.suspect = NULL, method = "none", all.keep = FALSE, move.files = TRUE, pp = FALSE, cores.left = NULL){
  # For testing purposes, un-text these lines:
  #in.dir <- file.path(getwd(), "test_data", "flagged") # The input directory. Location where flagged data is saved
  #out.dir <- file.path(getwd(), "test_data", "cleaned")  # The output directory. Location where cleaned data should be saved
  #FF.remove <- NULL                              # Should any additional flags be removed during initial cleaning?
  #FF.remove <- c("OVER_CAPACITY_DEVICE")
  #FF.suspect <- NULL                             # Should any additional flags be added to the suspect flags
  #FF.suspect <- c("LOW_ACCURACY")
  #method <- "none"                         # For details of methods, see below. Use "HighSpeeds" for removing all high speeds, regardless of suspect flags
  #all.keep <- FALSE                              # Should all the data be kept? Useful for checking and confirming proper functioning. Even if set to true, this still removes flags that company says to remove
  #move.files <- TRUE                             # Should the completed file get moved to a subfolder?
  #pp <- TRUE                                     # Should parallel processing be enabled?
  #cores.left <- 20                                # How many processing cores should be reserved for additional use. Suggest leaving at least 1.

  # FUNCTION START #
  message("This function started at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  # Initial file path checks
  if(!fs::dir_exists(in.dir)){
    stop("Your in directory does not exist. The inputted file path was ", in.dir)
  }
  if(!fs::dir_exists(out.dir)){
    stop("Your out directory does not exist. The inputted file path was ", out.dir)
  }

  # Load in the flagged files
  files.flagged <- fs::dir_ls(in.dir, type = "file")

  # Check that there are raw files in the folder
  if(length(files.flagged) == 0){
    stop("There are no files in the in.dir. Is this step complete?")
  }

  # Define flags of interest
  ## All known flags
  FF.all <- c("LAT_GRID_LOCATION",
              "TOO_MANY_DEVICES_AT_LOCATION",
              "SPOOF_LOCATION",
              "RADIO_DERIVED",
              "EU",
              "OVER_CAPACITY_DEVICE",
              "LIKELY_DRIVING",
              "HIGH_ACCURACY",
              "MODERATE_ACCURACY",
              "LOW_ACCURACY",
              "MOBILE_NETWORK",
              "HYPERV_OUTSIDE_CLUSTER",
              "HYPERV_WITHIN_CLUSTER",
              "HYPERV_CLUSTER_INTERLEAVE",
              "TIMESHIFT",
              "US",
              "IMPLAUSIBLE_MOVEMENT or BI_LOCATION",
              "APPROXIMATED_SIGNAL",
              "REPLAY")

  ## Check that added flags are real
  if(any(!(FF.suspect %in% FF.all))){
    stop("One of your flags in FF.suspect is not a real flag name. Try again...")
  }
  if(any(!(FF.remove %in% FF.all))){
    stop("One of your flags in FF.remove is not a real flag name. Try again...")
  }

  ## These are flags defined by the company (GRAVY) that should be removed
  if(is.null(FF.remove)){
    message("No additional flags being used to remove rows. Using only SPOOF_LOCATION and REPLAY flags.")
    FF.remove <- c("SPOOF_LOCATION", "REPLAY")
  }else{
    message("The ", paste(FF.remove, collapse = ", "), " flags are being added to SPOOF_LOCATION and REPLAY for removal.")
    FF.remove <- c("SPOOF_LOCATION", "REPLAY", FF.remove)
    FF.remove <- FF.remove[!duplicated(FF.remove)]
  }

  ## These are flags that may be associated with problematic points
  if(is.null(FF.suspect)){
    message("No additional flags being used to identify suspect rows. Using OVER_CAPACITY_DEVICE, TOO_MANY_DEVICES_AT_LOCATION, HYPERV_OUTSIDE_CLUSTER, HYPERV_CLUSTER_INTERLEAVE, HYPERV_WITHIN_CLUSTER, and IMPLAUSIBLE_MOVEMENT or BI_LOCATION flags.")
    FF.suspect <- c('OVER_CAPACITY_DEVICE','HYPERV_OUTSIDE_CLUSTER','HYPERV_CLUSTER_INTERLEAVE',
                    'IMPLAUSIBLE_MOVEMENT or BI_LOCATION','HYPERV_WITHIN_CLUSTER','TOO_MANY_DEVICES_AT_LOCATION')
  }else{
    message("The ", paste(FF.suspect, collapse = ", "), " flags are being added to OVER_CAPACITY_DEVICE, TOO_MANY_DEVICES_AT_LOCATION, HYPERV_OUTSIDE_CLUSTER, HYPERV_CLUSTER_INTERLEAVE, HYPERV_WITHIN_CLUSTER, and IMPLAUSIBLE_MOVEMENT or BI_LOCATION flags to identify suspect rows.")
    FF.suspect <- c('OVER_CAPACITY_DEVICE','HYPERV_OUTSIDE_CLUSTER','HYPERV_CLUSTER_INTERLEAVE',
                    'IMPLAUSIBLE_MOVEMENT or BI_LOCATION','HYPERV_WITHIN_CLUSTER','TOO_MANY_DEVICES_AT_LOCATION',
                    FF.suspect)
    FF.suspect <- FF.suspect[!duplicated(FF.suspect)]
    if(any(FF.suspect %in% FF.remove)){
      message("Dropping flags from suspect that are being removed.")
      FF.suspect <- FF.suspect[!(FF.suspect %in% FF.remove)]
    }
  }

  # Set up parallel processing
  if(pp == TRUE){
    message("Parallel Processing Enabled")
    if(is.null(cores.left)){
      cores.left <- 5
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    if(parallel::detectCores() - cores.left > 5){
      message("Using more than 5 cores is not recommended.")
    }
    cl1 <- parallel::makeCluster(parallel::detectCores() - cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("in.dir", "files.flagged", "FF.all", "FF.suspect", "FF.remove", "out.dir", "method", "all.keep", "move.files"), envir = environment())
  }else{
    cl1 <- NULL
  }

  # Remove data based of flags and cleaning method
  out2 <- pbapply::pblapply(1:length(files.flagged), cl = cl1, function(i){
    # For testing purposes
    #i <- 1

    # FUNCTION START #
    x <- files.flagged[i]
    name <- basename(x)

    message("\nStarted vetting for ", name, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ". Loading data...")

    # Load the flagged file
    f1 <- readRDS(x)
    #f1[1:5,]
    #class(f1)

    # Identify which rows have which flags
    f.flags <- data.table::as.data.table(sapply(FF.all, grepl, f1$flag_name))

    # Identify duplicated files
    dup <- duplicated(f1, by = c("grid", "latitude", "longitude", "timestamp_POSIXct.UTC", "flag"))

    ## Define what the accuracy level of each point
    f.acc <- f.flags[,c("HIGH_ACCURACY", "MODERATE_ACCURACY", "LOW_ACCURACY")]
    f.acc[,ACCURACY := (ACCURACY = ifelse(HIGH_ACCURACY == TRUE,
                                          ifelse(MODERATE_ACCURACY == TRUE, 4, 5),
                                          ifelse(MODERATE_ACCURACY == TRUE,
                                                 ifelse(LOW_ACCURACY == TRUE, 2, 3),
                                                 ifelse(LOW_ACCURACY == TRUE, 1, 0))))]
    #f.acc[1:5,]
    f1[,`:=` (ACCURACY = f.acc$ACCURACY)]
    #f1[1:5,]

    ## Determine which rows have potentially problematic flags
    f.suspect <- f.flags[,..FF.suspect]
    f1[,`:=` (suspect = apply(f.suspect, 1, any))]
    #f1[1:5,]

    ## Identify rows to remove
    f.remove <- cbind(f.flags[,..FF.remove], duplicate = dup)
    f.remove[,`:=` (remove = apply(.SD, 1, any)), .SDcols = c(FF.remove, "duplicate")]
    #f.remove[1:5,]

    message(round(sum(f.remove$duplicate)/nrow(f.remove) * 100, digits = 2), "% of rows in ", name, " were duplicates and will be removed. \n",
            round(sum(sum(f.remove$remove)-sum(f.remove$duplicate))/nrow(f.remove) * 100, digits = 2), "% of additional rows had flags that needed to be removed.")

    ## Remove rows
    f2 <- f1[!f.remove$remove,]

    # Sort data by unique user (grid) and timestamp
    message("Loaded data and removed duplicates and bad flags from ", name, ". Sorting and calculating movement parameters...")
    f3 <- f2[order(order(grid,timestamp_POSIXct.UTC)),]

    # Day in local time
    f3[,c("day") := lubridate::as_date(timestamp_POSIXct.local)]

    # Grid days
    f3[,c("grid_day") := paste(grid, day, sep = "_")]

    cols.shift1 <- c("grid", "timestamp_POSIXct.UTC", "latitude", "longitude")
    cols.shift2 <- paste("temp_", cols.shift1, sep = "")
    f3[,(cols.shift2) := data.table::shift(.SD, n = -1, type = "lag"), .SDcols = cols.shift1]

    # Calculate time between fixes
    f3[,("difft_sec") := ifelse(grid == temp_grid, difftime(temp_timestamp_POSIXct.UTC, timestamp_POSIXct.UTC), NA)]

    # Calculate step length
    f3[,("stepl_m") := ifelse(grid == temp_grid, geosphere::distHaversine(p1 = f3[,c("longitude", "latitude")], p2 = f3[,c("temp_longitude", "temp_latitude")]), NA)]

    # Calculate speed
    ## In km per hour
    f3[,("speed_kmh") := stepl_m/difft_sec * (60*60)/1000]
    ## In miles per hour
    f3[,("speed_mph") := speed_kmh * 0.6214]

    # Calculate direction
    f3[,("bearing") := ifelse(grid == temp_grid, geosphere::bearing(f3[,c("temp_longitude", "temp_latitude")], f3[,c("longitude", "latitude")]), NA)]
    #f3[1:5,]

    # Calculate number of points per day
    #f4 <- merge(f3, f3[,.(PointsPerDay = .N, av.bearing = mean(bearing, na.rm = TRUE), sd.bearing = sd(bearing, na.rm = TRUE)), by = .(grid, day)])
    f4 <- merge(f3, f3[,.(PointsPerDay = .N), by = .(grid, day)])
    f4[,(cols.shift2) := NULL]
    f4[1:5,]

    # Clean the data
    ## Single cleaning methods
    f5 <- f4[,.(BaseCleanHigh = !(suspect & (speed_kmh > 190 | is.infinite(speed_kmh) | is.na(speed_kmh))),
                BaseCleanMed = !(suspect & (speed_kmh > 130 | is.infinite(speed_kmh) | is.na(speed_kmh))),
                HighSpeeds = !(speed_kmh > 190 | is.infinite(speed_kmh) | is.na(speed_kmh)),
                MedSpeeds = !(speed_kmh > 130 | is.infinite(speed_kmh) | is.na(speed_kmh)),
                SinglePoints = PointsPerDay > 1,
                Accurate = ACCURACY > 3)]

    ## Combined cleaning methods
    f5[,c("AccHigh",
          "AccMed",
          "AccSingle",
          "HighSingle",
          "MedSingle",
          "AccHighSingle",
          "AccMedSingle") := list(AccHigh = Accurate & HighSpeeds,
                                  AccMed = Accurate & MedSpeeds,
                                  AccSingle = Accurate & SinglePoints,
                                  HighSingle = HighSpeeds & SinglePoints,
                                  MedSingle = MedSpeeds & SinglePoints,
                                  AccHighSingle = Accurate & HighSpeeds & SinglePoints,
                                  AccMedSingle = Accurate & MedSpeeds & SinglePoints)]
    #f5[1:5,]

    ## Select your cleaning method
    if(method == "all"){
      message("Method set to all. all.keep is overridden to TRUE")
      all.keep <- TRUE
    }else if(method == "none"){
      message("Method set to none. Only bad flags and duplicates are used for cleaning data. ")
    }else if(!(method %in% colnames(f5))){
      message("Method was inproperly specified. Providing all methods instead. \nThe options for cleaning methods are: \nc('all', 'BaseHigh', 'BaseMed', 'HighSpeeds', 'MedSpeeds', 'Accurate', 'SinglePoints', 'AccHigh', 'AccMed', 'HighSingle', 'MedSingle', 'AccHighSingle', 'AccMedSingle')")
      all.keep <- TRUE
    }

    # Remove rows and temporary columns
    message("Calculations complete for ", name, ". Removing locations and saving output...")
    if(all.keep == TRUE){
      f6 <- cbind(f4, f5)
    }else{
      if(method == "none"){
        f6 <- f4
      }else{
        cleaner <- as.vector(f5[,..method])[[1]]
        f6 <- f4[cleaner,]
      }
    }

    # Clean up and save the output
    ## Create a new name for save output
    new.name <- sub("flagged.*", paste("clean_", format(Sys.Date(), "%Y%m%d"), ".RDS", sep = ""), name)

    ## Save the output
    saveRDS(f6, file = file.path(out.dir, new.name))

    ## Move completed files to a subfolder
    if(move.files == TRUE){
      if(!fs::dir_exists(file.path(dirname(x), "cleaned"))){
        message("Creating the cleaned folder inside ", in.dir)
        fs::dir_create(file.path(dirname(x), "cleaned"))
      }
      fs::file_move(x, file.path(dirname(x), "cleaned", basename(x)))
    }else{
      message("Files not moved.")
    }

    # Finish up and close loop
    if(all.keep){
      message("Finished vetting ", name, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              "\nLost approximately ", round((nrow(f1) - nrow(f6))/nrow(f1)*100, 1), "% of the original data.",
              "\nNo cleaning method defined. All remaining data returned.")
    }else{
      message("Finished vetting ", name, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              "\nLost approximately ", round((nrow(f1) - nrow(f6))/nrow(f1)*100, 1), "% of the original data.",
              "\nThe cleaning method removed an additional ", round((nrow(f4) - nrow(f6))/nrow(f4)*100, digits = 1), "% of the data after removing duplicates and bad flags.")
    }

    return(file.path(out.dir, new.name))
    rm(x, name, new.name, f1, f2, f3, f4, f5, f6, f.acc, f.flags, f.remove, f.suspect, dup, cols.shift1, cols.shift2, cleaner)
    #rm(i)
  })

  # Stop parallel processing
  if(pp == TRUE){
    parallel::stopCluster(cl1)
  }

  # Close function
  return(out2)
  rm(files.flagged, FF.all, FF.suspect, FF.remove, out2, cl1)
  #rm(in.dir, out.dir, method, move.files, all.keep, pp, cores.left)
}


################################################################################

# Calculate Spatial HMD metrics (Added 2026-05-15) ####
##' @description Calculate spatial and track-level metrics from HMD data
##'
##' @title Classify HMD based on distance to features
##'
##' @param in.dir character. Directory containing cleaned HMD
##' @param out.dir character. Directory where classified HMD should be stored
##' @param studyarea Study area shapefile. This can be a character vector of a file path to a shapefile, "HMD" (Uses the area of the HMD to define a study area), or an sf or terra object.
##' @param coord.sys character. Projected coordinate system in epsg format.
##' If a geographic coordinate system is used (e.g., WGS84 or NAD83), this will default to epsg:3857.
##' @param data.dir character. Directory where the geopackage and raster layers are stored.
##' See details for important information about the structure of this directory
##' @param ths list. Distance thresholds for main roads (main), railroads (rail), buildings (bldg), local roads (local), and trails (trail).
##' If not all names are provided, this function will exclude those from determining hmd.type.
##' @param road.source character. The source data to use for local roads and trail data.
##' This should be any of ("NTD", "USFS").
##' If both are provided, then the function will use the nearest of the two to determine distance to local roads/trails.
##' @param snowfraction numeric between 0 and 100. What percentage of snow cover in a cell is required to assign a point as having snow.
##' @param add.calcs logical. Should the additional, non-distance calculations be included? See details for which calculations are included
##' @inheritParams flagAssignment pp cores.left
##'
##' @details This function uses a previously downloaded data directory containing spatial data and calculates distance to features and possibly additional spatial data associated with HMD.
##' Additional data currently being calculated is: the land manager, protected area status, distance to cell towers, elevation and terrain metrics (TRI, slope, aspect), snow cover, whether the point is in water, and whether the point is in an urban area.
##'
##' @return A list of file paths leading to the newly created classified data.
##'
##' @note While parallel processing is possible, this is not recommended for this function.
##' Even for moderately sized HMD files, distance calculations are very memory intensive and can easily over-use the system memory.
##'
##' @inheritSection flagAssignment {Warning}
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @seealso \code{\link{flagAssignment}}, \code{\link{flagRemoval}} for functions in HMD processing pipeline.
##' This function uses the \code{\link{trackFun}} and \code{\link{distToFeature}} functions.
##'
##' @importFrom data.table data.table as.data.table rbindlist mergelist
##' @importFrom fs dir_ls dir_exists path_ext_remove dir_create dir_delete
##' @importFrom lubridate as_date
##' @importFrom matrixStats rowMins
##' @importFrom parallel detectCores makeCluster clusterExport stopCluster
##' @importFrom pbapply pblapply
##' @importFrom sf st_crs st_as_sf st_as_sfc st_bbox st_read st_transform st_buffer st_intersects st_geometry st_zm st_layers st_nearest_feature st_distance st_drop_geometry st_filter st_coordinates
##' @importFrom terra vect crop sprc mosaic project terrain extract
##' @importFrom suncalc getSunlightTimes getSunlightPosition
##' @importFrom tidyr separate
##' @importFrom units as_units drop_units
##'
##' @keywords manip
##'
##' @concept HMD
##' @concept Classification
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
classifyHMD <- function(in.dir, out.dir, studyarea, coord.sys, data.dir, ths, road.source, snowfraction = 50, add.calcs = TRUE, pp = FALSE, cores.left = NULL){
  #in.dir <- file.path(getwd(), "test_data", "cleaned")               # Directory containing cleaned HMD data
  #out.dir <- file.path(getwd(), "test_data", "classify")            # Directory containing classified HMD data and where files will be saved
  #studyarea <- file.path("E:","HMD", "Wolverines", "RecClass_test", "data_spatial", "Flathead_NF.shp")     # Define a study area for filtering points and road features. This can be a file path, sf object, or SpatVector object. Will be converted to a sf object
  #coord.sys <- "epsg:32612"                                               # Projected coordinate system for calculations. If this is not a projected coordinate system (e.g., epsg:4326 [WGS84] or epsg:4269 [NAD83]), the function will override this using epsg:3857 (World mercator).
  #data.dir <- file.path("E:","HMD", "Wolverines", "RecClass_test", "input_layers", "ready")                # Directory containing the geopackages and folders for vector and raster data
  #ths <- list(bldg = 5, Rmain = 30, Rlocal = 50, rail = 30, trail = 50)  # What thresholds should be used to define HMD.types. This should be a list containing up to bldg, Rmain, Rlocal, Tmotor, Tnon
  #snowfraction <- 50                                                     # What percent coverage of snow will be used to define snow in a cell
  #road.source <- c("NTD", "USFS")                                        # Should we use NTD roads/trails or USFS roads/trails. Both is acceptable
  #add.calcs <- TRUE                                                      # Should we include non-essential calculations like land type, protected area status, etc.? Will probably be removed once function is fully tested
  #pp <- FALSE                                                            # Should we use parallel processing for this
  #cores.left <- NULL                                                     # If so, how many cores should we reserve


  ##############################################################################

  # Check that necessary files exist
  ## geopackage files in the directory
  gpkg_files <- fs::dir_ls(data.dir, type = "file", glob = "*.gpkg$", recurse = FALSE)
  ## raster folders in the directory
  rast_files <- fs::dir_ls(data.dir, type = "directory", recurse = FALSE)
  ## general geopackage
  if(!any(grepl("general", gpkg_files))){
    stop("The general geopackage does not exist in the data directory. This is required.")
  }else{
    gen_gpkg <- gpkg_files[grep("general", gpkg_files)]
  }
  ## Main roads
  if(!any(grepl("NTD_mainroads", gpkg_files))){
    stop("The main roads geopackage does not exist in the data directory. This is required.")
  }else{
    main_gpkg <- gpkg_files[grep("NTD_mainroads", gpkg_files)]
  }
  ## NTD local roads
  if(!any(grepl("NTD_localroads", gpkg_files))){
    stop("The NTD local roads geopackage does not exist in the data directory. This is required.")
  }else{
    ntd_local_gpkg <- gpkg_files[grep("NTD_localroads", gpkg_files)]
  }
  ## NTD trails
  if(!any(grepl("NTD_trails", gpkg_files))){
    stop("The NTD trails geopackage does not exist in the data directory. This is required.")
  }else{
    ntd_trail_gpkg <- gpkg_files[grep("NTD_trails", gpkg_files)]
  }
  ## NTD railroads
  if(!any(grepl("NTD_railroads", gpkg_files))){
    stop("The NTD railroads geopackage does not exist in the data directory. This is required.")
  }else{
    rail_gpkg <- gpkg_files[grep("NTD_railroads", gpkg_files)]
  }
  ## USFS local roads
  if(!any(grepl("USFS_localroads", gpkg_files))){
    stop("The USFS local roads geopackage does not exist in the data directory. This is required.")
  }else{
    usfs_local_gpkg <- gpkg_files[grep("USFS_localroads", gpkg_files)]
  }
  ## USFS trails
  if(!any(grepl("USFS_trails", gpkg_files))){
    stop("The USFS trails geopackage does not exist in the data directory. This is required.")
  }else{
    usfs_trails_gpkg <- gpkg_files[grep("USFS_trails", gpkg_files)]
  }
  ## Microsoft buildings
  if(!any(grepl("MS_buildings", gpkg_files))){
    stop("The buildings geopackage does not exist in the data directory. This is required.")
  }else{
    bldg_gpkg <- gpkg_files[grep("MS_buildings", gpkg_files)]
  }
  ## Waterbodies
  if(!any(grepl("USGS_waterbodies", gpkg_files))){
    stop("The waterbodies geopackage does not exist in the data directory. This is required.")
  }else{
    water_gpkg <- gpkg_files[grep("USGS_waterbodies", gpkg_files)]
  }
  ## DEM elevation data
  if(!any(grepl("USGS_DEM", rast_files))){
    stop("The DEM folder does not exist in the data directory. This is required.")
  }else{
    dem_dir <- rast_files[grep("USGS_DEM", rast_files)]
  }
  ## Snow cover data
  if(!any(grepl("NSIDC_SnowCover", rast_files))){
    stop("The Snow Cover folder does not exist in the data directory. This is required.")
  }else{
    snow_dir <- rast_files[grep("NSIDC_SnowCover", rast_files)]
  }

  # Check that HMD data directories exist
  if(!all(c(fs::dir_exists(in.dir), fs::dir_exists(out.dir)))){
    stop("At least one of the HMD directories do not exist.")
  }else{
    # Load in HMD files
    ## Cleaned HMD data
    hmd.clean.files <- fs::dir_ls(path = in.dir, type = "file", glob = "*_clean*")
    message(length(hmd.clean.files), " file(s) in ", in.dir, " will be classified.")
    ## Already classified data
    hmd.class.files <- fs::dir_ls(path = out.dir, type = "file", glob = "*_classify*")

    # Get HMD dates
    hmd.clean.dates <- tidyr::separate(data.frame(path = hmd.clean.files, file = basename(hmd.clean.files)), col = file, into = c("SA", "Start", "End", "Status", "DateCompleted", "EXT"))
    hmd.dates.limits <- lubridate::as_date(c(min(hmd.clean.dates$Start), max(hmd.clean.dates$End)), format = "%Y%m%d")
    hmd.dates <- paste("D", format(seq.Date(hmd.dates.limits[1], hmd.dates.limits[2], by = "day"), "%Y%m%d"), sep = "")
  }

  # Check which distance calculations will be done
  ## Building calculation
  if(any(grepl("bldg", names(ths)))){
    bldg.th <- ths[["bldg"]]
  }else{
    message("No building threshold found. Will calculate distance to buildings but not include it in classification.")
    bldg.th <- NULL
  }
  ## Main roads calculation
  if(any(grepl("Rmain", names(ths)))){
    main.th <- ths[["Rmain"]]
  }else{
    message("No main road threshold found. Will calculate distance to main roads but not include it in classification.")
    main.th <- NULL
  }
  ## Local roads calculation
  if(any(grepl("Rlocal", names(ths)))){
    local.th <- ths[["Rlocal"]]
  }else{
    message("No local road threshold found. Will calculate distance to local roads but not include it in classification.")
    local.th <- NULL
  }
  ## Railroad calculation
  if(any(grepl("rail", names(ths)))){
    rail.th <- ths[["rail"]]
  }else{
    message("No railroad threshold found. Will calculate distance to railroads but not include it in classification.")
    rail.th <- NULL
  }
  ## Trails calculation
  if(any(grepl("trail", names(ths)))){
    trail.th <- ths[["trail"]]
  }else{
    message("No trail threshold found. Will calculate distance to trails but not include it in classification.")
    trail.th <- NULL
  }

  thresholds <- lapply(list(bldg = bldg.th, main = main.th, rail = rail.th, local = local.th, trail = trail.th), units::as_units, "meter")
  message("Function will use ", paste(names(thresholds)[!sapply(thresholds,is.null)], collapse = ", "), " to classify HMD.")

  # Check what the source of the road data should be
  if(all(sapply(c("NTD", "USFS"), function(x){any(grepl(x, road.source))}))){
    use.source <- "both"
  }else if(any(grepl("USFS", road.source))){
    use.source <- "usfs"
  }else if(any(grepl("NTD", road.source))){
    use.source <- "ntd"
  }else{
    stop("You defined an incorrect source for local road and trail data. This should be at one or both of c('USFS','NTD').")
  }

  # Check if provided coordinate system is a projected coordinate system
  ## Calculations of distance are best done in a projected CRS so forcing projection when necessary
  if(!grepl("PROJCRS", sf::st_crs(coord.sys)[[2]])){
    message("The selected coordinate system is a geographic coordinate system. Overriding to the world mercator (epsg:3857) projected CRS")
    coord.sys <- "epsg:3857"
  }

  # Check that snow fraction is a number between 0 and 100
  snowfraction <- as.numeric(snowfraction)
  if(snowfraction >= 0 & snowfraction <= 100){
    message("Snow fraction is set to ", snowfraction, "%")
  }else{
    stop("Snow fraction is either not a number or not between 0 and 100.")
  }

  message("Initial data checking complete. Loading and projecting spatial data...")


  ##############################################################################

  # Identify which tiles are included in the study area
  ## Make sure the study area is an sf object
  if(studyarea == "HMD"){
    message("Using the first file in the cleaned HMD folder to define the study area. \nThis can have unintended consequences if HMD comes from multiple areas...")
    temp1 <- readRDS(hmd.clean.files[1])
    temp2 <- sf::st_as_sf(temp1, coords = c("longitude", "latitude"), crs = "epsg:4326")
    sa <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(temp2)))
  }else if(class(studyarea)[1] == "character"){
    message("Reading study area from file...")
    sa <- sf::st_read(studyarea, quiet = TRUE)
  }else if(class(studyarea)[1] == "SpatVector"){
    message("Converting study area from terra vector to sf...")
    sa <- sf::st_as_sf(sa)
  }else if(class(studyarea)[1] == "sf"){
    message("Study area is already an sf object. No conversion needed")
    sa <- studyarea
  }else{
    stop("Could not read studyarea. Be sure that study area is either 'HMD', a string pointing to a file, a SpatVector object from the terra package or an sf object.")
  }
  ## Transform study area to projected crs
  sa_prj <- sf::st_transform(sa, crs = coord.sys)
  ## Create bounding box from study area
  bbox <- sf::st_as_sfc(sf::st_bbox(sa_prj))
  ## Create buffer around study area to ensure features on edges of area are incorporated into calculations
  bbox_buffer <- sf::st_buffer(bbox, dist = 1000)
  ## Convert bbox to terra::vect for easier calculations
  bbox_vect_wgs84 <- terra::vect(sf::st_transform(bbox_buffer, crs = "epsg:4326"))

  # Load in tiles index and project it to correct coordinate system
  ## Load in tiles index
  tiles <- sf::st_read(gen_gpkg, layer = "tiles_all_WGS84", quiet = TRUE)
  ## Project tiles index
  tiles_prj <- sf::st_transform(tiles, coord.sys)

  # Identify which tiles intersect the study area
  ## Do intersection between study area and tiles index
  tiles_bbox <- unique(do.call(c, sf::st_intersects(bbox_buffer, tiles_prj)))

  ## Subset the tiles that are included the bounding box
  tiles_sel <- tiles$tile[tiles_bbox]


  ##############################################################################

  # Load in data from geopackages and project to coord.sys
  ## Function for loading data
  loadGPKG <- function(x){
    message("Loading and projecting ", length(tiles_sel), " tiles from ", substitute(x))
    n <- sub("_gpkg", "", substitute(x))
    ## NEED TO ADD A TRYCATCH SO IT NOTIFIES AND SKIPS ERRORS FOR NOW
    x1 <- do.call(rbind, lapply(tiles_sel, function(y){
      tryCatch(sf::st_read(x, layer = y, quiet = TRUE),
               error = function(e){
                 message(y, " failed. Skipping.")
                 return(NULL)
               })
      sf::st_read(x, layer = y, quiet = TRUE)
    }))
    x2 <- sf::st_transform(x1, crs = coord.sys)
    colnames(x2)[-ncol(x2)] <- paste(colnames(x2)[-ncol(x2)], "_", n, sep = "")
    colnames(x2)[ncol(x2)] <- "geometry"
    sf::st_geometry(x2) <- "geometry"
    return(x2)
    rm(n, x1, x2)
    #rm(x)
  }
  ## Main roads
  main_prj <- loadGPKG(x = main_gpkg)
  ## NTD local roads
  ntd_local_prj <- loadGPKG(x = ntd_local_gpkg)
  ntd_local_prj <- sf::st_zm(ntd_local_prj)
  ## NTD trails
  ntd_trails_prj <- loadGPKG(x = ntd_trail_gpkg)
  ## USFS local roads
  usfs_local_prj <- loadGPKG(x = usfs_local_gpkg)
  ## USFS trails
  usfs_trails_prj <- loadGPKG(x = usfs_trails_gpkg)
  ## NTD railroads
  rail_prj <- loadGPKG(x = rail_gpkg)
  ## Buildings
  bldg_prj <- loadGPKG(x = bldg_gpkg)
  ## Water bodies
  water_prj <- loadGPKG(x = water_gpkg)


  ##############################################################################

  # Load data from general geopackage and crop to study area
  ## Quick function for calculations
  loadVector <- function(x, gpkg){
    lyrs <- sf::st_layers(gen_gpkg)
    x1 <- terra::vect(x = gpkg, layer = lyrs$name[grep(x, lyrs$name)])
    x2 <- terra::crop(x1, bbox_vect_wgs84)
    x3 <- sf::st_transform(sf::st_as_sf(x2), crs = coord.sys)
    colnames(x3)[-ncol(x3)] <- paste(colnames(x3)[-ncol(x3)], "_", x, sep = "")
    return(x3)
    rm(x1, x2, x3)
    #rm(x, gpkg)
  }
  ## Cell towers
  celltowers_prj <- loadVector(x = "CellTowers", gpkg = gen_gpkg)
  ## Land ownership
  fedlands_prj <- loadVector(x = "LandOwnership", gpkg = gen_gpkg)
  ## Land status
  pad_prj <- loadVector(x = "SpecialStatus", gpkg = gen_gpkg)
  ## Urban areas
  urban_prj <- loadVector(x = "UrbanAreas", gpkg = gen_gpkg)


  ##############################################################################

  # Raster data
  ## Function for raster data
  loadRast <- function(x, name){
    files1 <- fs::dir_ls(x, type = "file", glob = "*.tif$")

    rast1 <- files1[fs::path_ext_remove(basename(files1)) %in% tiles_sel]
    rast2 <- terra::sprc(rast1)
    rast3 <- terra::mosaic(rast2)
    rast_prj <- terra::project(rast3, sa_prj)
    names(rast_prj) <- name
    return(rast_prj)
    rm(files1, rast1, rast2, rast3, rast_prj)
  }
  ## Elevation
  dem_prj <- loadRast(x = dem_dir, name = "elevation")

  ## Calculate elevation-based metrics
  elev_prj <- c(dem_prj,
                terra::terrain(dem_prj, v = "TRI"),
                terra::terrain(dem_prj, v = "slope"),
                terra::terrain(dem_prj, v = "aspect"))
  names(elev_prj) <- c("elev_m", "tri", "slope", "aspect")

  ## Snow cover
  ### Locate the daily snow cover directories
  snow_dir2 <- fs::dir_ls(snow_dir, recurse = FALSE, type = "directory")
  ### Select only directories that overlap in time with HMD
  snow_days1 <- lapply(hmd.dates, grep, snow_dir2)
  ### Check that all HMD dates overlap with snow cover
  if(any(lengths(snow_days1) == 0)){
    message("Some dates of HMD do not have snow cover information. This will be skipped for now but may cause problems later...")
  }
  snow_days2 <- snow_dir2[do.call(c, snow_days1)]
  ### Create study area rasters of daily snow cover
  snow_prj <- pbapply::pblapply(snow_days2, loadRast, name = "snowcover")
  names(snow_prj) <- basename(snow_days2)

  message("Spatial data projected. Preparing HMD calculations...")

  ##############################################################################

  # Set up for parallel processing
  ## Set up parallel processing
  if(pp == TRUE){
    message("Parallel Processing Enabled")
    if(is.null(cores.left)){
      cores.left <- 10
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    if(parallel::detectCores() - cores.left > 5){
      message("Beware, using more than 5 cores is likely to overuse the computer's RAM...")
    }
    cl1 <- parallel::makeCluster(parallel::detectCores() - cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("hmd.clean.files", "hmd.class.files", "bbox_buffer",
                                             "main_prj", "ntd_local_prj", "ntd_trail_prj",
                                             "usfs_local_prj", "usfs_trail_prj",
                                             "rail_prj", "bldg_prj", "water_prj", "urban_prj",
                                             "celltowers_prj", "fedlands_prj", "landstatus_prj", "elev_prj", "snow_prj"),
                            envir = environment())
  }else{
    cl1 <- NULL
  }

  # Calculate HMD
  hmd1 <- pbapply::pblapply(1:length(hmd.clean.files), cl = cl1, FUN = function(h){
    # Basic loading
    ## Select 1 file
    h1 <- hmd.clean.files[h]
    ## Figure out its name
    n <- sub("_clean.*", "", basename(hmd.clean.files)[h])
    message("Starting ", n, " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ". This is ", h, " of ", length(hmd.clean.files))
    ## Identify where its going
    out.path <- file.path(out.dir, paste(n, "_classify_", format(Sys.Date(), "%Y%m%d"), ".RDS", sep = ""))

    # Check if output already exists
    ext <- fs::dir_ls(path = out.dir, type = "file", glob = paste("*", n, "_classify_", "*", sep = ""))

    # Do all the calculations
    if(length(ext) != 0){
      return(ext)
    }else{
      # Load in the cleaned HMD data
      message("Loading HMD and calculating time of day metrics...")
      h2 <- readRDS(h1)[,.(grid = grid,
                           day = day,
                           longitude = longitude,
                           latitude = latitude,
                           ts_UTC = timestamp_POSIXct.UTC)]
      #h2[1:5,]

      # Check for and remove duplicate time stamps for an individual
      dup <- duplicated(h2, by = c("grid", "day", "ts_UTC"))
      message("Removing ", round(sum(dup)/nrow(h2)*100, digits = 2), "% of locations due to duplicate timestamps.")
      h3 <- h2[!dup,]

      # Non-spatial pieces
      ## Recalculate number of points per day
      h4 <- merge(h3, h3[,.(PointsPerDay = .N), by = .(grid, day)])

      ## Calculate time of day and sun angle
      ### Modify data.table for suncalc package
      sun1a <- h3[,.(grid = grid,
                     ts = ts_UTC,
                     lon = longitude,
                     lat = latitude)]
      sun1a[,`:=` (date = lubridate::as_date(ts))]
      sun2a <- h3[,.(grid = grid,
                     date = ts_UTC,
                     lon = longitude,
                     lat = latitude)]
      ### Calculate sunrise/sunset times
      sun1b <- suncalc::getSunlightTimes(data = sun1a,
                                         keep = c("nauticalDawn", "sunrise", "sunset", "nauticalDusk"),
                                         tz = "UTC")
      ### Calculate time of day
      sun1a[,`:=` (tod = ifelse((ts >= nauticalDawn & ts < sunrise) | (ts > sunset & ts <= nauticalDusk), "crep",
                                ifelse(ts >=sunrise & ts <= sunset, "day", "night")))]
      sun1a
      ### Calculate sun angle
      sun2b <- suncalc::getSunlightPosition(data = sun2a, keep = c("altitude", "azimuth"))
      ### Add calculations to data.table
      h4[,`:=` (OID = 1:nrow(h4),
                tod = sun1a$tod,
                altitude = sun2b$altitude,
                azimuth = sun2b$azimuth)]
      h4[1:5,]

      message("Time of day calculated. Converting to spatial data.frame...")

      # Function for calculating distance metrics
      distToFeature <- function(x, n, feature){
        near <- sf::st_nearest_feature(x = x, y = feature)
        dist <- sf::st_distance(x = x, y = feature[near,], by_element = TRUE)
        out <- data.table::data.table(OID = x$OID, sf::st_drop_geometry(feature)[near,], near, units::set_units(dist, "meter"))
        colnames(out) <- c("OID", colnames(feature)[!grepl("geometry", colnames(feature))], paste(c("near", "dist"), "_", n, sep = ""))
        return(out)
      }

      # Convert to spatial file
      if(nrow(h4) > 500000){
        chunks <- data.frame(start = seq(1,nrow(h4), by = 500000), end = c(seq(1,nrow(h4), by = 500000)[-1]-1, nrow(h4)))
        message("HMD contains more than 500,000 rows. Chunking spatial processes into ", nrow(chunks), " chunks...")
      }else{
        chunks <- data.frame(start = 1, end = nrow(h4))
      }

      # Create temporary directory for chunk storage
      if(nrow(chunks) > 1){
        fs::dir_create("tempDir_Spatial")
      }

      # Calculate spatial metrics for each chunk
      h7.out <- pbapply::pblapply(1:nrow(chunks), function(i){
        # Pull out chunk
        h4.temp <- h4[chunks[i,1]:chunks[i,2],]

        # Spatialize chunk
        h5 <- sf::st_as_sf(h4.temp, coords = c("longitude", "latitude"), crs = "epsg:4326")

        # Project chunk
        h5_prj <- sf::st_transform(h5, crs = coord.sys)

        # Filter chunk to study area
        h6 <- sf::st_filter(h5_prj, bbox_buffer)
        if(nrow(h6) == 0){
          stop("There is no HMD data available in your study area. Try again")
        }

        # Create the output object
        h7 <- data.table::data.table(h4.temp[OID %in% h6$OID,], sf::st_coordinates(h6), crs = coord.sys)

        ## Distance to main roads
        message("Calculating distance metrics for Chunk ", i, " of ", nrow(chunks), " starting at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        #message("HMD spatialized and cropped to study area at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to main roads...")
        dist_main <- distToFeature(x = h6, n = "main", feature = main_prj)
        ## Distance to NTD local roads
        #message("Distance to main roads calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to ntd local roads...")
        dist_ntd_local <- distToFeature(x = h6, n = "ntd_local", feature = ntd_local_prj)
        ## Distance to NTD trails
        #message("Distance to ntd local roads calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to ntd trails...")
        dist_ntd_trail <- distToFeature(x = h6, n = "ntd_trail", feature = ntd_trails_prj)
        ## Distance to USFS local roads
        #message("Distance to ntd trails calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to usfs local roads...")
        dist_usfs_local <- distToFeature(x = h6, n = "usfs_local", feature = usfs_local_prj)
        ## Distance to USFS trails
        #message("Distance to usfs local roads calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to usfs trails...")
        dist_usfs_trail <- distToFeature(x = h6, n = "usfs_trail", feature = usfs_trails_prj)
        ## Distance to railroads
        #message("Distance to usfs trails calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to railroads...")
        dist_rail <- distToFeature(x = h6, n = "rail", feature = rail_prj)
        ## Distance to buildings
        #message("Distance to railroads calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nCalculating distance to buildings...")
        dist_bldg <- distToFeature(x = h6, n = "bldg", feature = bldg_prj)
        message("Distance metrics calculations for Chunk ", i, " of ", nrow(chunks), " completed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

        # Calculate HMD type
        ## Determine if location is close to each infrastructure
        hmd_ths <- do.call(cbind, lapply(1:length(thresholds), function(j){
          n <- names(thresholds)[j]
          th <- units::drop_units(thresholds[[j]])

          if(is.null(th)){
            in.th <- FALSE
          }else{
            if(n %in% c("local", "trail")){
              if(use.source == "both"){
                n.col <- paste("dist_", c("ntd", "usfs"), "_", n, sep = "")
                arg.col <- paste("dist_", n, sep = "")
              }else{
                n.col <- paste("dist_", use.source, "_", n, sep = "")
                arg.col <- n.col
              }
              DT1 <- switch(arg.col,
                            "dist_local" = data.table::data.table(dist_ntd_local[,.(dist_ntd_local)], dist_usfs_local[,.(dist_usfs_local)]),
                            "dist_trail" = data.table::data.table(dist_ntd_trail[,.(dist_ntd_trail)], dist_usfs_trail[,.(dist_usfs_trail)]))
            }else{
              n.col <- paste("dist_", n, sep = "")
              arg.col <- n.col
            }
            d1 <- switch(arg.col,
                         "dist_bldg" = units::drop_units(dist_bldg[,.(DIST = matrixStats::rowMins(as.matrix(.SD))), .SDcols = n.col]),
                         "dist_main" = units::drop_units(dist_main[,.(DIST = matrixStats::rowMins(as.matrix(.SD))), .SDcols = n.col]),
                         "dist_rail" = units::drop_units(dist_rail[,.(DIST = matrixStats::rowMins(as.matrix(.SD))), .SDcols = n.col]),
                         "dist_local" = units::drop_units(DT1[,.(DIST = matrixStats::rowMins(as.matrix(.SD))), .SDcols = n.col]),
                         "dist_trail" = units::drop_units(DT1[,.(DIST = matrixStats::rowMins(as.matrix(.SD))), .SDcols = n.col]))
            in.th <- d1[,lapply(.SD, function(x){x <= th}), .SDcols = "DIST"]
            colnames(in.th) <- n
          }
          return(in.th)
          rm(n, th, n.col, arg.col, in.th, DT1, d1)
          #rm(j)
        }))

        ## Categorize based on which infrastructure is close
        h7[,`:=` (hmd.type = apply(hmd_ths, 1, function(x){if(any(x == TRUE)){colnames(hmd_ths)[min(which(x == TRUE))]}else{"dispersed"}}))]
        #h7[1:5,]

        # Additional spatial calculations
        message("HMD type calculated. Calculating additional metrics...")
        if(add.calcs == TRUE){
          # Is the point in a water body
          ## Calculate whether HMD point intersects waterbody
          int.water <- sf::st_intersects(x = h6, y = water_prj)

          # In an urban area
          ## Calculate whether HMD point intersects an urban area
          int.urban <- sf::st_intersects(x = h6, y = urban_prj)

          # Calculate distance to nearest cell tower
          dist.cell <- distToFeature(x = h6, n = "cell", feature = celltowers_prj)

          # On federal lands
          ## Calculate whether HMD point intersects federal lands
          int.fed <- sf::st_intersects(x = h6, y = fedlands_prj)

          # Special Management area
          ## Calculate whether HMD point intersects protected areas
          int.pad <- sf::st_intersects(x = h6, y = pad_prj)

          # Elevation metrics
          ## Extract raster values for each HMD point
          elev1 <- data.table::as.data.table(terra::extract(elev_prj, h6))
          colnames(elev1)[1] <- "OID"
          elev1[1:5,]

          # Add new columns
          h7.adds <- data.table::data.table(OID = h7$OID,
                                            in.water = lengths(int.water) > 0,
                                            in.urban = length(int.urban) > 0,
                                            land.type = as.factor(sapply(int.fed, function(x){
                                              if(length(x) == 0){"Other (state or private)"}else if(length(x) > 1){"Multiple agencies"}else{paste(fedlands_prj$manager[x], collapse = ", ")}
                                              ## ADD WAY TO GROUP BY NPS, USFS, BLM, OTHER ##
                                            })),
                                            pad.type = as.factor(sapply(int.pad, function(x){
                                              if(length(x) == 0){"none"}else{paste(pad_prj$type[x], collapse = ", ")}
                                            })))
          h7.adds
          #h7 <- cbind(h7, dist.cell)

          # Snow cover metrics
          ## Split HMD by day
          ## Calculate snow cover on a given day
          days <- sort(unique(h6$day))
          h6.sc <- pbapply::pblapply(days, function(x){
            d <- paste("D", format(unique(x), "%Y%m%d"), sep = "")
            x1 <- h6[h6$day == x,]

            if(!any(names(snow_prj) %in% d)){
              message("There is no snow cover data available for ", sub("D", "", d), ". Assigning all snowcover values to NA...")
              x1$snow.cover <- NA
            }else{
              sc <- snow_prj[[d]]

              sc_values <- terra::extract(sc, x1)
              sc_values[1:5,]
              x1$snow.cover <- ifelse(sc_values$snowcover > snowfraction, "Y", "N")
            }
            x2 <- data.table::data.table(sf::st_drop_geometry(x1[,c("OID", "snow.cover")]))
            return(x2)
            rm(d, sc, sc_values)
            #rm(x)
          })
          ## Re-combine HMD data
          h7.sc <- data.table::rbindlist(h6.sc)

          # Add data to full dataset
          h7.adds[1:5,]
          h8 <- data.table::mergelist(l = list(h7,
                                               dist_bldg, dist_main, dist_rail,
                                               dist_ntd_local, dist_usfs_local,
                                               dist_ntd_trail, dist_usfs_trail,
                                               h7.adds, elev1, dist.cell, h7.sc),
                                      on = "OID")
        }else{
          h8 <- data.table::mergelist(l = list(h7,
                                               dist_bldg, dist_main, dist_rail,
                                               dist_ntd_local, dist_usfs_local,
                                               dist_ntd_trail, dist_usfs_trail),
                                      on = "OID")
        }
        #h8

        # Return different outputs based on number of chunks
        if(nrow(chunks) == 1){
          return(h8)
        }else{
          file.out <- file.path("tempDir_Spatial", paste("Chunk_", i, ".RDS", sep = ""))
          saveRDS(h8, file = file.out)
          return(file.out)
        }

        rm(h4.temp, h5, h5_prj, h6, h7, h8,
           dist_main, dist_ntd_local, dist_ntd_trail, dist_usfs_local, dist_usfs_trail, dist_rail, dist_bldg, hmd_ths,
           int.water, int.urban, dist.cell, int.fed, int.pad, elev1, h7.adds, days, h6.sc, h7.sc)
      })

      # Recombine data and load data back in
      if(nrow(chunks) == 1){
        h8 <- data.table::rbindlist(h7.out)
      }else{
        h8 <- data.table::rbindlist(lapply(h7.out, readRDS))
      }

      # Create track metrics (speed, turn angle, etc.)
      message("All spatial metrics calculated. Calculating track-level metrics...")
      h9 <- trackFun(ds = h8, xcol = "X", ycol = "Y", dtcol = "ts_UTC", idcol = "grid", thin = NULL)


      # Save the distance calculations
      message("Track metrics calculated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ". Cleaning up and saving output...")
      ## View output
      #class(h9)
      #h9[1:5,]

      ## Save output
      saveRDS(h9, out.path)

      ## Delete temporary directory
      if(nrow(chunks) > 1){
        fs::dir_delete("tempDir_Spatial")
      }

      # Close the function
      return(out.path)
    }
    rm(h1, n, out.path, ext, h2, dup, h3, h4, h8, h9, sun1a, sun2a, sun1b, sun2b, distToFeature)
    #rm(h)
  })

  # Stop parallel processing
  if(pp == TRUE){
    parallel::stopCluster(cl1)
  }

  # Convert the outputs to a string
  hmd2 <- do.call(c, hmd1)

  # Close function
  return(hmd2)
  rm(gpkg_files, rast_files, gen_gpkg, main_gpkg, ntd_local_gpkg, ntd_trail_gpkg, usfs_local_gpkg, usfs_trails_gpkg, rail_gpkg, bldg_gpkg, water_gpkg, dem_dir, snow_dir,
     main_prj, ntd_local_prj, ntd_trails_prj, usfs_local_prj, usfs_trails_prj, rail_prj, bldg_prj, water_prj, dem_prj, elev_prj, snow_prj,
     celltowers_prj, fedlands_prj, pad_prj, urban_prj, sa, sa_prj, bbox, bbox_buffer, bbox_vect_wgs84, tiles, tiles_prj, tiles_bbox, tiles_sel,
     bldg.th, local.th, main.th, rail.th, trail.th, hmd.class.files, hmd.clean.files, hmd.clean.dates, hmd.dates, hmd.dates.limits, thresholds,
     snow_days1, snow_days2, snow_dir2, use.source,
     loadGPKG, loadRast, loadVector)
  #rm(in.dir, out.dir, studyarea, coord.sys, data.dir, ths, road.source, add.calcs, snowfraction, pp, cores.left)
}


################################################################################

# Calculate track-level metrics for HMD ####
##' @description Calculate movement metrics (speed, step length, and time between points) for each track
##'
##' @title Movement metrics for HMD data
##'
##' @param ds data.table. Data.table of cleaned or classified HMD to add movement metrics to
##' @param xcol character. Column name of the column storing x coordinates
##' @param ycol character. Column name of the column storing y coordinates
##' @param dtcol character. Column name of the column storing date-time information
##' @param idcol character. Column name of the column storing a unique user id
##'
##' @details This function is modified from \code{\link{amt::make_track}} which utilizes \code{\link{amt::step_lengths}}, \code{\link{amt::direction_rel}}, and \code{\link{amt::speed}}.
##'
##' @return The original ds with movement metrics added. Adds speed_kmh, stepl_m, difft_sec, ta
##'
##' @references \code{\link{amt::amt}} \code{\link{amt::make_track}}
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @seealso \code{\link{classifyHMD}}
##'
##' @importFrom sf st_drop_geometry
##' @importFrom collapse roworder
##' @importFrom data.table data.table shift
##' @importFrom units set_units
##'
##' @keywords manip
##'
##' @concept HMD
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
trackFun <- function(ds, xcol = "X", ycol = "Y", dtcol = "ts_UTC", idcol = "grid"){
  #ds <- hmd.class3
  #xcol <- "X"
  #ycol <- "Y"
  #dtcol <- "ts_UTC"
  #idcol <- "grid"
  #thin <- 0

  ds$OID <- 1:nrow(ds)

  # Define column names
  cols <- c("OID", "X", "Y", "dt", "id")

  # Pull out only necessary columns
  if(class(ds)[1] == "sf"){
    ds2 <- sf::st_drop_geometry(ds[,c("OID", xcol, ycol, dtcol, idcol)])
  }else{
    ds2 <- ds[,c("OID", ..xcol, ..ycol, ..dtcol, ..idcol)]
  }

  # Rename columns
  colnames(ds2) <- cols
  #ds2[1:5,]


  ds3 <- collapse::roworder(ds2, id, dt)
  #ds3[1:5,]
  o <- ds3$OID

  # Convert to steps
  out1 <- lapply(cols, function(x){
    x1 <- ds3[,..x]
    x2 <- data.table::data.table(x1, data.table::shift(x1[[1]], n = -1, type = "lag"))
    if(x == "id"){
      x2$diff <- ifelse(x2[,1] == x2[,2],TRUE,FALSE)
      colnames(x2) <- paste(x, c("1","2","same"), sep = "__")
    }else{
      colnames(x2) <- paste(x, c("1","2"), sep = "__")
    }
    return(x2)
    rm(x1, x2)
    #rm(x)
  })
  out2 <- do.call(cbind, out1)
  out2[1:5,]

  # Calculate difference in X, Y, and dt
  out3 <- lapply(cols[2:4], function(x){
    x1 <- data.frame(ifelse(out2$id__same, out2[[paste(x, 2, sep = "__")]] - out2[[paste(x, 1, sep = "__")]], NA))
    colnames(x1) <- paste(x, "diff", sep = "__")
    return(x1)
    rm(x1)
    #rm(x)
  })
  out4 <- data.table::data.table(out2, do.call(cbind, out3))
  out4[1:5,]


  # Calculate step length and speed
  out4[,`:=` (stepl_m = ifelse(id__same, sqrt(X__diff^2 + Y__diff^2), NA))]
  out4[,`:=` (speed_kmh = ifelse(id__same, stepl_m/dt__diff * 3.6, NA))]

  # Calculate direction and turn angle
  a <- atan2(y = out4$Y__diff, x = out4$X__diff)
  a[out4$X__diff == 0 & out4$Y__diff == 0] <- NA
  a <- ifelse(a < 0, 2 * pi + a, a)
  #a[1:10]

  p <- c(NA, diff(a))

  out4[,`:=` (direction = a,
              ta = ifelse(id__same, p, NA))]
  #out4[1:5,]

  # Clean up output
  out6 <- out4[,c("OID__1", "id__same", "dt__diff", "stepl_m", "speed_kmh", "direction", "ta")]
  colnames(out6) <- c("OID", "same_gridday", "difft_sec", "stepl_m", "speed_kmh", "direction", "ta")
  out6[,`:=` (difft_sec = units::set_units(difft_sec, "sec"),
              stepl_m = units::set_units(stepl_m, "m"),
              speed_kmh = units::set_units(speed_kmh, "km/h"))]
  #out6[1:5,]

  # Merge with full data
  out7 <- cbind(ds[out6$OID,], out6[,-1])
  out7[1:5,]

  # tortuosity = (total distance) / (cumulative distance)
  # (total distance) = (euclidean distance between first and last points)
  # (cumulative distance) = sum(step lengths)
  # (summarize sampling rate) = data.frame(summary(difft_sec), sd = sd(difft_sec), n = length(difft_sec), unit = "sec")

  return(out7)
  rm(ds2, ds3, o, out1, out2, out3, out4, a, p, out6, out7)
  #rm(ds, xcol, ycol, dtcol, idcol, thin)
}


################################################################################

# Calculate distance to nearest feature (added 2026-05-28) ####
##' @description Simplified calculation of distance to nearest feature while maintaining structure of nearest feature data
##'
##' @title Distance to the nearest feature
##'
##' @param x sf object. Spatial data.frame of the input data
##' @param n String. Name that should be used for output files
##' @param feature sf object. Spatial data.frame of the nearest feature
##'
##' @details This is just a simple function to calculate distance to nearest feature and add features of the nearest feature to the output.
##'
##' @return a data.table object in the same order as X with the distance to nearest feature and features of the nearest feature.
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @seealso \code{\link{classifyHMD}}
##'
##' @importFrom sf st_nearest_feature st_distance st_drop_geometry
##' @importFrom data.table data.table
##' @importFrom units set_units
##'
##' @keywords methods
##'
##' @concept HMD
##' @concept Distance to Feature
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
distToFeature <- function(x, n, feature){
  near <- sf::st_nearest_feature(x = x, y = feature)
  dist <- sf::st_distance(x = x, y = feature[near,], by_element = TRUE)
  out <- data.table::data.table(OID = x$OID, sf::st_drop_geometry(feature)[near,], near, units::set_units(dist, "meter"))
  colnames(out) <- c("OID", colnames(feature)[!grepl("geometry", colnames(feature))], paste(c("near", "dist"), "_", n, sep = ""))
  return(out)
}


################################################################################

# Convert flagged data back to raw (added 2026-06-03) ####
##' @description Convert flagged data back to raw (a reverse of flagAssignment)
##'
##' @title Flagged data to raw data
##'
##' @inheritParams flagRemoval in.dir
##' @param out.dir character. The directory where the raw data should be saved
##'
##' @details This function takes flagged data and converts it back to raw (by removing columns created by the flagging process).
##' Beware, if using this function on cleaned data, data removed during the cleaning process will not be restored.
##'
##' @return Vector of file paths leading to the newly (re)created raw data
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @seealso \code{\link{flagAssignment}}, \code{\link{flagRemoval}}
##'
##' @importFrom fs dir_ls
##' @importFrom pbapply pblapply
##'
##' @keywords manip
##'
##' @concept HMD
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
flaggedToRaw <- function(in.dir, out.dir){
  in.files <- fs::dir_ls(path = in.dir, type = "file", recurse = FALSE)

  out1 <- pbapply::pblapply(in.files, function(x){
    x1 <- readRDS(x)
    x2 <- x1[,c("grid", "advertiserid", "latitude", "longitude", "timestamp", "timezone", "ipaddress", "forensicflag", "devicetype", "recordcount")]
    new.name <- sub("*_flagged.*", ".RDS", x)
    saveRDS(x2, file = file.path(out.dir, new.name))
    return(file.path(out.dir, new.name))
  })

  out2 <- do.call(c, out1)
  return(out2)
}


################################################################################

# Split a combined HMD file by month (added 2026-06-03) ####
##' @description Split a combined HMD file by month
##'
##' @title Split HMD by month
##'
##' @param in.dir character. The directory containing the file(s) to split up.
##' @param out.dir character. The directory where files should be saved.
##' @param time.col character. The column name of the time column used to determine months. Defaults to "timestamp_POSIXct.UTC" which is created by \code{\link{flagAssignment}}
##' @param name.prefix character. The name of this data that should be common among all newly created data
##' @param name.type character. A suffix for the data related to what stage of processing it is in. Usually one of (raw = "", flagged = "flagged", cleaned = "clean")
##'
##' @details This function can help to make HMD more manageable and easier to process through other functions in this package, especially \code{\link{classifyHMD}}.
##' The file name of the new data will be in the format:
##' [name.prefix]_[first of month in YYYYMMDD]_[last of month in YYYYMMDD]_[name.type (usually one of c("", "flagged", "clean"))]_[date created in YYYYMMDD format].RDS
##'
##' @return A list of file paths to the newly created data
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @seealso \code{\link{flagAssignment}}, \code{\link{flagRemoval}}, \code{\link{classifyHMD}}
##'
##' @importFrom fs dir_ls
##' @importFrom pbapply pblapply
##' @importFrom lubridate as_date floor_date ceiling_date
##'
##' @keywords manip
##'
##' @concept HMD
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
splitByMonth <- function(in.dir, out.dir, time.col = "timestamp_POSIXct.UTC", name.prefix, name.type){
  in.files <- fs::dir_ls(path = in.dir, type = "file", recurse = FALSE)

  out1 <- pbapply::pblapply(in.files, function(x){
    x1 <- readRDS(x)

    x1.dts <- unique(lubridate::as_date(x1[[time.col]]))
    months <- data.frame(start = unique(lubridate::floor_date(x1.dts, "1 month")), end = unique(lubridate::ceiling_date(x1.dts, "1 month"))-1)

    x2 <- lapply(1:nrow(months), function(i){
      y1 <- x1[x1[[time.col]] >= months[i,1] & x1[[time.col]] <= months[i,2],]
      out.name <- paste(name.prefix, "_", format(months[i,1], "%Y%m%d"), "_", format(months[i,2], "%Y%m%d"), "_", name.type, "_", format(Sys.Date(), "%Y%m%d"), ".RDS", sep = "")
      saveRDS(y1, file = file.path(out.dir, out.name))
      return(file.path(out.dir, out.name))
      rm(y1, out.name)
    })

    x3 <- do.call(c, x2)
    return(x3)
    rm(x1, x1.dts, months, x2, x3)
  })
  return(out1)
  rm(in.files, out1)
}


################################################################################

# Resample locations to thin fixes based on a time interval (added 2026-06-03) ####
##' @description Resample HMD locations to remove locations that are too close in time
##'
##' @title Resample HMD to a set sampling rate
##'
##' @param x data.table. Data.table containing the time information.
##' @param idcol character. Column name of the unique user id column
##' @param dtcol character. Column name of the date-time column
##' @param rate period or numeric. What is the expected sampling rate to include.
##' This can be either a period object from the lubridate package or a numeric in seconds.
##' @param tolerance period or numeric. The range tolerance for how close consecutive locations can be to exclude
##' This can be either a period object from the lubridate package or a numeric in seconds.
##' @param thin logical. Should rows be removed if they are too close together?
##'
##' @details This function is derived from and modified from the function amt::track_resample.
##' I adapted functions from amt to better accommodate running on multiple tracks.
##'
##' @returns One of:
##' When thin == FALSE: X with a new column added called burst. Bursts < 0 are locations that are sampled more frequently than is allowed by rate and tolerance
##' When thin == TRUE: X with extra locations removed.
##'
##' @references \code{\link{amt::amt}}
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom lubridate seconds period_to_seconds
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept track resampling
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
thinHMD <- function(x, idcol, dtcol, rate = lubridate::seconds(30), tolerance = lubridate::seconds(10), thin = TRUE){
  #x <- dt.selected
  #idcol <- "grid"
  #dtcol <- "ts_UTC"
  #rate <- lubridate::seconds(30)
  #tolerance <- lubridate::seconds(10)

  # Initial data checking
  if(class(rate) != "Period"){
    message("rate is not a period object. Assuming this is a numeric in seconds.")
  }else{
    rate <- lubridate::period_to_seconds(rate)
  }
  if(class(tolerance) != "Period"){
    message("tolerance is not a period object. Assuming this is a numeric in seconds.")
  }else{
    tolerance <- lubridate::period_to_seconds(tolerance)
  }

  # Core function for identifying periods. From amt package
  resFun <- function(time.col){
    t1 <- as.numeric(time.col)

    n <- length(t1)
    out <- numeric(n)
    k <- 1

    i <- 1

    while(i != n){
      t_min = t1[i] + rate - tolerance
      t_max = t1[i] + rate + tolerance
      j <- i + 1
      while((j < n) && (t1[j] < t_min)) {
        out[j] <- -1
        j <- j + 1
      }
      i <- j
      if ((j == n) && (t1[j] < t_min)) {
        out[j] = -1
      } else if (t1[j] >= t_min && t1[j] <= t_max) {
        out[j] = k
      } else {
        k <- k + 1
        out[j] = k
      }
    }
    return(out)

    #return(length(t1))
  }

  # Calculate bursts by period
  x[,("burst") := lapply(.SD, resFun), by = c(idcol), .SDcols = dtcol]

  if(isTRUE(thin)){
    x1 <- x[burst > 0,]
    x2 <- x1[,burst := NULL]
    return(x2)
  }else{
    return(x)
  }
  rm(resFun, x1, x2)
  #rm(x, idcol, dtcol, rate, tolerance, thin)
}


################################################################################


