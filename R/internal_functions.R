# Primarily internal functions for other functions in this package

## This script contains the following functions:
### distToFeatures()
### calclateIndices()
### loadGPKG()
### loadVector()
### loadRast()
### numericFun()
### factorFun()


#-------------------------------------------------------------------------------

# Calculate distance to nearest feature (added 2026-05-28) ####
##' @description Simplified calculation of distance to nearest feature while maintaining structure of nearest feature data
##'
##' @title (Internal) Distance to the nearest feature
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
  colnames(out) <- c("OID", colnames(feature)[!grepl("geom", colnames(feature))], paste(c("near", "dist"), "_", n, sep = ""))
  return(out)
  rm(near, dist, out)
  #rm(x, n, feature)
}


#-------------------------------------------------------------------------------

# Calculate number of rows in each month's worth of data to create a universal Object Identifier that is unique for each row ####
##' @description Calculate number of rows in each month to create a continuous OID across months
##'
##' @title (Internal) Create index across multiple files
##'
##' @param in.files. character vector of file paths to saved RDS files containing data.tables or data.frames.
##' @param starting. Numeric. What should the starting value of the index be? Defaults to 1
##'
##' @details This function is standalone but intended to be used within the \code{\link{flagAssignment}} as a way to create a unique OID that starts at the beginning of the function.
##'
##' @returns data.table with index number of the in.files, the number of rows in each file, the starting index number, and ending index number
##'
##' @references \code{\link{flagAssignment}} \code{\link{summarizeHMD}}
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom data.table data.table shift
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept indicing
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
calculateIndices <- function(in.files, by = NULL, starting = 1){
  #in.files <- files.raw
  #by <- c("grid", "day")

  f.rows <- data.table::data.table(index = 1:length(in.files), rows = sapply(in.files, function(x){
    x1 <- readRDS(x)
    if(is.null(by)){
      return(nrow(x1))
    }else{
      x2 <- x1[,.N, by = by]
      return(nrow(x2))
    }
  }))
  f.rows[,`:=` (OID.start = NA,
                OID.end = cumsum(rows))]
  f.rows[,`:=` (OID.start = data.table::shift(OID.end, n = 1, fill = 0, type = "lag") + starting)]
  f.rows
  return(f.rows)
  rm(in.files, f.rows)
  #rm(in.dir)
}


#-------------------------------------------------------------------------------

# Load tiled data from geopackages (added 2026-09-17) ####
##' @description Load in data from geopackages and subset to tiles
##'
##' @title (Internal) load tiled geopackages
##'
##' @param l list of geopackage file paths
##' @param n name of the geopackage to use. Defines names of columns for this data
##'
##' @details This function is intended to be used within the \code{\link{classifyHMD}} function. It is not currently set up to run standalone
##'
##' @returns A spatial data.frame of vector data within tiles
##'
##' @references \code{\link{classifyHMD}}
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom sf st_read st_transform
##'
##' @keywords data
##'
##' @concept hmd
##' @concept vector data
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
loadGPKG <- function(l, n){
  message("Loading and projecting ", length(tiles_sel), " tiles from ", n)
  if(n == "bldg"){
    x <- l[[grep("building", names(l), ignore.case = TRUE)]]
  }else{
    x <- l[[grep(n, names(l), ignore.case = TRUE)]]
  }
  x1 <- do.call(rbind, lapply(tiles_sel, function(y){
    tryCatch(sf::st_read(x, layer = y, quiet = TRUE), error = function(e){message(y, " failed. Skipping."); return(NULL)})
  }))
  x2 <- sf::st_transform(x1, crs = coord.sys)
  colnames(x2)[-ncol(x2)] <- paste(colnames(x2)[-ncol(x2)], "_", n, sep = "")
  #colnames(x2)[ncol(x2)] <- "geometry"
  #sf::st_geometry(x2) <- "geometry"
  return(x2)
  rm(n, x1, x2)
  #rm(x)
}


#-------------------------------------------------------------------------------

# Load in vector data (added 2026-09-17) ####
##' @description Load in vector data from a geopackage based on a short name of a layer
##'
##' @title (Internal) Load in vector data
##'
##' @param x character. Name of the layer. Need not be a full name but must be unique.
##' @param gpkg character. Name of the geopackage containing the data
##'
##' @details This function is intended to be used within the \code{\link{classifyHMD}} function. This function is standalone
##'
##' @returns A spatial data frame within the study area
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom terra vect crop
##' @importFrom sf st_layers st_transform st_as_sf
##'
##' @keywords data
##'
##' @concept hmd
##' @concept vector data
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
loadVector <- function(x, gpkg){
  lyrs <- sf::st_layers(gpkg)
  x1 <- terra::vect(x = gpkg, layer = lyrs$name[grep(x, lyrs$name)])
  x2 <- terra::crop(x1, bbox_vect_wgs84)
  x3 <- sf::st_transform(sf::st_as_sf(x2), crs = coord.sys)
  colnames(x3)[-ncol(x3)] <- paste(colnames(x3)[-ncol(x3)], "_", x, sep = "")
  return(x3)
  rm(x1, x2, x3)
  #rm(x, gpkg)
}


#-------------------------------------------------------------------------------

# Load in raster data (added 2026-09-17) ####
##' @description Load in raster data within a study area
##'
##' @title (Internal) Load tiled raster data
##'
##' @param x Directory containing rasters
##' @param name Name to use for the raster layer
##'
##' @details This function is intended to be used within the \code{\link{classifyHMD}} function. This function is standalone
##'
##' @returns A raster within the study area
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom fs path_ext_remove dir_ls
##' @importFrom terra sprc mosaic project
##'
##' @keywords data
##'
##' @concept hmd
##' @concept vector data
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
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


#-------------------------------------------------------------------------------

# Summarize numeric data (Added 2026-09-18) ####
##' @description Summarize numeric data by specified columns
##'
##' @title (Internal) Summarize numeric data
##'
##' @param f character. Which type of numeric summary should be created.
##' This should be one of c("N", "mn", "min", "q01", "q05", "q25", "med", "q75", "q95" "q99", "max", "sd", "first" "last", "cum_dist", "cnt").
##' @param ds data.table. The data.table that should be summarized.
##' @param cols character. Which columns should be summarized.
##' @param by.cols character. Which columns should be used for grouping data.
##'
##' @details This function is intended to be used within the \code{\link{summarizeHMD}} function. It likely works standalone but is untested.
##'
##' @returns A data.table containing the by.cols and summarized cols
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom data.table setnames
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept summary
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
numSummary <- function(f, ds, cols, by.cols = sort.cols){
  #f <- c("N", "mn", "med", "sd", "first", "last")
  out <- switch(f,
                N = ds[,.N, by = by.cols],
                mn = ds[,lapply(.SD, mean, na.rm = TRUE), by = by.cols, .SDcols = cols],
                min = ds[,lapply(.SD, min, na.rm = TRUE), by = by.cols, .SDcols = cols],
                q01 = ds[,lapply(.SD, quantile, probs = 0.01, na.rm = TRUE), by = by.cols, .SDcols = cols],
                q05 = ds[,lapply(.SD, quantile, probs = 0.05, na.rm = TRUE), by = by.cols, .SDcols = cols],
                q25 = ds[,lapply(.SD, quantile, probs = 0.25, na.rm = TRUE), by = by.cols, .SDcols = cols],
                med = ds[,lapply(.SD, median), by = by.cols, .SDcols = cols],
                q75 = ds[,lapply(.SD, quantile, probs = 0.75, na.rm = TRUE), by = by.cols, .SDcols = cols],
                q95 = ds[,lapply(.SD, quantile, probs = 0.95, na.rm = TRUE), by = by.cols, .SDcols = cols],
                q99 = ds[,lapply(.SD, quantile, probs = 0.99, na.rm = TRUE), by = by.cols, .SDcols = cols],
                max = ds[,lapply(.SD, max, na.rm = TRUE), by = by.cols, .SDcols = cols],
                sd = ds[,lapply(.SD, sd, na.rm = TRUE), by = by.cols, .SDcols = cols],
                first = ds[,lapply(.SD, head, n = 1), by = by.cols, .SDcols = cols],
                last = ds[,lapply(.SD, tail, n = 1), by = by.cols, .SDcols = cols],
                cum_dist = ds[,lapply(.SD, sum, na.rm = TRUE), by = by.cols, .SDcols = cols],
                cnt = ds[,lapply(.SD, sum), by = by.cols, .SDcols = cols]
  )
  if(f == "N"){
    data.table::setnames(out, "N", "n_other")
  }else if(f == "cum_dist"){
    data.table::setnames(out, cols, "cum_dist")
  }else if(f == "cnt"){
    NULL
  }else{
    data.table::setnames(out, cols, paste(cols, "_", f, sep = ""))
  }
  return(out)
}


#-------------------------------------------------------------------------------

# Summarize factor data (Added 2026-09-18) ####
##' @description Summarize factor data by specified columns
##'
##' @title (Internal) Summarize factor data
##'
##' @param f character. Column name of the factor column that should be summarized.
##' @param ds data.table. The data.table that should be summarized.
##' @param by.cols character. Which columns should be used for grouping data.
##'
##' @details This function is intended to be used within the \code{\link{summarizeHMD}} function.
##' It likely works standalone but is untested.
##'
##' @returns A data.table containing the by.cols and columns named after the factor of choice
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom data.table setnames dcast
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept summary
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
factSummary <- function(f, ds, by.cols = sort.cols){
  t1 <- ds[,.N, by = c(by.cols, f)]
  colnames(t1)[colnames(t1) %in% f] <- "f"
  cols.old <- as.character(unique(t1$f))
  if(any(is.na(cols.old))){
    cols.old[is.na(cols.old)] <- "NA"
  }
  cols.new <- paste(f, "_", cols.old, sep = "")
  t2 <- data.table::dcast(t1, ... ~ f, value.var = "N", fill = 0)
  data.table::setnames(t2, old = cols.old, new = cols.new)

  t3 <- merge(ds[,.N, by = by.cols], t2)
  t4 <- t3[,lapply(.SD, function(x){x/N}), by = by.cols, .SDcols = cols.new]
  data.table::setnames(t4, old = cols.new, new = paste(cols.new, "_prop", sep = ""))
  t5 <- merge(t2, t4)
  return(t5)
  rm(t1, t2, t3, t4, t5, cols.old, cols.new)
}


#-------------------------------------------------------------------------------

# Identify bursts in space and time (Added 2026-11-05) ####
##' @description Identify bursts based on spatial and temporal information
##'
##' @title (Internal) Identify bursts
##'
##' @param geom character. Column name identifying the geometry column
##' @param time character. Column name identifying the date-time column
##' @param size.val numeric or units. Distance threshold to identify points as being close to each other.
##' Inherits from \code{\link{distanceThin}}.
##' @param rate.val numeric or period. Time threshold to identify points as being close to each other.
##' Inherits from \code{\link{distanceThin}}.
##'
##' @details This function is intended to be used within the \code{\link{distanceThin}} function.
##' It likely does not work as a standalone function, unless used as a do operation in a data.table.
##'
##' @returns an integer vector of burst identities.
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom sf st_distance
##' @importFrom igraph make_empty_graph add_edges components
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept track resampling
##' @concept rarifaction
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
burstFun2 <- function(geom, time, size.val = size, rate.val = rate) {
  # Get length of bursts
  n <- length(time)
  if (n == 1L) return(1L)

  # Sort times so we can find each point's temporal window
  t <- as.numeric(time)
  ord <- order(t)
  ts <- t[ord]

  # For each point, find later points within the time threshold
  end <- findInterval(ts + rate.val, ts)
  candidates <- lapply(seq_len(n - 1L), function(k) {
    if(end[k] > k){seq.int(k + 1L, end[k])}else{integer()}
  })

  counts <- lengths(candidates)
  if (sum(counts) == 0L) return(seq_len(n))

  i <- rep.int(seq_len(n - 1L), counts)
  j <- unlist(candidates, use.names = FALSE)

  # Map candidate pairs back to the original row order
  i <- ord[i]
  j <- ord[j]

  # Calculate only candidate-pair distances, not a full distance matrix
  d <- sf::st_distance(geom[i], geom[j], by_element = TRUE)
  keep <- as.numeric(d) <= size.val

  if (!any(keep)) return(seq_len(n))

  # Determine relationship between pairs
  graph <- igraph::make_empty_graph(n = n, directed = FALSE)
  graph <- igraph::add_edges(graph, as.vector(rbind(i[keep], j[keep])))

  # Output group assignment
  out <- igraph::components(graph)$membership

  # Return value and cleanup
  return(out)
  rm(n, t, ord, ts, end, candidates, counts, i, j, d, keep, graph, out)
  #rm(geom, time, size.val, rate.val)
}


#-------------------------------------------------------------------------------

# Select a single row from a burst (Added 2026-11-05) ####
##' @description Select a single row from a burst.
##'
##' @title (Internal) Select a row from a burst
##'
##' @param oid. character. Column name identifying the OID.POINT column
##' @param mthd. method to use for selection. See \code{\link{distanceThin}} for details on method options.
##'
##' @details This function is intended to be used within the \code{\link{distanceThin}} function.
##' It likely does not work as a standalone function, unless used as a do operation in a data.table.
##'
##' @returns a logical vector of selected points.
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept track resampling
##' @concept rarifaction
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
selFun2 <- function(oid, mthd = method, seed = seed){
  # Get length of the OIDs
  n <- length(oid)
  # Select a single OID from the burst based on multiple criteria
  # Create vector of logicals
  sel <- rep(FALSE, times = n)
  # Select a single point based on selection method
  set.seed <- seed
  sel[switch(mthd,
             random = ifelse(n == 1, 1, sample(seq_len(n), size = 1)),
             first = 1,
             last = n,
             median = floor(median(seq_len(n))))] <- TRUE
  # Return output
  return(sel)
  # Clean up
  rm(n, sel)
  #rm(oid)
}


#-------------------------------------------------------------------------------


