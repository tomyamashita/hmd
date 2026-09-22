# Othe functions

## This script contains the following functions:
### createBldgTiles()


#-------------------------------------------------------------------------------

# Create building tiles from Microsoft Building Detector ####
##' @description Download building tiles from Microsoft's building detector
##'
##' @title Download building tiles
##'
##' @param url character. URL where the csv file of the building tiles are stored.
##' This defaults to "https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv"
##' @param tiles sf object. An sf object of the polygons of the tiles used to split the building data.
##' @param run numeric or "all". Which tiles should be included in the tile creation? Defaults to 'all' which includes all tiles in "tiles". Useful if the function fails and needs to be restarted from where it was left off.
##' @param out_gpkg character. File path of the geopackage to save the output tiles.
##'
##' @details See https://github.com/microsoft/GlobalMLBuildingFootprints#will-there-be-more-data-coming-for-other-geographies for details on how the building detector works.
##' This function simply downloads already created data and separates it into the USGS tiles used for classification.
##' Currently, this function will redownload all the data and not just updates.
##' Microsoft's current system for defining updates do not state where updates come from so it is impossible to know what the most recent data is.
##' Running this function takes about 1 week for the Contiguous United States (CONUS)
##'
##' @return A list of file paths pointing to the output location of the tiles
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom quadkeyr quadkey_df_to_polygon
##' @importFrom sf st_intersects st_as_sf st_write
##' @importFrom pbapply pblapply
##' @importFrom terra vect is.valid makeValid
##' @importFrom geojsonsf geojson_sf
##' @importFrom readr read_lines
##'
##' @keywords datasets
##' @keywords manip
##'
##' @concept buildings
##' @concept data download
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
createBldgTiles <- function(url = "https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv", tiles, run = "all", out_gpkg = "MS_buildings_WGS84.gpkg"){
  start.time <- Sys.time()
  # Load list of building quadkeys
  db1 <- read.csv(url)

  # Select only US data
  db2 <- db1[db1$Location == "UnitedStates",]
  db2$quadkey <- formatC(db2$QuadKey, width = 9, flag = "0")

  # Convert quadkeys to polygons
  db3 <- quadkeyr::quadkey_df_to_polygon(data = db2)

  # Plot the overlap
  #ggplot() + geom_sf(data = tiles, color = "black", fill = NA) + geom_sf(data = db3, color = "blue", fill = NA) + theme_bw()

  # Identify which quadkeys overlap with each tile
  t2 <- sf::st_intersects(tiles, db3)

  if(run[1] == "all"){
    run <- 1:length(t2)
  }

  # Download buildings data and crop to each tile
  t3 <- pbapply::pblapply(run, function(i){
    # Pull out tile-specific information
    tile1 <- terra::vect(tiles[i,])
    tile.name <- tiles$tile[i]
    bldgs <- t2[[i]]

    # Download building data
    if(length(bldgs) == 0){
      message("No buildings in tile ", tile.name, ". Creating empty tile...")
      out2 <- terra::vect(geojsonsf::geojson_sf(readr::read_lines(db3$Url[1])))
      out2$LastUpdated <- db3$UploadDate[1]
    }else{
      out1 <- lapply(bldgs, function(j){
        x1 <- readr::read_lines(db3$Url[j])
        x2 <- geojsonsf::geojson_sf(x1)
        x2$LastUpdated <- db3$UploadDate[j]
        return(terra::vect(x2))
        rm(x1, x2)
      })
      out2 <- do.call(rbind, out1)
    }
    out2$tile <- tile.name
    out2[1:5,]

    # Validate building data
    test.valid <- terra::is.valid(out2)
    if(any(!test.valid)){
      valid.polys <- terra::makeValid(out2[!test.valid])
      valid.polys
      out3 <- rbind(out2[test.valid], valid.polys)
    }else{
      out3 <- out2
    }
    out3

    # Crop building data to tiles
    out4 <- terra::crop(out3, tile1)
    out4

    # Save building data to geopackage
    sf::st_write(sf::st_as_sf(out4), dsn = out_gpkg, layer = tile.name)

    # Close function
    message("Tile ", i, " finished at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    return(c(dsn = out_gpkg, n = tile.name))
    rm(tile1, tile.name, bldgs, out1, out2, out3, out4)
  })

  # Clean up and close function
  t4 <- do.call(rbind, t3)

  end.time <- Sys.time()
  message("This function took ", round(difftime(end.time, start.time, units = "hours"), digits = 2), " hours.")
  return(t4)
  rm(db1, db2, db3, t2, t3, t4)
}


#-------------------------------------------------------------------------------

# Create linked indices between points and summaries (Added 2026-09-22) ####
##' @description Combine object IDs from point data and track data based on the columns used for track-level summaries
##'
##' @title Combine Point and Track OIDs
##'
##' @param in.pt character. File paths to the point data
##' @param in.tk character. File paths to the track data. This should be the same length as in.pt.
##' @param by character. Character vector of column names used to summarize the track data.
##' @param out.type character. Should the output be a list of data.tables by file or a single combined file (dt)
##' @param verbose logical. Should the input tables be printed
##'
##' @details This function does not confirm that point and track files are paired other than by the merge.
##' This could produce hidden errors if point-level and track-level data are not in the same order
##'
##' @return either a list or data.table containing the by columns and the OID.POINT and OID.TRACK columns
##'
##' @inheritSection flagAssignment {Disclaimer}
##'
##' @importFrom data.table data.table rbindlist
##' @importFrom pbapply pblapply
##'
##' @keywords manip
##'
##' @concept hmd
##' @concept data organization
##'
##' @export
##'
##' @examples \dontrun{
##' ## No example right now
##' }
combineIndices <- function(in.pt, in.tk, by, out.type = "list", verbose = TRUE){
  #in.pt <- fs::dir_ls(file.path("E:", "HMD", "Classification_Rec", "data_HMD_v3", "classify"))
  #in.tk <- fs::dir_ls(file.path("E:", "HMD", "Classification_Rec", "data_HMD_v3", "summary_gridday"))
  #by <- c("grid", "day")

  if(length(in.pt) != length(in.tk)){
    stop("The number of files in the point data is different from the number of files in the summarized tracks.")
  }

  fs1 <- data.table::data.table(pts = in.pt, tks = in.tk)

  fs2 <- pbapply::pblapply(1:nrow(fs1), function(i){
    x1 <- readRDS(fs1$pts[i])[,.SD, .SDcols = c(by, "OID.POINT")]
    x2 <- readRDS(fs1$tks[i])[,.SD, .SDcols = c(by, "OID.track")]
    x3 <- merge(x1, x2, by = by)
    return(x3)
  })

  if(out.type == "dt"){
    fs3 <- data.table::rbindlist(fs2)
  }else if(out.type == "list"){
    fs3 <- fs2
  }else{
    message("You specified an invalid out.type. Choose one of c('dt', 'list').")
    fs3 <- fs2
  }

  if(verbose == TRUE){
    message("The files used to link OIDs are: ")
    print(fs1)
  }

  return(fs3)
  rm(fs1, fs2, fs3)
  #rm(in.pt, in.tk, by, out.type)
}


#-------------------------------------------------------------------------------


