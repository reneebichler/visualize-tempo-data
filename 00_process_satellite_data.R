## ------------------------------------------------------------------------------------
## Description
## ------------------------------------------------------------------------------------

## Author: Renee Bichler, 2025
## Find me on GitHub: reneebichler

## This code contains functions to process satellite observations.

cat("##################################################################################\n")
cat("##\n")
cat("##                              Load functions!\n")
cat("##                             ╰( ^o^)╮╰( ^o^)╮\n")
cat("##\n")
cat("##################################################################################\n")

## ------------------------------------------------------------------------------------
## Libraries
## ------------------------------------------------------------------------------------

packages <- c("terra", "raster", "sf", "dplyr")

## Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {install.packages(packages[!installed])}

## Load all packages
lapply(packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

#-----------------------------------------------------------------------------
# (1) Crop the satellite data to the size of the polygon file
#-----------------------------------------------------------------------------

crop_sat <- function(polygon, file_l) {

    aoi_full_df <- data.frame(NaN)
  
    for (sat_file in file_l) {

        print(paste0("Crop satellite file: ", sat_file))

        ## Load satellite and polygon file
        sat <- terra::rast(sat_file)

        ## Retrieve the filename and the date from the raster object
        filename <- strsplit(sources(sat), split = "/")[[1]][length(strsplit(sources(sat), split = "/")[[1]])]

        ## Retrieve the date from the filename and replace the "-" with "."
        date <- regmatches(filename, regexpr("\\d{4}-\\d{2}-\\d{2}", filename))
        date <- gsub("-", ".", date)

        ## Rename the raster object since this name will be used as column name in the df
        names(sat) <- paste0("X_", date)

        ## Set both objects to EPSG:4326
        sat <-  terra::project(sat, "EPSG:4326")
        polygon <- st_transform(polygon, crs = 4326)

        ## Check if CRS is the same for both objects
        is.logical(terra::crs(sat) == terra::crs(polygon))

        ## Crop and mask the raster object and convert the new AOI in a df
        sat_crop <- terra::crop(sat, vect(polygon))
        sat_crop <- terra::mask(sat_crop, polygon)
        aoi_df <- as.data.frame(sat_crop, xy = TRUE)

        rm(sat, sat_crop)

        ## Remove all rows where value column is NA
        aoi_df <- aoi_df[rowSums(is.na(aoi_df)) != ncol(aoi_df), ]

        ## Full join based on x and y
        if (sat_file == file_l[1]) {
            aoi_full_df <- cbind(aoi_full_df, aoi_df)
            aoi_full_df <- aoi_full_df[,  ! names(aoi_full_df) == "NaN.",  drop = F]

        } else{
            aoi_full_df <- aoi_full_df %>% full_join(aoi_df, by=c("x", "y"))
        }
    }

    return(aoi_full_df)
}


crop_tempo_nc <- function(polygon, file_l, var_nc) {

    aoi_full_df <- data.frame(NaN)

    for (nc_file in file_l) {

        print(paste0("Process NetCDF file: ", nc_file))

        ## Load nc and polygon file
        print("Create nc.brick")
        nc.brick <- raster::brick(x = nc_file, varname = var_nc)

        ## Convert the column names into dates
        options(digits.secs = 6)
        columndate <- as.POSIXct(strptime(gsub('X', '', names(nc.brick)), '%Y.%m.%d.%H.%M.%S.%OS'))
        columndate <- format(columndate, format = '%Y-%m-%d %H:%M:%D')

        ## Rename columns
        names(nc.brick) <- paste0('X_', columndate)

        ## Transform raster and polygon to ESPG:4326
        nc.brick <- raster::projectRaster(nc.brick, crs = "+proj=longlat +datum=WGS84")
        crs(nc.brick) <- "EPSG:4326"
        polygon <- st_transform(polygon, crs = 4326)
        
        ## Crop raster to polygon
        print("Crop raster to polygon")
        re <- terra::crop(nc.brick, polygon)
        re <- terra::mask(re, polygon)
        aoi_df <- as.data.frame(re, xy = TRUE)

        rm(nc.brick, re)

        ## Remove all rows where value column is NA
        aoi_df <- aoi_df[rowSums(is.na(aoi_df)) != ncol(aoi_df), ]

        l <- length(aoi_df)
        print(paste0("Dataset length: ", l-2))
        
        ## Full join based on x and y
        if (nc_file == file_l[1]) {
            aoi_full_df <- cbind(aoi_full_df, aoi_df)
            aoi_full_df <- aoi_full_df[,  ! names(aoi_full_df) == "NaN.",  drop = F]
        
        } else{
            aoi_full_df <- aoi_full_df %>% full_join(aoi_df, by = c("x", "y"))
        }
    }
    rm(polygon, var_nc)
    return(aoi_full_df)
}


#-----------------------------------------------------------------------------
# (2) Create column mean df based on the croped input df from step (1)
#-----------------------------------------------------------------------------

generate_c_mean_df <- function(aoi_df, var_nc, aoi, id, name) {

    aoi_df <- aoi_df[, ! names(aoi_df) == "x", drop = F]
    aoi_df <- aoi_df[, ! names(aoi_df) == "y", drop = F]

    #n <- names(aoi_df)
    #n <- lapply(n, function(x) gsub("\\:", "-", x))
    #n <- lapply(n, function(x) gsub("\\.", "-", x))
    #n <- lapply(n, function(x) gsub("\\X_", "", x))
    #date_l <- as.POSIXct(as.character(date_l), "%Y-%m-%d-%H-%M-%S")

    #if (isTRUE(length(colnames(aoi_df)) == length(date_l))) {
    #    colnames(aoi_df) <- date_l
    #}

    #else {
    #    print("ERROR: colnames has not the same lenght as date_l")
    #}
  
    aoi_c_mean_df <- data.frame(
        id = id,
        name = name,
        cname = names(aoi_df), 
        date = names(aoi_df), 
        variable = var_nc,
        aoi = aoi, 
        mean = colMeans(as.matrix(aoi_df), na.rm=TRUE)
    )
    
    cname1 <- as.POSIXct(aoi_c_mean_df$date, format = "X_%Y.%m.%d.%H.%M.%S")

    if (all(is.na(cname1) == TRUE)) {
        cname1 <- as.Date(aoi_c_mean_df$date, format = "X_%Y.%m.%d")
    }

    aoi_c_mean_df$date <- cname1
    rownames(aoi_c_mean_df) <- NULL

    return(aoi_c_mean_df)
}
