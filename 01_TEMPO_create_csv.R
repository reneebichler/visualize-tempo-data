## ------------------------------------------------------------------------------------
## Description
## ------------------------------------------------------------------------------------

## Author: Renee Bichler, 2025
## Find me on GitHub: reneebichler

## Within this code, we will create a CSV file for the TEMPO data.

cat("##################################################################################\n")
cat("##\n")
cat("##                                 Alright!\n")
cat("##                       Let's preppare some TEMPO data!\n")
cat("##                             ╰( ^o^)╮╰( ^o^)╮\n")
cat("##\n")
cat("##################################################################################\n")

## ------------------------------------------------------------------------------------
## Libraries
## ------------------------------------------------------------------------------------

packages <- c("sf", "stringi")

## Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {install.packages(packages[!installed])}

## Load all packages
lapply(packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

source("/Other/00_process_satellite_data.R")

## ------------------------------------------------------------------------------------
## Variables
## ------------------------------------------------------------------------------------

satellite <- "TEMPO"
var_name_list <- c("NO2")

time_res <- "H"
var_nc <- "product/vertical_column_troposphere"

nc_path <-    "/RemoteSensing/DATA/TEMPO"
path_out <-   "/RemoteSensing/Results/Other/CSV/TEMPO"

instrument_dic <- c(
    "TEMPO" = "TEMPO"
)

aoi_l <- c(
    "AOI_BO_City"
)

aoi_keys <- c(
    "AOI_BO_City" = "/RemoteSensing/DATA/GEODATA/SHP/mapc_dissolved.shp"
)

## ------------------------------------------------------------------------------------
## Main
## ------------------------------------------------------------------------------------

for (aoi in aoi_l) {

    cat("Process AOI: ", aoi, "\n")

    ## Retrieve the path for the polygon based on the aoi
    polygon_file <- aoi_keys[[aoi]]
    
    ## Get layername from file and remove ".geojson"
    layer_name <- gsub("\\..*", "", strsplit(polygon_file, "/")[[1]][7])

    ## Read polygon
    multipolygon_sf <- read_sf(polygon_file)

    ## Add a column with a new ID
    multipolygon_sf$new_id <- seq_len(nrow(multipolygon_sf))
    multipolygon_sf$path <- polygon_file

    id_df <- data.frame()

    for (i in seq(1, nrow(multipolygon_sf))) {
        for (var_name in var_name_list) {

            cat("Process multipolygon: ", i, "/", nrow(multipolygon_sf), " ; ", var_name, " ; ", aoi, "\n")

            ## Walk through polygons
            polygon <- multipolygon_sf[i,]

            ## Retrieve ID
            id <- polygon$new_id

            if ("NAME" %in% names(polygon)) {
                name <- polygon$NAME
            } else {
                cat("No NAME column in polygon\n")
                name <- NA
            }
            
            ## Row bind the polygon to the id data frame
            id_df <- rbind(id_df, polygon)

            ## Retrieve CRS from polygon_sf and apply to 
            polygon <- st_as_sfc(polygon, crs = st_crs(multipolygon_sf))
            polygon <- st_sf(geometry = polygon)

            ## Make sure polygon is in CRS 4326 in case it is not
            polygon <- st_transform(polygon, crs = 4326)

            ## Retrieve the instument name based on the satellite
            instrument <- instrument_dic[[satellite]]

            ## List all files in nc_path out based on the defined pattern
            file_l <- list.files(paste0(nc_path, "/", var_name), sprintf(".nc"), recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

            ## Crop the satellite data to the polygon
            df_crop_aoi <- crop_tempo_nc(polygon, file_l, var_nc)

            ## Extract column names, skipping the first two
            col_names <- names(df_crop_aoi)[-c(1,2)]

            ## Extract date from the column names
            date_list <- as.POSIXct(strptime(gsub("X_", "", col_names), format = "%Y.%m.%d.%H.%M.%S"))

            ## Remove duplicates and sort
            unique_sorted_dates <- as.Date(sort(unique(date_list)), format = "%Y.%m.%d.%H.%M.%S")

            ## Get start and end date
            sd <- unique_sorted_dates[1]
            ed <- unique_sorted_dates[length(unique_sorted_dates)]
            
            ## Export data frame as csv file
            write.csv(df_crop_aoi, file = paste0(path_out, "/", instrument, "_", time_res, "_TVC_", sd, "-", ed, "_crop_", aoi, "_ID_", id, ".csv"))
            cat("Saving: ", instrument, "_", time_res, "_TVC_", sd, "-", ed, "_crop_", aoi, "_ID_", id, ".csv\n")

            ## Calculate daily mean based on daily data for each utc zone
            df_c_mean_aoi <- generate_c_mean_df(df_crop_aoi, var_nc, aoi, id, name)
            write.csv(df_c_mean_aoi, file = paste0(path_out, "/", instrument, "_", time_res, "_TVC_", sd, "-", ed, "_crop_mean_", aoi, "_ID_", id, ".csv"))
            cat(instrument, "_", time_res, "_TVC_", sd, "-", ed, "_crop_mean_", aoi, "_ID_", id, ".csv\n")
        }
    }
    write.csv(id_df, paste0(path_out, "/", instrument, "_id-df_", aoi, "_ID_", id, ".csv"))
    cat("Saved csv file: ", path_out, "/", instrument, "_id-df_", aoi, "_ID_", id, ".csv\n")
}

cat("##################################################################################\n")
cat("##\n")
cat("##                                Success!\n")
cat("##                             Job completed!\n")
cat("##                                ( ┘^o^)┘\n")
cat("##\n")
cat("##################################################################################\n")
