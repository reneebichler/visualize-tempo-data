## ------------------------------------------------------------------------------------
## Description
## ------------------------------------------------------------------------------------

## Author: Renee Bichler, 2025
## Find me on GitHub: reneebichler

## Within this code, we will create maps and time series plots for the TEMPO data.

cat("##################################################################################\n")
cat("##\n")
cat("##                               You rock!\n")
cat("##                     Let's create awesome TEMPO maps!\n")
cat("##                             ╰( ^o^)╮╰( ^o^)╮\n")
cat("##\n")
cat("##################################################################################\n")

## ------------------------------------------------------------------------------------
## Libraries
## ------------------------------------------------------------------------------------

packages <- c(
    "sf", "dplyr", "readr", "tidyr", "patchwork", "magick", "stringr", "scales", 
    "ggplot2", "ggpubr", "gganimate", "ggnewscale", "ggpattern",  "plotly",
    "rnaturalearth", "ncdf4", "raster"
)

## Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {install.packages(packages[!installed])}

## Load all packages
lapply(packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

show_col_types = FALSE

## ------------------------------------------------------------------------------------
## Variables
## ------------------------------------------------------------------------------------

var_name_list <- c("NO2")
time_resolution <- c("h")
satellite <- "TEMPO"
no2_div_val <- 15
var_nc <- "product/vertical_column_troposphere"

inp_path <- "/RemoteSensing/Results/Other/CSV/TEMPO"
tempo_path <- "/RemoteSensing/DATA/TEMPO/NO2"
out_path <- "/RemoteSensing/Results/Other/Plots"

instrument_dic <- c(
    "TEMPO" = "TEMPO"
)

subtitle_dic <- list(
    NO2 = "Tropospheric Vertical Column of NO\u2082\nVariable: vertical_column_troposphere"
)

unit_dic <- list(
    "NO2" = bquote("10"^.(no2_div_val) * " molec./cm"^2)
)

aoi_l <- c(
    "AOI_BO_City"
)

aoi_keys <- c(
    "AOI_BO_City" = "/RemoteSensing/DATA/GEODATA/SHP/mapc_dissolved.shp"
)

## ------------------------------------------------------------------------------------
## Data for map
## ------------------------------------------------------------------------------------

## States and Provinces
ne_10m_admin_1_states_provinces <- ne_states(returnclass = "sf")
ne_10m_admin_1_states_provinces <- st_transform(ne_10m_admin_1_states_provinces, crs = 4326)
ne_10m_admin_1_states_provinces <- subset(ne_10m_admin_1_states_provinces, subset = iso_a2 == "US")

## Counties
ne_10m_admin_2_counties <- ne_download(scale = 10, type = "admin_2_counties", category = "cultural", returnclass = "sf")
ne_10m_admin_2_counties <- st_transform(ne_10m_admin_2_counties, crs = 4326)
ne_10m_admin_2_counties <- subset(ne_10m_admin_2_counties, subset = ISO_A2 == "US")

## Ocean
ne_10m_ocean <- ne_download(scale = 10, type = "ocean", category = "physical", returnclass = "sf")
ne_10m_ocean <- st_transform(ne_10m_ocean, crs = 4326)

## Populated places
ne_10m_populated_places <- ne_download(scale = 10, type = "populated_places", category = "cultural", returnclass = "sf")
ne_10m_populated_places <- st_transform(ne_10m_populated_places, crs = 4326)
ne_10m_populated_places <- subset(ne_10m_populated_places, subset = SOV_A3 == "USA")

## Airports
ne_10m_airports <- ne_download(scale = 10, type = "airports", category = "cultural", returnclass = "sf")
ne_10m_airports <- st_transform(ne_10m_airports, crs = 4326)

## Ports
ne_10m_ports <- ne_download(scale = 10, type = "ports", category = "cultural", returnclass = "sf")
ne_10m_ports <- st_transform(ne_10m_ports, crs = 4326)

## Lakes
ne_10m_lakes <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf")
ne_10m_lakes <- st_transform(ne_10m_lakes, crs = 4326)

## Rivers
ne_10m_rivers_lake_centerlines <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
ne_10m_rivers_lake_centerlines <- st_transform(ne_10m_rivers_lake_centerlines, crs = 4326)

## ------------------------------------------------------------------------------------
## Functions
## ------------------------------------------------------------------------------------

spline_trend <- stat_smooth(
    method = "gam", 
    formula = y~s(x), 
    color = "steelblue3", 
    fill = "steelblue1", 
    alpha = 0.2, 
    size = 0.8
)

theme1 <- theme_bw() +
theme(
    rect = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(hjust = 0, size = 16), 
    plot.subtitle = element_text(color = "grey35", size = 14),
    plot.caption = element_text(color = "grey35", size = 12),
    plot.background = element_blank(),
    axis.title = element_text(color = "grey35", size = 14),
    axis.text = element_text(color = "grey35", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_blank(), 
    legend.title = element_text(color = "grey35", size = 14),
    legend.title.position = "top",
    legend.text = element_text(color = "grey35", size = 12), 
    legend.position = "bottom", 
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(.4, "cm"),
    legend.margin = margin(-10, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(family = "serif")
)

extract_all_max_values <- function(df, date) {

    ## Remove unnecessary columns
    minmax_df <- df[, !names(df) %in% c("x", "y", "...1")]

    cat("Convert to matrix\n")
    minmax_mat <- as.matrix(minmax_df)

    ## Get the overall maximum value in the matrix
    cat("Extract maximum value\n")
    max_val <- max(minmax_mat, na.rm = TRUE)

    ## Find all indices where the value equals the maximum value
    cat("Find indices of maximum value\n")
    max_idx <- which(minmax_mat == max_val, arr.ind = TRUE)

    ## Extract x, y coordinates and the date (column name) for each maximum value
    max_df <- data.frame(
        date = date,
        lon = df$x[max_idx[, 1]],
        lat = df$y[max_idx[, 1]],
        max_val = max_val
    )

    return(max_df)
}

plot_map_time_series <- function(inp_path, map_file_pattern, ts_file_pattern, aoi, var_name, time_res, polygon_path, output_folder, start_date, end_date) {

    map_l <- list.files(path = inp_path, pattern = map_file_pattern, recursive = FALSE, full.names = TRUE, include.dirs = TRUE)
    ts_l <- list.files(path = inp_path, pattern = ts_file_pattern, recursive = FALSE, full.names = TRUE, include.dirs = TRUE)
    sat_l <- list.files(path = tempo_path, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

    for (map_file in map_l) {
        
        ## Retrieve the id number based on the file name from crop csv
        id_value <- str_extract(map_file, "(?<=_ID_)\\d+")

        ## Retrieve time series path based on the current id from the map_l
        ts_file <- grep(paste0("_ID_", id_value, "\\_plot_df.csv$"), ts_l, value = TRUE)

        cat(paste0("Using time series file: ", ts_file, "\n"))

        ## Load time series data
        ts_data <- read_csv(ts_file) %>% mutate(date = as.POSIXct(date))

        ## Filter ts_l and sat_l data based on start and end date
        ts_data <- ts_data %>% subset(date >= as.POSIXct(start_date) & date <= as.POSIXct(end_date))
        sat_l <- sat_l[which(as.Date(str_extract(sat_l, "\\d{8}T\\d{2}"), format = "%Y%m%dT%H") >= as.Date(start_date) & as.Date(str_extract(sat_l, "\\d{8}T\\d{2}"), format = "%Y%m%dT%H") <= as.Date(end_date))]
  
        ## Load map and polygon data
        map_data <- as.data.frame(read_csv(map_file))

        ## Check if polygon feature is a multipolygon
        if (nrow(read_sf(polygon_path)) > 1) {

            cat("Multipolygon detected!\n")

            ## Add ID to folder name
            folder_name <- paste0(aoi, "_ID_", id_value)

        } else {

            ## A different folder name could be defined here
            folder_name <- paste0(aoi, "_ID_", id_value)
        }

        ## Load polygon
        cat("Load polygon data\n")
        polygon <- read_sf(polygon_path)[id_value,]
        polygon <- st_transform(polygon, crs = 4326)

        ## If polygon includes a name, plot it in the map title
        if (any(colnames(polygon) %in% c("NAME", "name", "Name"))) {
            
            ## Find matching column
            matching_col <- which(colnames(polygon) %in% c("NAME", "name", "Name"))

            ## Rename the column to name
            if (length(matching_col) > 0) {

                ## Rename the column to aoi_name
                colnames(polygon)[matching_col] <- "aoi_name"

                ## Retrieve the name of the AOI
                aoi_name <- paste0(" (", polygon$aoi_name, ")")

            } else {cat("No matching column found!\n")}
        } else {aoi_name <- ""}

        ## Get filename
        fname <- gsub(".csv", "", strsplit(map_file, split = "/")[[1]][length(strsplit(map_file, split = "/")[[1]])])
        cat(paste0("Filename: ", fname, "\n"))

        ## Ensure output folder exists
        if (!dir.exists(output_folder)) {
            dir.create(output_folder, recursive = TRUE)
        }

        ## Create and save individual maps
        image_files <- c()
  
        for (i in seq(1, length(ts_data$date))) {

            ## Retrieve date at index i
            d <- ts_data$date[i]
            cat(paste0("Processing date: ", as.character(d), "\n"))
            
            ## Convert: 2023-12-01 12:17:12 --> 2023.12.01 12:17:12
            d1 <- gsub("-", ".", d)

            ## Convert: 2023.12.01 12:17:12 --> 2023.12.01.12:17:12
            d2 <- gsub(" ", ".", d1)

            ## Convert: 2023.12.01.12:17:12 --> 2023.12.01.12.17.12
            d3 <- gsub(":", ".", d2)

            ## Retrieve satellite data based on the formatted date
            formatted_d <- format(as.POSIXct(d, tz = "UTC"), "%Y%m%dT%H")

            ## Create date object
            ts_data$date <- as.POSIXct(ts_data$date, tz = "UTC")

            ## Load satellite data
            sat_file <- grep(paste0("TEMPO_NO2_L3_V03_", formatted_d), sat_l, value = TRUE)
            cat(paste0("Using satellite file: ", sat_file, "\n"))

            ## ToDo: Add check if sat_file is only one file
            ## if sat_file > 1, select the first one
            if (length(sat_file) > 1) {
                cat("Multiple satellite files found! Using the first one.\n")
                sat_file <- sat_file[1]
            }
            
            ## Load raster data
            cat("Load raster data\n")
            #if (as.character(d) == "2024-01-01 12:52:01") {browser()}
            sat_raster <- raster::brick(x = sat_file, varname = var_nc)
            epsg_code <- "4326"
            raster::crs(sat_raster) <- paste0("EPSG:", epsg_code)

            ## Convert polygon to same CRS as raster
            cat("Transform polygon to same CRS as raster\n")
            polygon <- st_transform(polygon, crs = as.numeric(epsg_code))

            ## Create a rasterized mask (1 inside polygon, NA outside)
            poly_raster <- rasterize(polygon, sat_raster)
            
            ## Invert mask to get outside polygon only
            cat("Create masks\n")
            outer_mask <- mask(sat_raster, poly_raster, maskvalue=1)
            inner_mask <- raster::mask(sat_raster, polygon)

            raster_df <- as.data.frame(sat_raster, xy = TRUE, na.rm = TRUE)
            colnames(raster_df) <- c("x", "y", "value")

            inner_mask_df <- as.data.frame(inner_mask, xy = TRUE, na.rm = TRUE)
            colnames(inner_mask_df) <- c("x", "y", "value")

            ## Set negative values to NA
            cat("Set negative values to NA\n")
            raster_df <- raster_df %>% mutate(value = ifelse(value < 0, NA, value))
            inner_mask_df <- inner_mask_df %>% mutate(value = ifelse(value < 0, NA, value))

            outer_mask_df <- as.data.frame(outer_mask, xy = TRUE)
            colnames(outer_mask_df) <- c("x", "y", "value")

            ## Divide values by no2_div_val for better visualization
            cat("Divide NO2 values by", no2_div_val, "for better visualization\n")
            raster_df$value <- raster_df$value / 10^no2_div_val
            inner_mask_df$value <- inner_mask_df$value / 10^no2_div_val
            outer_mask_df$value <- outer_mask_df$value / 10^no2_div_val

            extent <- as.numeric(st_bbox(polygon))

            ## Colorbar settings
            colors <- c(
                "magenta",          # ~0-
                "#08306B",          # 0
                "#08306B",          # 1
                "#5ea9ff",          # 2
                "#FFFF00",          # 5
                "#FFA500",          # 12
                "#FF0000",          # 28
                "#800080"           # 30
            )

            color_positions <- c(
                -0.000000000000001, # magenta
                0,                  # sharp transition to dark blue
                4,                  # dark blue
                6,                  # light blue
                8,                  # yellow starts
                12,                 # orange
                28,                 # red
                30                  # purple
            )

            limits <- c(0, 30)
            breaks <- c(0, 2, 4, 6, 8, 10, 12, 15, 20, 25, 30)

            rescaled_positions <- rescale(color_positions, to = c(0, 1), from = limits)

            ## Extract min and max values
            if (inner_mask_df %>% filter(!is.na(value)) %>% nrow() != 0) {
                cat("Extract max values within AOI\n")
                max_df <- extract_all_max_values(inner_mask_df, d1)
                minmax_df <- inner_mask_df[, !names(inner_mask_df) %in% c("x", "y", "...1")]
                min_val <- min(minmax_df, na.rm = TRUE)
                max_val <- max(minmax_df, na.rm = TRUE)
            } else {
                cat("No valid values within AOI!\n")
                max_df <- data.frame(date = d1, lon = NA, lat = NA, max_val = NA)
                min_val <- NA
                max_val <- NA
            }

            p_map <- ggplot() +

            ## Plot original satellite data frame
            geom_tile(data = raster_df, mapping = aes(x = x, y = y, fill = value)) +
            scale_fill_gradientn(
                name = bquote("Hourly TEMPO TVC of NO"[2] ~ "[" * .(unit_dic[[var_name]]) * "]"),
                colors = colors,
                values = rescaled_positions,
                limits = limits,
                breaks = breaks,
                oob = scales::squish,
                na.value = "transparent",
                guide = guide_colorbar(
                    draw.ulim = TRUE,
                    draw.llim = TRUE,
                    barwidth = 20,
                    barheight = .8,
                    title.theme = element_text(size = 14),
                    label.theme = element_text(size = 12),
                    text = element_text(family = "serif")
                )
            ) +
            new_scale_fill() +

            geom_tile(data = outer_mask_df, aes(x = x, y = y), fill = "white", alpha = ifelse(is.na(outer_mask_df$value), 0, 0.5)) +

            ## Line Features: Rivers
            geom_sf(data = ne_10m_rivers_lake_centerlines, aes(color = "Rivers"), linewidth = .6) +
            scale_color_manual(
                values = c(
                    "Rivers" = "steelblue1"
                ),
                guide = guide_legend(
                    order = 2,
                    title = "Line Features",
                    title.position = "top",
                    ncol = 1
                )
            ) +

            new_scale_color() +
            
            ## Administrtaive boundaries for states, counties, and AOI
            geom_sf(data = ne_10m_ocean, aes(color = "Waterbodies", fill = "Waterbodies", linetype = "Waterbodies"), linewidth = .6) +
            geom_sf(data = ne_10m_lakes, aes(color = "Waterbodies", fill = "Waterbodies", linetype = "Waterbodies"), linewidth = .6) +
            geom_sf(data = ne_10m_admin_2_counties, aes(color = "Counties", fill = "Counties", linetype = "Counties"), linewidth = .2) +
            geom_sf(data = ne_10m_admin_1_states_provinces, aes(color = "States", fill= "States", linetype = "States"), linewidth = .6) +
            geom_sf(data = polygon, aes(color = "AOI", fill = "AOI", linetype = "AOI"), linewidth = .8) +
            scale_color_manual(
                values = c(
                    "States" = "black",
                    "Counties" = "grey65",
                    "AOI" = "red",
                    "Waterbodies" = "steelblue3"
                )
            ) +
            scale_fill_manual(
                values = c(
                    "States" = "transparent",
                    "Counties" = "transparent",
                    "AOI" = "transparent",
                    "Waterbodies" = "steelblue1"
                )
            ) +
            scale_linetype_manual(
                values = c(
                    "States" = "dashed",
                    "Counties" = "solid",
                    "AOI" = "solid",
                    "Waterbodies" = "solid"
                )
            ) +
            guides(
                color = guide_legend(order = 3, title = "Area Features", title.position = "top", ncol = 1),
                fill = guide_legend(order = 3, title = "Area Features", title.position = "top", ncol = 1),
                linetype = guide_legend(order = 3, title = "Area Features", title.position = "top", ncol = 1)
            ) +

            new_scale_color() +

            ## Airports, Ports, and Cities
            geom_sf(data = ne_10m_airports, aes(color = "Airports"), shape = 23, size = 4, stroke = 1) +
            geom_sf(data = ne_10m_ports, aes(color = "Ports"), shape = 24, size = 4, stroke = 1) +
            geom_sf(data = ne_10m_populated_places, aes(color = "Cities"), size = 2) +
            scale_color_manual(
                values = c("Airports" = "black", "Ports" = "blue", "Cities" = "black"),
                guide = guide_legend(
                    order = 1,
                    title = "Point Features",
                    title.position = "top",
                    ncol = 1
                )
            ) +

            ## Text annotations
            geom_text(
                data = ne_10m_admin_1_states_provinces,
                aes(label = postal, geometry = geometry),
                stat = "sf_coordinates",
                nudge_y = 0.05,
                color = "grey35",
                size = 6,
                family = "serif"
            ) +
            geom_text(
                data = ne_10m_populated_places,
                aes(label = NAME_EN, geometry = geometry),
                stat = "sf_coordinates",
                nudge_y = 0.05,
                color = "black",
                size = 6,
                family = "serif"
            ) +

            ## Set x and y limits for zoom
            coord_sf(
                xlim = c(round(extent[1]-0.5, digits = 2), round(extent[3]+0.5, digits = 2)),
                ylim = c(round(extent[2]-0.5, digits = 2), round(extent[4]+0.5, digits = 2))
            ) +

            labs(
                title = bquote(
                    .(toupper(time_res)) ~ 
                    .(if (var_name == "NO2") bquote(NO[2]) else var_name) ~
                    "Value for" ~ .(aoi) ~ "ID" ~ .(id_value) * .(aoi_name) ~ "for" ~ .(as.character(d))
                ),
                subtitle = subtitle_dic[[var_name]],
                caption = paste0(
                    "\n", # Add space above the caption
                    "Data: U.S. States and Territories (NOAA), version 18 March 2025; ",
                    "NASA Langley Research Center Atmospheric Science Data Center; EPSG: ", epsg_code,
                    "\nThis map includes data from Natural Earth."
                ),
                x = "", y = ""
            ) +
            theme1

            ## Time series plot
            p_time <- ggplot(ts_data, aes(x = date, y = mean)) +

            ## Highlight current date
            geom_vline(xintercept = as.numeric(d), linetype = "dashed", color = "red", linewidth = .8) +

            geom_line(aes(y = interpl), color = "grey45", linewidth = .4) +
            geom_point(
                aes(x = date, y = interpl),
                color = ifelse(ts_data$date == d, "#ff7070", "grey45"),
                size = ifelse(ts_data$date == d, 3, .8),
                shape = ifelse(ts_data$date == d, 13, 16)
            ) +
            geom_line(color = "black", linewidth = .4) +
            geom_point(
                aes(x = date, y = mean),
                color = ifelse(ts_data$date == d, "red", "black"),
                size = ifelse(ts_data$date == d, 3, .8),
                shape = ifelse(ts_data$date == d, 13, 16)
            ) +

            ## Longterm trend
            spline_trend +

            scale_x_datetime(
                date_labels = "%Y-%m-%d",
                date_breaks = "2 day",
                limits = c(
                    min(ts_data$date) - lubridate::days(1),
                    max(ts_data$date) + lubridate::days(1)
                ),
                expand = expansion(add = c(0, 0))
            ) +
            labs(x = "", y = unit_dic[[var_name]]) +
            theme1

            ## Table plot
            ## Style examples: https://rpkgs.datanovia.com/ggpubr/reference/ggtexttable.html (last access: 2024-03-24)
            p_table <- ggtexttable(
                max_df,
                theme = ttheme(
                    "minimal",
                    colnames.style = colnames_style(color = "grey45", fill = "transparent"),
                    rownames.style = rownames_style(color = "grey45", fill = "transparent"),
                    tbody.style = tbody_style(color = "grey45", fill = "transparent")
                ),
                rows = NULL
            )

            ## Combine plots
            full_plot <- p_map / p_time +
            plot_layout(heights = c(3, 1)) & 
            theme(plot.background = element_rect(fill = "transparent", color = NA))

            # Add labels "A" and "B" in serif font
            full_plot <- full_plot + 
            plot_annotation(
                tag_levels = 'A',
                theme = theme(plot.tag = element_text(family = "serif", face = "bold", size = 16, color = "grey35"))
            )

            ## Create filename
            fname1 <- gsub("\\d{4}-\\d{2}-\\d{2}-\\d{4}-\\d{2}-\\d{2}", formatted_d, fname)
            cat(paste0("Filename: ", fname1, "\n"))

            ## Check if folder name exists and if not create it
            if (!dir.exists(file.path(output_folder, paste0("TEMPO/", var_name, "/maps/", folder_name)))) {
                dir.create(file.path(output_folder, paste0("TEMPO/", var_name, "/maps/", folder_name)), recursive = TRUE)
            } else {cat("Folder already exists!\n")}

            file_path <- file.path(output_folder, paste0("TEMPO/", var_name, "/maps/", folder_name))

            ggsave(
                filename = file.path(file_path, paste0(fname1, "_map.png")),
                plot = full_plot,
                width = 10,
                height = 12,
                bg = "transparent"
            )
            cat(paste0("Saved to: ", file.path(file_path, paste0(fname1, "_map.png"))), "\n")

            ggsave(
                filename = file.path(file_path, paste0(fname1, "_map_table.png")),
                plot = p_table,
                width = 4,
                bg = "transparent"
            )
            cat(paste0("Saved to: ", file.path(file_path, paste0(fname1, "_map_table.png"))), "\n")

            ## Create a list of images for animation
            image_files <- c(image_files, file.path(file_path, paste0(fname1, "_map.png")))
        }
  
        ## Read all images
        cat("Create animation\n")
        img_list <- image_read(image_files)

        ## Create animation (fps = frames per second)
        animation <- image_animate(img_list, fps = 1)

        print(file.path(output_folder, paste0("TEMPO/animations/", fname1, ".gif")))

        ## Write out as GIF
        image_write(animation, path = file.path(output_folder, paste0("TEMPO/animations/", fname1, ".gif")))
        cat(paste0("Saved to: ", file.path(output_folder, paste0("TEMPO/animations/", fname1, ".gif\n"))))
    }
}

## ------------------------------------------------------------------------------------
## Main
## ------------------------------------------------------------------------------------

instrument <- instrument_dic[[satellite]]

for (aoi in aoi_l) {
    for (var_name in var_name_list) {
        for (time_res in time_resolution) {

            df_map_plot_pattern <- paste0("^", instrument, "_", toupper(time_res), "_TVC_\\d{4}-\\d{2}-\\d{2}-\\d{4}-\\d{2}-\\d{2}_crop_", aoi, "_ID_\\d{+}.csv$")
            df_ts_plot_patttern <- paste0("^", instrument, "_", toupper(time_res), "_TVC_\\d{4}-\\d{2}-\\d{2}-\\d{4}-\\d{2}-\\d{2}_crop_mean_", aoi, "_ID_\\d{+}_plot_df.csv$")

            polygon <- aoi_keys[[aoi]]

            start_date <- "2024-01-01"
            end_date <- "2024-01-14"

            ## Create map and time series
            plot_map_time_series(inp_path, df_map_plot_pattern, df_ts_plot_patttern, aoi, var_name, time_res, polygon, out_path, start_date, end_date)
        }
    }
}

cat("##################################################################################\n")
cat("##\n")
cat("##                                Success!\n")
cat("##                             Job completed!\n")
cat("##                                ( ┘^o^)┘\n")
cat("##\n")
cat("##################################################################################\n")
