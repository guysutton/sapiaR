# Define map_sapia function to make a SAPIA-style map
# - Users can choose to plot all records, presence/absence or an abundance map.

# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rlang)
library(ggspatial)
library(raster)

#############################################################################
#############################################################################
#############################################################################

# Define function
map_sapia <- function(data = sapia_plant_db,
                      species,
                      map_type = "all",
                      col = plant_species,
                      add_copyright = FALSE,
                      add_name = TRUE,
                      ...) {

  #####
  # - Section 0: Download base layer for map
  ####

  # Import base map
  world <- rnaturalearth::ne_countries(scale = "medium",
                                       returnclass = "sf")

  # Keep only South Africa
  world <- world %>%
    dplyr::filter(name == "South Africa")


  #####
  # - Section 1: Set up raster cells to derive abundances
  ####

  # Initialise a raster of uniform cells over South Africa
  rast <- raster::raster(xmn= -180,
                         ymn= -90,
                         xmx = 180,
                         ymx = 90,
                         resolution = 0.5,
                         crs = "+proj=longlat +datum=WGS84 +no_defs")

  # Set the resolution of the raster
  x_res <- 0.25
  y_res <- 0.25
  res(rast) <- c(x_res, y_res)
  res(rast)

  # Filter which plant species to plot
  # Must use %in%, not ==
  data <- dplyr::filter({{ data }}, {{ col }} %in% {{ species }})

  # ----- Filter only GPS points within South Africa

  # Make RSA shapefile into SpatialPolygonDataFrame
  rsa_sp <- as(world, 'Spatial')

  # Make GPS points into SpatialPointsDataFrame
  gps_sp <- sp::SpatialPointsDataFrame(coords = data[, c("longitude", "latitude")],
                                       data = data,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))


  # Filter and keep only points within SpatialPolygon
  filter_data <- gps_sp[!is.na(over(gps_sp, as(rsa_sp, "SpatialPolygons"))), ]

  # Make filtered points into a dataframe
  filter_data <- as.data.frame(filter_data)

  # Get xy-cords into a matrix
  xy <- filter_data %>%
    dplyr::select(longitude, latitude) %>%
    as.matrix()

  # Count the number of GPS points in each raster cell
  r0 <- raster::rasterize(xy,
                          rast,
                          fun = "count")

  #####
  # - Section 2: Filter data for presence/absense map
  ####

  # Convert matrix to data frame
  datf <- as.data.frame(r0,
                        xy=TRUE)


  # Filter out all cells with 0 records
  data_pres <- datf %>%
    dplyr::filter(layer > 0)

  #####
  # - Section 3: Filter data for abundance map
  ####

  # Replace NA with 0
  data_abun <- datf %>%
    dplyr::filter(layer > 0) %>%
    tidyr::drop_na(layer)

  # Cut abundance vector into abundance categories
  # Cat1 = 0 records (filtered out = white cells)
  # Cat2 = 1 record
  # Cat3 = 2-5 records
  # Cat4 = 6-25 records
  # Cat5 = 25+
  data_abun$layer <-
    cut(data_abun$layer,
        breaks = c(1, 2, 5, 25, 10000),
        labels = c("1", "2-5", "6-25", "25+"),
        include.lowest = T)

  # Set ggplot theme
  theme_set(theme_classic() +
              theme(panel.border = element_rect(colour = "black", fill = NA),
                    axis.text = element_text(colour = "black"),
                    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                    axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                    legend.position = "none"))

  #####
  # -  Section 5: Plot all records
  #####

  # If users specify map_type = "all",
  # the following map is returned.

  if (map_type == "all") {

    # Plot map

    p <- ggplot(data = world) +
      scale_fill_viridis_c(na.value = "white") +
      geom_sf(fill = "NA") +
      geom_point(data = filter_data, aes(x = longitude,
                                         y = latitude)) +
      labs(x = "Longitude",
           y = "Latitude") +
      coord_sf(xlim = c(15.5, 33.5),
               ylim = c(-35, -21.75),
               expand = FALSE) +
      annotation_scale(location = "br", #
                       style = "ticks",
                       width_hint = 0.150) +
      annotation_north_arrow(location = "br",
                             which_north = "true",
                             pad_x = unit(0.175, "in"),
                             pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering) +
      annotate("text",
               x = 16.5,
               y = -22.5,
               fontface = "italic",
               size = 5,
               hjust = 0,
               label = {{ species }})

  }

  #####
  # -  Section 5: Plot presence/absense map
  #####

  # If users specify map_type = "presence",
  # the following map is returned.

  if (map_type == "presence") {

    # Plot map

    p <- ggplot(data = world) +
      scale_fill_viridis_c(na.value = "white") +
      geom_sf(fill = "NA") +
      geom_point(data = data_pres, aes(x = x,
                                       y = y,
                                       fill = layer)) +
      labs(x = "Longitude",
           y = "Latitude") +
      coord_sf(xlim = c(15.5, 33.5),
               ylim = c(-35, -21.75),
               expand = FALSE) +
      annotation_scale(location = "br", #
                       style = "ticks",
                       width_hint = 0.150) +
      annotation_north_arrow(location = "br",
                             which_north = "true",
                             pad_x = unit(0.175, "in"),
                             pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering) +
      annotate("text",
               x = 16.5,
               y = -22.5,
               fontface = "italic",
               size = 5,
               hjust = 0,
               label = {{ species }})

  }

  #####
  # -  Section 6: Plot abundance map
  #####

  # If users specify map_type = "abundance",
  # the following map is returned.

  if (map_type == "abundance") {

    # Plot abundance map
    p <- ggplot(data = world) +
      geom_sf(fill = "white") +
      geom_tile(data = data_abun, aes(x = x,
                                      y = y,
                                      fill = layer)) +
      scale_fill_manual(values = c("gray80",
                                   "gray60",
                                   "gray40",
                                   "black"),
                        drop = FALSE,
                        na.translate = F,
                        labels = c("1", "2-5", "6-25", "25+")) +
      labs(x = "Longitude",
           y = "Latitude",
           fill = "No. of records") +
      coord_sf(xlim = c(15.5, 33.5),
               ylim = c(-35, -21.75),
               expand = FALSE) +
      annotation_scale(location = "br", #
                       style = "ticks",
                       width_hint = 0.150) +
      annotation_north_arrow(location = "br",
                             which_north = "true",
                             pad_x = unit(0.175, "in"),
                             pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering) +
      theme(legend.position = c(0.14, 0.775),
            legend.title = element_text(size = 12.5),
            legend.text = element_text(size = 12.5)) +
      annotate("text",
               x = 16.5,
               y = -22.5,
               fontface = "italic",
               size = 5,
               hjust = 0,
               label = {{ species }})

  }

  ###
  # - Add conditional elements
  ###

  # 1. Copyright symbol and text

  if( add_copyright == TRUE ) {

    p +
      annotate("text",
               x = 21.5,
               y = -34.75,
               fontface = "italic",
               size = 3,
               hjust = 0,
               label = "Copyright \u00A9 2020. Centre for Biological Control")
  } else {

    p

  }

  # 2. Add species name to plot

  if ( add_name == TRUE ) {

    p +
      annotate("text",
               x = 16.5,
               y = -22.5,
               fontface = "italic",
               size = 5,
               hjust = 0,
               label = {{ species }})

  } else {

    p

  }

  p

}

################################################################################
################################################################################
################################################################################
