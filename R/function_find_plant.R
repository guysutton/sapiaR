# Define find_plant function to find correct plant names in SAPIA database

#############################################################################
#############################################################################
#############################################################################

# Define function
find_plant <- function(data = sapia_plant_db,
                       col = plant_species,
                       search_name,
                       ...) {

  #####
  # - Section 1: Process data
  ####

  # Pull a vector with all the plant species names
  plant_names <- {{ data }} %>%
    dplyr::filter(plant_species %in%
                    stringr::str_subset(plant_species, {{ search_name }})) %>%
    dplyr::distinct(plant_species) %>%
    dplyr::pull()

  # Print table

  plant_names

}

##############################################################################
##############################################################################
##############################################################################

# Test function
find_plant(search_name = "Acacia")

# Find plant name, and then map
plants_to_map <- find_plant(search_name = "Pereskia")
map_sapia(species = plants_to_map)

# Find plant name, and then map
plants_to_map <- find_plant(search_name = "Eichh")
map_sapia(species = plants_to_map)
