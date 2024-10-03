install.packages(rgbif)
library(rgbif)

# Check out the number of occurrences found in GBIF:
occ_count()

# number of observations reported for Germany:
occ_count(
  country = rgbif::rgb_country_codes("Germany")$iso2, 
  basisOfRecord = 'OBSERVATION'
  )

# Check for synonyms
name_suggest(q='Sorex alpinus', rank='species')

# Check number of records - here filtered to those with coordinate information
occ_search(scientificName = "Sorex alpinus", hasCoordinate=T, limit = 10)

# Turning hasCoordinate ON only returns records that contain latitude and 
# longitude coordinates with them
# This is the longest list of arguments to an R function I have seen in my life.

gbif_shrew <- occ_search(
  scientificName = "Sorex alpinus", 
  hasCoordinate = TRUE, 
  basisOfRecord ='HUMAN_OBSERVATION', 
  limit = 600
  )$data

if (!require(maps)) {install.packages("maps")} # download package map only if you don't have it already
par(mar = rep(0, 4)) # delete plot margins
maps::map('world', xlim = c(5, 30), ylim=c(40,55))
points(
  gbif_shrew$decimalLongitude, 
  gbif_shrew$decimalLatitude, 
  col='#65a3b4',  
  pch=16
  )

if(!require(CoordinateCleaner)) {install.packages("CoordinateCleaner")}

gbif_shrew <- gbif_shrew %>% dplyr::filter(!is.na(decimalLatitude))

# We now clean the coordinates and check for outliers - see ?clean_coordinates for more options
?CoordinateCleaner::clean_coordinates
gbif_shrew_cleaned_coord <- gbif_shrew %>% 
  CoordinateCleaner::clean_coordinates(
    lon = "decimalLongitude", 
    lat = "decimalLatitude", 
    countries = "countryCode", 
    tests = c("centroids", "outliers", "duplicates", "institutions"), 
    inst_rad = 1000
  ) %>%
  dplyr::filter(.summary == TRUE) # drop sus entries

# Plot all remaining points after cleaning
maps::map('world', xlim = c(5, 30), ylim=c(40,55))
points(
  gbif_shrew$decimalLongitude, 
  gbif_shrew$decimalLatitude, 
  col='red',  
  pch=19
)
points(
  gbif_shrew_cleaned_coord$decimalLongitude, 
  gbif_shrew_cleaned_coord$decimalLatitude, 
  col = 'blue',
  pch = 18
)

# Tests:
# centroids: flag records which coordinates lie 
# outside of the country they are associated with,
# indicating the coordinates may have a typo
# EDIT: WRONG - centroids checks whether the coordinates correspond to the centroid
# (i.e., geographical center) of their country, which would indicate the coordinates
# are actually a default value and not the actual location of the observation
# outliers: a single, isolated record is sus
# duplicates: if two entries share exactly the same data, it is likely to be 
# a single record that has mistakenly been entered twice.
# institutions: zoos, botanical gardens or museums may keep specimens of the
# species, which may be mistakenly uploaded as observations

# other issues (Zizka et al.) include switched latitude and longitude;
# coordinates corresponding to country capital (same issue as centroids);
# rounding of coordinates, which may bias location far away from the site of
# the actual observation, and other conversion errors.

save(gbif_shrew_cleaned_coord, file = "data/gbif_shrew_cleaned.RData")

# Great crested newt
gbif_newt <- occ_search(scientificName = "Triturus cristatus", hasCoordinate = TRUE, limit = 500)$data

gbif_newt_cleaned <- gbif_newt %>% 
  CoordinateCleaner::clean_coordinates(
    lon = "decimalLongitude", 
    lat = "decimalLatitude", 
    countries = "countryCode", 
    tests = c("centroids", "outliers", "duplicates", "institutions"), 
    inst_rad = 1000
  )

# Let's do some ggplot funsies
if(!require(ggpubr)) {install.packages("ggpubr")}
if(!require(jpeg)) {install.packages("jpeg")}

img <- jpeg::readJPEG("data/Die_Reptilien_und_Amphibien_mitteleuropas_(1912)_(20942539445).jpg")
# picture too large for version control, you'll need to download it to 
# reproduce the figure; or just comment out the background_image() line

za_warudo <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

gg_easy <- ggplot2::ggplot() +
  ggpubr::background_image(img) +
  ggplot2::geom_sf(data = za_warudo) + # renders THE WORLD
  ggplot2::geom_point( # we add our occurrence data
    ggplot2::aes(
      x = decimalLongitude, 
      y = decimalLatitude,
      colour = .summary
      ), 
    data = gbif_newt_cleaned
  ) +
  ggplot2::coord_sf(
    # crop to europe
    xlim = c(-30, 40), 
    ylim = c(40,65)
    ) +
  ggplot2::scale_colour_discrete(
    name = "vibe check",
    labels = c("sus", "cool n good")
    ) + 
  ggplot2::theme(
    axis.title = ggplot2::element_blank()
  )

# Save as a PNG
# ragg::agg_png(filename = "figures/triturus_cristatus_map.png", height = 720, res = 150)
gg_easy
#dev.off()
