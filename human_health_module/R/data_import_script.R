importing_datafiles <- function(){
  
  hospitals <- fread("../raw_data/human_health/Health_facilities.csv")
  locs <- st_read("../raw_data/shp/locs.shp") %>% mutate(distance = Distance * 0.001)
  population_data <- read.csv("../raw_data/human_health/Population Data_2019 Makueni County.csv")
  
  # import wards shapefile----
  wards <- shapefile("../raw_data/shp/Makueni_Wards.shp") %>%
    sp::spTransform(.,sp::CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  return(list(hospitals,locs,population_data,wards))
}