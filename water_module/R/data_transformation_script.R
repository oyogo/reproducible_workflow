data_wrangling <- function(sand_dams,wards_shapefile,earthdams,makueni.dams,makueni.boreholes){
  
  
  # sand dams----
  colnames(sand_dams)[3:4] <- c("longitude","latitude")
  
  sand_dams <- sand_dams %>% dplyr::filter(longitude != "NA")
  
  coordinates(sand_dams) <- ~ longitude + latitude
  
  proj4string(sand_dams) <- proj4string(wards_shapefile)
  
  s.dams <- over(sand_dams,wards_shapefile[,"CAW"])
  
  sand_dams@data$wards_new <- s.dams$CAW
  
  unique(sand_dams@data$wards_new)
  
  sand_dams <- as.data.frame(sand_dams)
  
  
  # Earth dams----
  
  earthdams$Latitude <- as.numeric(as.matrix(earthdams$Latitude))
  earthdams$Longitude <- as.numeric(as.matrix(earthdams$Longitude))
  
  
  coordinates(earthdams) <- ~ Longitude + Latitude
  
  proj4string(earthdams) <- proj4string(wards_shapefile)
  
  res.earthdams <- over(earthdams,wards_shapefile[,"CAW"])
  
  earthdams@data$wards_new <- res.earthdams$CAW
  
  #unique(boreholes@data$wards2)
  
  #class(boreholes)
  
  earthdams_wnames <- as.data.frame(earthdams)
  
  colnames(earthdams_wnames) <- c("source","division_district","project_name","Latitude","Longitude","production_volume","catchment_area","sub_location","population_served","area_coverage_km2",
                                   "status","remarks","wards_new")
  
  earthdams_wnames <- data.table::setDT(earthdams_wnames)
  earthdams_wnames <- earthdams_wnames[, SubLocation := sub_location]
  earthdams_wnames <- earthdams_wnames[, earthdamsWard := .N, by = "wards_new"]
  
  # Makueni dams----
  makueni.dams <- data.table::setDT(makueni.dams)
  makueni.dams <- makueni.dams[, damsSubCounty := .N, by = "Sub.County"]
  makueni.dams <- makueni.dams[, damsWard := .N, by = c("Sub.County","Ward")]
  makueni.dams <- makueni.dams[, damsSubWard := .N, by = c("Sub.County","Ward","Subward")]
  
  # Makueni boreholes-----
  
  makueni.boreholes <- makueni.boreholes %>% dplyr::filter(Longitude !="NA")
  coordinates(makueni.boreholes) <- ~ Longitude + Latitude
  
  proj4string(makueni.boreholes) <- proj4string(wards_shapefile)
  
  res.boreholes <- over(makueni.boreholes,wards_shapefile[,"CAW"])
  
  makueni.boreholes@data$wards_new <- res.boreholes$CAW
  
  makueni.boreholes <- as.data.frame(makueni.boreholes)
  
  colnames(makueni.boreholes) <- c("financial_year","borehole_name","Latitude","Longitude","ward","sub_location","division_district","depth_metres","water_struck_level","water_rest_level",
                                   "yield_m3_hr","population_served","area_coverage_km2","date","dynamic","status","remarks","contractor","wards_new")
  
  makueni.boreholes <- data.table::setDT(makueni.boreholes)
  makueni.boreholes <- makueni.boreholes[, Yield := as.numeric(as.character(yield_m3_hr))]
  makueni.boreholes <- makueni.boreholes[, Ward := str_to_title(wards_new)]
  makueni.boreholes <- makueni.boreholes[, Borehole := str_to_title(borehole_name)]
  makueni.boreholes <- makueni.boreholes[, Longitude := as.numeric(as.character(Longitude))]
  makueni.boreholes <- makueni.boreholes[, Latitude := as.numeric(as.character(Latitude))]
  
  ### 10 sand dams are not properly georeferenced, 1 doesn't have coordinates. 
  
  # blank_wards <- sand_dams %>% dplyr::filter(is.na(wards_new))
  # no_coordinates <- sand_dams2 %>% dplyr::filter(is.na(longitute))
  # 
  # blankwards_nocoordinates <- blank_wards %>% dplyr::full_join(no_coordinates)
  
  
  # Makueni Africa sand dams---
  
  
  return(list(sand_dams,earthdams_wnames,makueni.dams,makueni.boreholes))
}