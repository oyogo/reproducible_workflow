data_wrangling <- function(hospitals,wards,population_data){
  
  hosp_population <- hospitals
  colnames(hospitals) = stringr::str_replace(colnames(hospitals), " ", ".")
  
  
  hospitals <- hospitals[, `:=`(
    Longitude = as.double(Longitude),
    Latitude = as.double(Latitude),
    Regulatory.body = stringr::str_trim(Regulatory.body),
    Facility.type = mgsub::mgsub(
      stringr::str_to_sentence(stringr::str_trim(Facility.type)),
      c("Medical clinic","Medical center","Vct centre"),
      c("Medical clinics and health centers","Medical clinics and health centers","Medical clinics and health centers")
    )
  )]
  
  #hosp_population <- hospitals 
  
  hospitals[, c(13,14)] <- lapply(hospitals[, c(13,14)], function(x){ as.numeric(x) })
  
  hosp_geo <- st_as_sf(
    hospitals, 
    coords = c("Longitude","Latitude"), 
    crs = st_crs(4326)
  )
  
  hosp_geo$Facility.type <- as.factor(stringr::str_to_sentence(stringr::str_trim(hosp_geo$Facility.type)))
  
  dist_matrix <- as.data.frame(st_distance(hosp_geo,hosp_geo)) %>%
    `colnames<-`(hospitals[, Facility.Name]) %>%
    cbind(name = hospitals[, Facility.Name])

  
 
  
  hosp_population <- sf::st_as_sf(hospitals, coords = c("Longitude", "Latitude")) %>%
    as_Spatial()
  
  proj4string(hosp_population) <- proj4string(wards)
  rest <- over(hosp_population,wards[,"CAW"])
  
  hosp_population@data$wards_new <- rest$CAW
  hosp_population.df <- as.data.frame(hosp_population)
  
  # hosp_population <- data.table(dbGetQuery(conn = conn, statement = "select * from `hosp_population_df`;"))
  # population_data <- data.table(dbGetQuery(conn = conn, statement = "select * from `makueni_population`;"))
  
  population_data <- data.table::setDT(population_data)[, SUM_Total := as.integer(gsub(",","",SUM_Total))]
  #dbDisconnect(conn)
  ward_population <- population_data[, .(sumtotal = sum(SUM_Total)), by = Ward]
  ward_population <- ward_population[, Ward := stringr::str_trim(Ward)]
  join_pop <- data.table(merge(ward_population, wards, by.x = "Ward", by.y = "CAW"))
  hpop_count <- data.table::setDT(hosp_population.df)[, .(hpop_count = .N), by = "wards_new"]
  join_pop <- merge(join_pop, hpop_count, by.x = "Ward", by.y = "wards_new")
  join_pop <- join_pop[, pop_per_hos := round(sumtotal/hpop_count,0)]
  #join_pop <- st_as_sf(join_pop, crs = st_crs(4326))


 return(list(hospitals = hospitals, hosp_geo = hosp_geo, dist_matrix = dist_matrix, join_pop))
  
}