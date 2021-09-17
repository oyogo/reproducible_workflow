importing_datafiles <- function(){
  
  # Sand dams----
  sand_dams <- data.table(read.csv("../raw_data/water/MAKUENI TOTAL SAND DAMS.csv"))
  sand_dams <- sand_dams[,-c("X","X.1")]
  
  # wards shapefile----
  wards_shapefile <- shapefile("../raw_data/shp/Makueni_Wards.shp")%>%
    sp::spTransform(.,sp::CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  
  earthdams <- fread("../raw_data/water/Makueni_earthdams.csv", encoding = "UTF-8")
  
  
  
  # County dams----
  
  makueni.dams <- fread("../raw_data/water/County_dams.csv")
  
  # Boreholes-----
  
  makueni.boreholes <- fread("../raw_data/water/Makueni_boreholes.csv")
  
  
  ### Data proc ----
  # makueni.dams <- data.table(dbReadTable(con.sqlite, "makueni_dams_clean"))
  # makueni.earthdams <- data.table(dbReadTable(con.sqlite, "makueni_earthdams_clean"))
  # makueni.boreholes <- data.table(dbReadTable(con.sqlite, "makueni_boreholes_clean"))[!is.na(yield_m3_hr)]
  # makueni.sanddams <- data.table(dbReadTable(con.sqlite, "makueni_sanddams_clean"))
  # makueni.africa_sanddams <- data.table(dbReadTable(con.sqlite, "makueni_total_sanddams"))
  # 
  # dbDisconnect(con.sqlite)
  # 
  # makueni.dams <- makueni.dams[, damsSubCounty := .N, by = "Sub.County"]
  # makueni.dams <- makueni.dams[, damsWard := .N, by = c("Sub.County","Ward")]
  # makueni.dams <- makueni.dams[, damsSubWard := .N, by = c("Sub.County","Ward","Subward")]
  # 
  # makueni.earthdams <- makueni.earthdams[, SubLocation := sub_location]
  # makueni.earthdams <- makueni.earthdams[, earthdamsWard := .N, by = "wards_new"]
  # 
  # makueni.boreholes <- makueni.boreholes[, Yield := as.numeric(as.character(yield_m3_hr))]
  # makueni.boreholes <- makueni.boreholes[, Ward := str_to_title(wards_new)]
  # makueni.boreholes <- makueni.boreholes[, Borehole := str_to_title(borehole_name)]
  # makueni.boreholes <- makueni.boreholes[, Longitude := as.numeric(as.character(Longitude))]
  # makueni.boreholes <- makueni.boreholes[, Latitude := as.numeric(as.character(Latitude))]
  # 
  # sand_dams <- makueni.africa_sanddams %>% dplyr::filter(wards_new != "NA")
  # 
  # sand_dams_count <-  data.table::setDT(sand_dams)[,.N, by=c("wards_new")]
  # 
  # sand_dams_combined <- merge(sand_dams, sand_dams_count, by = "wards_new")
  
  return(list(sand_dams,wards_shapefile,earthdams,makueni.dams,makueni.boreholes))
}