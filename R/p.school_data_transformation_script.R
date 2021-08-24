p.school_data_transformations <- function(edepartment_data,sabstract_data,wards_shapefile){
  
  # data from education department of Makueni County----
  
  # convert table to data.table format---
  edepartment_data <- data.table::setDT(edepartment_data)
  
  # drop redundant and null columns----
  primary_schools_data <- edepartment_data %>% dplyr::select(-c(type_priv_pub2,nullcol))
  primary_schools_data <- primary_schools_data[-c(1,970:1022),]
  
  ## string manipulations
  primary_schools_data$type_day_boarding <- str_to_lower(primary_schools_data$type_day_boarding)
  primary_schools_data$type_day_boarding <- str_trim(primary_schools_data$type_day_boarding)
  primary_schools_data$type_day_boarding <- gsub("day&boarding","day & boarding",primary_schools_data$type_day_boarding)
  primary_schools_data$type_day_boarding <- gsub("day &boarding","day & boarding",primary_schools_data$type_day_boarding)
  primary_schools_data$type_day_boarding <- gsub("day& boarding","day & boarding",primary_schools_data$type_day_boarding)
  primary_schools_data$type_day_boarding <- gsub("day/boarding","day & boarding",primary_schools_data$type_day_boarding)
  primary_schools_data$type_day_boarding <- gsub("both" ,"day & boarding",primary_schools_data$type_day_boarding)
  
  primary_schools_data$type_gender <- str_trim(primary_schools_data$type_gender)
  primary_schools_data$type_gender <- str_to_lower(primary_schools_data$type_gender)
  
  primary_schools_data$type_gender <- gsub("boy" ,"boys",primary_schools_data$type_gender)
  primary_schools_data$type_priv_pub <- str_trim(primary_schools_data$type_priv_pub)
  primary_schools_data$type_priv_pub <- str_to_lower(primary_schools_data$type_priv_pub)
  
  # some schools don't have coordinates, we will assign the ward names manually----
  
  primary_schools_nolatlong <- primary_schools_data %>% dplyr::filter(longitudes %in% c(0,"NA") & latitudes %in% c(0,"NA"))
  
  
  
  primary_schools_nolatlong$ward <- primary_schools_nolatlong$ward %>% stringr::str_replace_all(c("Kitaingo/mukaa" = "Mukaa","Kalanzoni"="Kiima Kiu/Kalanzoni","Kikumbulyu south"="Kikumbulyu South",
                                                                                                  "Kathyaka" = "Kikumbulyu North","Nguu-Masumba"="Nguu Masumba",
                                                                                                  "Nzaui-Kalamba-Kilili"="Nzaui/Kilili/Kalamba","Mulala-Emali"="Emali/Mulala","Wote/ Nziu "="Wote" ,
                                                                                                  "Kako/Waia"="Waia Kako", "Mukaange" = "Mukaa",
                                                                                                  "Mtito - Andei" = "Mtito Andei",
                                                                                                  "MASONGALENI"="Masongeleni","Ivingoni/ Nzambani"="Ivingoni/Nzambani","Masongaleni"="Masongeleni",
                                                                                                  "Kilili- Kalamba"="Nzaui/Kilili/Kalamba",
                                                                                                  "KEE"="Kee","Kisau/Kiteta" ="Kiketa/Kisau"
  ))
  
  # rename column: ward to ward_new----
  colnames(primary_schools_nolatlong)[colnames(primary_schools_nolatlong) == "ward"] <- "ward_new"
  
  # convert latitude and longitude columns to numeric----
  primary_schools_data$latitudes <- as.numeric(as.character(primary_schools_data$latitudes))
  primary_schools_data$longitudes <- as.numeric(primary_schools_data$longitudes)

  # filter dataframe where longitude and latitude columns are not 0 or NA----
  primary_schools_data <- primary_schools_data %>% dplyr::filter(longitudes !=0 & longitudes !="NA" & latitudes !=0 & latitudes !="NA")
  coordinates(primary_schools_data) <- ~ longitudes + latitudes

  # assign coordinate projection of wards shapefile to dataframe----
  proj4string(primary_schools_data) <- proj4string(wards_shapefile)

  res2 <- over(primary_schools_data,wards_shapefile[,"CAW"])

  primary_schools_data@data$wards_new <- res2$CAW

   unique(primary_schools_data@data$wards_new)
   
   primary_schools_data <- as.data.frame(primary_schools_data)
  
  #combined_primary_schoolsdata <- rbind(primary_schools_nolatlong,primary_schools_data)
  
   primary_schools_data$enrol_girls <- as.integer(primary_schools_data$enrol_girls)
   primary_schools_data$enrol_boys <- as.integer(primary_schools_data$enrol_boys)
   primary_schools_data$enrol_total <- as.integer(primary_schools_data$enrol_total)
  
  
  # data from statistical abstract----
  sabstract_data <- data.table::setDT(sabstract_data)
  # melt the data to long format
  sabstract_data_long <- data.table::melt.data.table(sabstract_data, id.vars = "subcounty",
                                                   #measure.vars = public_Boys_2017:private_Girls_2019 ,
                                                   variable.name = "schooltype_sex_year", value.name = "enrolment")
  # split the schooltype_sex_year column
  sabstract_data_split <- separate(sabstract_data_long,col = "schooltype_sex_year",into = c("schooltype" ,"sex", "year"),sep = "_")
  
  
  
  
  return(list(primary_schools_data,sabstract_data_split))

}