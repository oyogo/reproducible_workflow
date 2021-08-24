s.school_data_transformations <- function(sabstract_secschools_data,wards_shapefile,edepartment_secschools_data){
  
  # wrangling data from education department----
  ## drop redundant columns
  edepartment_secschools_data <- edepartment_secschools_data %>% dplyr::select(-c(type_priv_pub2,empty_column))
  edepartment_secschools_data <- edepartment_secschools_data[-1,]
  
  ## trim white spaces and convert ward column to sentence case----
  edepartment_secschools_data$ward <- stringr::str_trim(edepartment_secschools_data$ward)
  edepartment_secschools_data$ward <- stringr::str_to_sentence(edepartment_secschools_data$ward)
  
  
  ## separate schools that don't have coordinates from those that have ---
  ## This will help us to match ward names of those with GPS coordinates to the ward names of Makueni county shapefile
  sec_schools_nogps <- edepartment_secschools_data %>% dplyr::filter(longitudes == 0)
  
  ## clean the ward column with names that are consistent with those in the Makueni County shapefile
  sec_schools_nogps$ward <- sec_schools_nogps$ward %>% stringr::str_replace_all(c("Muvau/kikumini"="Muvau/Kikumini", "Ivingoni - nzambani" = "Ivingoni/Nzambani","Mtito andei" = "Mtito Andei",
                                                                                "Kalanzoni/kiimakiu" = "Kiima Kiu/Kalanzoni","Kalanzoni" = "Kiima Kiu/Kalanzoni","Kisau/kiteta" = "Kiketa/Kisau",
                                                                                "Wote-nziu"="Wote","Kee/kivani" = "Kee","Kikumini/muvau"="Muvau/Kikumini","Nziu-wote"="Wote",
                                                                                "Ukia/iuani" = "Ukia","Kithungo/kitundu"="Kithungo/Kitundu","Kitundu/kithungo"="Kithungo/Kitundu",
                                                                                "Masongaleni"="Masongeleni","Mangelete-nzambani"="Ivingoni/Nzambani","Kikumbulyu south" ="Kikumbulyu South",
                                                                                "Kikumbulyu north"="Kikumbulyu North","Nzambani-ivingoni"="Ivingoni/Nzambani","Nzambani"="Ivingoni/Nzambani",
                                                                                "Nthange"="Thange","Kasikeu/kiou"="Kasikeu","Kiimakiu"="Kiima Kiu/Kalanzoni","Mukaa/kitaingo"="Mukaa",
                                                                                "Waia/kako"="Waia Kako","Kitise/kithuki"="Kitise/Kithuki","Kathonzweni/mbuvo"="Kathonzweni",
                                                                                "Kitise"="Kitise/Kithuki","Kithuki/kitise"="Kitise/Kithuki","Kilili/kalamba"="Nzaui/Kilili/Kalamba",
                                                                                "Mulala/emali" = "Emali/Mulala","Kalamba" ="Nzaui/Kilili/Kalamba","Nguu/masumba"="Nguu Masumba"))
  
  
  
  
  
  sec_schools_nogps$ward <- gsub("Ivingoni/Ivingoni/Nzambani","Ivingoni/Nzambani",sec_schools_nogps$ward)
  sec_schools_nogps$ward <- gsub("Kiima Kiu/Kiima Kiu/Kalanzoni","Kiima Kiu/Kalanzoni",sec_schools_nogps$ward)
  
  sec_schools_nogps <- sec_schools_nogps %>% mutate(wards_new = ward)
  
  ## Now lets select schools that have gps coordinates and then match the ward names with those in the shapefile
  ## This is a quicker way to clean and match the ward names hence ensuring consistency
  
  sec_schools_withgps <- edepartment_secschools_data %>% dplyr::filter(longitudes > 0)
  coordinates(sec_schools_withgps) <- ~ longitudes + latitudes
  
  proj4string(sec_schools_withgps) <- proj4string(wards_shapefile)
  
  res2 <- over(sec_schools_withgps,wards_shapefile[,"CAW"])
  
  sec_schools_withgps@data$wards_new <- res2$CAW
  
  sec_schools_withgps <- as.data.frame(sec_schools_withgps)
  
  edep_sschools_cleaned <- rbind(sec_schools_withgps,sec_schools_nogps)
  
  ## some string operations, largely conversion of column data types 
  edep_sschools_cleaned <- data.table::setDT(edep_sschools_cleaned)[, `:=`(
    enrol_total = as.integer(enrol_total),
    type_day_boarding = stringr::str_to_lower(stringr::str_trim(type_day_boarding)),
    school_type = stringr::str_to_sentence(stringr::str_trim(school_type)),
    type_priv_pub = stringr::str_to_sentence(stringr::str_trim(type_priv_pub)),
    total_teachers = as.integer(total_teachers),
    enrol_girls = as.integer(enrol_girls),
    longitudes = as.double(longitudes),
    latitudes = as.double(latitudes),
    kcse_sec = as.double(kcse_sec)
  )] 

  ## drop observations with no gps and school mean grade
  edep_sschools_cleaned <- edep_sschools_cleaned[longitudes != 0.00 & kcse_sec != "NA"]
  
  ## split the school mean grades to categories---
  edep_sschools_cleaned <- edep_sschools_cleaned[, mean_category := ordered(stringi::stri_replace_all_fixed(
    cut(kcse_sec, breaks = seq(1,12,1), right=FALSE), 
    pattern = c("[1,2)","[2,3)","[3,4)","[4,5)","[5,6)", "[6,7)","[7,8)","[8,9)","[9,10)","[10,11)","[11,12)"),
    replacement = c("[1-2]","[2-3]","[3-4]","[4-5]","[5-6]","[6-7]","[7-8]","[8-9]","[9-10]","[10-11]","[11-12]"), 
    vectorize_all = FALSE
  ), levels =c("[1-2]","[2-3]","[3-4]","[4-5]","[5-6]","[6-7]","[7-8]","[8-9]","[9-10]","[10-11]","[11-12]"))
  ]
  
  ## string operations - ensuring there's consistency in school types (names)
  edep_sschools_cleaned$type_day_boarding <- stringi::stri_replace_all_fixed(edep_sschools_cleaned$type_day_boarding,
                                                                    pattern = c("both","day and boarding", "day& boarding","day&boarding"),
                                                                    replacement = c("day & boarding","day & boarding","day & boarding","day & boarding"),
                                                                    vectorise_all = FALSE
  )
  
  ## create a column with student - teacher ratio
  edep_sschools_cleaned <- edep_sschools_cleaned[, studteach_ratio := round(enrol_total/total_teachers, 0)]
  edep_sschools_cleaned <- edep_sschools_cleaned[studteach_ratio != Inf & wards_new != "NA"]
  
  
  ## Create table for school teachers---
  
  makueni_secsch_teachers <- data.table::melt(
    edep_sschools_cleaned,
    id.vars = "wards_new",
    measure.vars = c("tsc_teachers_male","tsc_teachers_female", "bom_male","bom_female"),
    variable.name = "teachers_gender",
    value.name = "teacher.count"
  )
  
  makueni_secsch_teachers$teacher.count <- as.double(makueni_secsch_teachers$teacher.count)
  
  makueni_secsch_teachers <- data.table(makueni_secsch_teachers)
  
  makueni_secsch_teachers <- makueni_secsch_teachers[, teachers_gender := stringr::str_remove(teachers_gender, "_teachers")]
  makueni_secsch_teachers <- makueni_secsch_teachers[, teacher := stringr::str_split(teachers_gender, "_", simplify = TRUE)[,1]]
  makueni_secsch_teachers <- makueni_secsch_teachers[, gender := stringr::str_split(teachers_gender, "_", simplify = TRUE)[,2]]
  
  
  ## create table for treemap data---
  
  secondary_treemap_data <- edep_sschools_cleaned %>% dplyr::group_by(wards_new) %>%
    dplyr::summarise(enrol_total = sum(enrol_total), total_teachers = sum(total_teachers)) %>%
    dplyr::mutate(studteach_ratio=round(enrol_total/total_teachers,0)) %>%
    dplyr::filter(studteach_ratio !=Inf & wards_new !="NA")
  
  secondary_treemap_data$bucket <- cut(secondary_treemap_data$studteach_ratio, breaks = c(2,7,12,17,22,27,32,37,42,46))
  
  # wrangling data from statistical abstract----
  
  ## change data to long format
  
  sabstract_secschools_data_long <-  data.table::melt(sabstract_secschools_data, id.vars = "subcounty",
                                                                               variable.name = "Gender_form_year",value.name="enrolment")
  
  ## Split the gender_form_year column into separate columns
  sabstract_secschools_cleaned <- separate(sabstract_secschools_data_long,col = "Gender_form_year",into = c("Gender" ,"form", "year"),sep = "_")


  return(list(edep_sschools_cleaned,sabstract_secschools_cleaned,makueni_secsch_teachers,secondary_treemap_data))
  
}