#library(data.table)
importing_datafiles <- function(){
  
  # data from Makueni County education department----
  primary.schools_edepartment <- read.csv("../raw_data/PRIMARY SCHOOLS MERGED 1.csv", 
                                       fileEncoding = "latin1", stringsAsFactors = FALSE, na.strings = "NA")
  
  colnames(primary.schools_edepartment) <- c("s_no","county","subcounty","education_zone","constituency","ward","school_name","type_priv_pub","latitudes","longitudes","nullcol",
                                  "tsc_code","knec_code","nemis_code","sponsor","landarea_ha","level","type_priv_pub2","registration_status","type_gender","type_day_boarding",
                                  "type_regular_integrated","enrol_boys","enrol_girls","enrol_total","tsc_teachers_male","tsc_teachers_female","countygov_male","countygov_female",
                                  "bom_male","bom_female","total_tsc","total_bom","total_teachers","hresource_csupport","staff_nonteaching","class_perm","class_temp",
                                  "lab_perm","lab_temp","boystoilet_perm","boystoilet_temp","girlstoilet_perm","girlstoilet_temp","kcpe_primary","kcse_secondary","contact_principal")
  
  # data from statistical abstract 2020----
  primary.schools_statabstract <- fread("../raw_data/primary_enrollment_sctypesex.csv")
  
  
  
  # secondary schools raw data from statistical abstract----
  
  secondary_enrolment_2015 <- read.csv("../raw_data/secondary_enrolscsexclass_2015.csv")
  secondary_enrolment_2016 <- read.csv("../raw_data/secondary_enrolscsexclass_2016.csv")
  secondary_enrolment_2017 <- read.csv("../raw_data/secondary_enrolscsexclass_2017.csv")
  secondary_enrolment_2018 <- read.csv("../raw_data/secondary_enrolscsexclass_2018.csv")
  secondary_enrolment_2019 <- read.csv("../raw_data/secondary_enrolscsexclass_2019.csv")
  
  combined_data <- merge(secondary_enrolment_2015,secondary_enrolment_2016)
  combined_data <- merge(combined_data,secondary_enrolment_2017)
  combined_data <- merge(combined_data,secondary_enrolment_2018)
  secschools_stabstract <- merge(combined_data,secondary_enrolment_2019)
  
  # secondary schools raw data from Education department----
  
  sschools_eddep <- read.csv("../raw_data/SECONDARY LIST.csv") 
  
  colnames(sschools_eddep) <- c("s_no","county","subcounty","education_zone","constituency","ward","school_name","latitudes","longitudes","school_type","tsc_code","knec_code","nemis_code",
                             "sponsor","landarea_ha","level","type_priv_pub","type_priv_pub2","registration_status","empty_column","type_day_boarding","type_regular_integrated",
                             "enrol_boys","enrol_girls","enrol_total","tsc_teachers_male","tsc_teachers_female","countygov_male","countygov_female","bom_male",
                             "bom_female","total_tsc","total_bom","total_teachers","hresource_csupport","staff_nonteaching","class_perm","class_temp","lab_perm",
                             "lab_temp","boystoilet_perm","boystoilet_temp","girlstoilet_perm","girlstoilet_temp","kcpe_primary","kcse_sec","contact_principal")
  
  
  # import wards shapefile----
  wards_shapefile <- shapefile("../raw_data/shp/Makueni_Wards.shp") %>%
    sp::spTransform(.,sp::CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  return(list(
    primary.schools_edepartment,
    primary.schools_statabstract,
    secschools_stabstract,
    wards_shapefile,
    sschools_eddep
  ))
}