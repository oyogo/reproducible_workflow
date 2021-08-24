initialize_datapaths <- function(){
  
  
  primary.schools <- fread("data/primary_enrollment_sctypesex.csv")
  
  # secondary schools raw data----
  
   secondary_schools_2015 <- fread("data/secondary_enrolscsexclass_2015.csv")
  # secondary_schools_2016 <- fread("data/secondary_enrolscsexclass_2016.csv")
  # secondary_schools_2017 <- fread("data/secondary_enrolscsexclass_2017.csv")
  # secondary_schools_2018 <- fread("data/secondary_enrolscsexclass_2018.csv")
  # secondary_schools_2019 <- fread("data/secondary_enrolscsexclass_2019.csv")
  return(list(primary.schools,secondary_schools_2015))
  # return(list(
  #   primary.schools,
  #   secondary_schools_2015,
  #   secondary_schools_2016,
  #   secondary_schools_2017,
  #   secondary_schools_2018,
  #   secondary_schools_2019
  #   
  # ))
}