importing_datafiles <- function(){
  
  # land management data
  nresource_data <- fread("../raw_data/kcep_cral/kcep_natural_resource_mgt.csv")[county=="Makueni"]
  kcep.agricultural_assets <- fread("../raw_data/kcep_cral/kcep_agricultural_assets.csv")[county=="Makueni"]
  cropprod_data <- data.table::fread("../raw_data/kcep_cral/kcep_crop_production_training.csv")[county=="Makueni"]
  crops_sold <- fread("../raw_data/kcep_cral/kcep_crop_sold.csv")[county=="Makueni"]
  crop_mngtdata <- fread("../raw_data/kcep_cral/kcep_crop_mgt_to_storage.csv")[county=="Makueni"]
  
  return(list(nresource_data,kcep.agricultural_assets,cropprod_data,crops_sold,crop_mngtdata))
  
}