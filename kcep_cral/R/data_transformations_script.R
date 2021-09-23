data_wrangling <- function(nresource_data,crop_mngtdata){
  
  # # filter Makueni County
  # makueni.kcep_natural_resource_mgt <- nresource_data %>% dplyr::filter(county == "Makueni")
  
  # Transform data to long format
  nresource_data.gather <- nresource_data %>% gather(cropmngt_apply:climaange_type, key = "Management type", value = "value") %>%
    separate("Management type", into = c("Type","measure","hhmemberapplying"))
  
  # string manipulation - shorten the text
  
  nresource_clean <- nresource_data.gather
  
  ### crop management
  
  # somedf_cropmngt <- nresource_clean %>% dplyr::filter(Type == "cropmngt")
  # unique(somedf_cropmngt$value)
  
  nresource_clean$value <- gsub("Seedling production and transplantation", "Seedling production & transplantation", nresource_clean$value , fixed = TRUE)
  
  ### Pests and disease management
  
  # somedf_pests <- nresource_data.gather %>% dplyr::filter(Type == "pestdiseasemngt")
  # 
  # unique(somedf_pests$value)
  
  nresource_clean$value <- gsub("Other (Specify)", "Others",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Others (Specify)", "Others", nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Plant resistant/tolerant varieties", "Plant resistant varieties",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Dry crops under low humidity conditions", "Low humidity crop drying",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Use of certified/treated seeds", "Use of certified seeds",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Use of pest traps and/or use chemicals in the traps", "Pests &/or chemicals in traps",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Pest control without use of pesticides", "Pests control_no pesticides",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Remove affected panicles/plant parts", "Remove affected plant parts",nresource_clean$value, fixed = TRUE)
  
  ### soil conservation
  
  # somedf_soilcons <- nresource_clean %>% dplyr::filter(Type == "soilconservation")
  # 
  # unique(somedf_soilcons$value)
  
  nresource_clean$value <- gsub("Use of improved insecticides", "Improved insecticides",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Others (Specify)", "Others",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Use of improved insecticides", "Improved insecticides",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Organic manure/composite manure", "Organic manure/compost manure",nresource_clean$value, fixed = TRUE)
  
  ### soil irrigation
  
  # somedf_irrig <- nresource_clean %>% dplyr::filter(Type == "irrigwatermngt")
  # 
  # unique(somedf_irrig$value)
  
  
  nresource_clean$value <- gsub("Farm yard manure and/or compost manure application", "Farm yard & composite manure",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("In situ water harvesting structures (Zai pits,  pitting,  ridge and furrow,  stone bunds,  bench terracing,  Fanya juu/Chini,  Strip cropping,  contour farming,  trash lines,  deep tillage,  etc)", "In situ water harvesting structures",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Ex-situ Water harvesting structures (Cut-off-drains,  water pans,  microcatchment,  road-runoff, etc)", "Ex-situ water harvesting structures",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("In situ water harvesting structures (Zai pits,  pitting,  ridge and furrow,  stone bunds,  bench terracing,  Fanya juu chini Strip cropping,  contour farming,  trash lines,  deep tillage,  etc)", "In situ water harvesting structures",nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Mulching (crop residues)","Mulching", nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Other (Specify)","Others", nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Organic manure/composite manure", "Organic manure/compost manure",nresource_clean$value, fixed = TRUE)
  
  ## climate change
  
  # somedf_clim <- nresource_clean %>% dplyr::filter(Type == "climaange")
  # unique(somedf_clim$value)
  
  nresource_clean$value <- gsub("Low-or no-till practises (minimum tillage/Jab planting,  ox-drawn direct seeders)","Low-or no-till practises", nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Adoption of Zai planting pit (Box technologies)","zai planting pit", nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub(" Yes","", nresource_clean$value, fixed = TRUE)
  nresource_clean$value <- gsub("Diversified crop associations and rotation","Diversified crop associations & rotation", nresource_clean$value, fixed = TRUE)
  
  
  techniques_data <- nresource_clean %>% dplyr::filter(measure == "type" & value != "No" & value != "Yes") %>%
    tidyr::separate_rows(value, sep = ",")
  
  techniques_data$value <- stringr::str_trim(techniques_data$value)
  
  techniques_data$Type <- as.factor(techniques_data$Type)
  
  management_techniques <- techniques_data%>% dplyr::group_by(Type, value, subcounty)%>%
    dplyr::filter(value != "" & value != "Yes" )%>%
    dplyr::count(value) %>% tidyr::drop_na() %>% as.data.frame()
  
  
  ## crop management
  colnames(crop_mngtdata)[34] <- "dryinglosscauses"
  colnames(crop_mngtdata)[27:37] <- paste0("drying", sep = "_",colnames(crop_mngtdata)[27:37])
  colnames(crop_mngtdata)[38:42] <- paste0("transportation", sep = "_", colnames(crop_mngtdata)[38:42])
  colnames(crop_mngtdata)[43:49] <- paste0("shellthresh", sep = "_", colnames(crop_mngtdata)[43:49])
  colnames(crop_mngtdata)[50:63] <- paste0("storage", sep = "_", colnames(crop_mngtdata)[50:63])
  
  gather_stages <-crop_mngtdata %>% tidyr::gather(27:63, key = "stage", value = "value")
  separate_stagecol <- gather_stages %>% tidyr::separate("stage", into = c("stage", "measurement"), sep = "_")
  
  return(list(nresource_clean,management_techniques,separate_stagecol,crop_mngtdata))
}