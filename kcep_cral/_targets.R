library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

source("R/data_import_script.R")
source("R/data_transformations_script.R")
source("R/land_management_script.R")
source("R/crop_management_script.R")
source("R/production_script.R")
source("R/marketing_script.R")
source("R/interventions_script.R")




# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","data.table","ggplot2","plotly","tidyr","stringr","stringi","raster","sp","sf","leaflet","d3treeR","treemap"))

# End this file with a list of target objects.
list(
  # record state of R environment---
  tar_target(lockenv,renv::snapshot()),
  # import data from data folder
  tar_target(importing_data, importing_datafiles()),
  # perform data transformations 
  tar_target(data_transformation, data_wrangling(importing_data[[1]],importing_data[[5]])),
  tar_target(land_management,land_management_visualizations(data_transformation[[1]],data_transformation[[2]])),
  tar_target(crop_management,crop_management_visualizations(data_transformation[[3]],data_transformation[[4]])),
  tar_target(production,production_visualizations(importing_data[[2]])),
  tar_target(marketing,marketing_visualizations(importing_data[[4]])),
  tar_target(interventions,interventions_visualizations(importing_data[[3]]))
)
