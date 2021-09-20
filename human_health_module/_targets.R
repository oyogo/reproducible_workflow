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
source("R/data_transformation_script.R")
source("R/facility_distribution_script.R")
source("R/facility_distance_script.R")
source("R/facility_population_script.R")




# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","data.table","ggplot2","plotly","tidyr","stringr","stringi","raster",
                            "sp","sf","leaflet","d3treeR","treemap","leaflet.extras","leaflet.providers"))

# End this file with a list of target objects.
list(
  #tar_target(education_rawdata_files, initialize_datapaths()),
  tar_target(importing_data, importing_datafiles()),
  tar_target(data_transformations, data_wrangling(importing_data[[1]],importing_data[[4]],importing_data[[3]])),
  tar_target(facility_distribution, facility_distribution_visualizations()),
  tar_target(facility_distance, facility_distance_visualizations(importing_data[[2]],data_transformations[[2]],importing_data[[1]])),
  tar_target(facility_population, facility_population_visualizations(importing_data[[4]],data_transformations[[4]]))
  
)