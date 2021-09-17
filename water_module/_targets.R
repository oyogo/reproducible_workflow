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
source("R/dams_script.R")
source("R/boreholes_script.R")
source("R/earthdams_script.R")
source("R/sanddams_script.R")



# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","data.table","ggplot2","plotly","tidyr","stringr","stringi","raster",
                            "sp","sf","leaflet","d3treeR","treemap","leaflet.extras","leaflet.providers"))

# End this file with a list of target objects.
list(
  #tar_target(education_rawdata_files, initialize_datapaths()),
  tar_target(importing_data, importing_datafiles()),
  tar_target(data_transformations, data_wrangling(importing_data[[1]],
                                                  importing_data[[2]],importing_data[[3]],importing_data[[4]],importing_data[[5]])),
  tar_target(dams, dams_visualizations(data_transformations[[3]])),
  tar_target(boreholes, boreholes_visualizations(data_transformations[[4]])),
  tar_target(earthdams, earthdams_visualizations(data_transformations[[2]])),
  tar_target(sanddams, sanddams_visualizations(data_transformations[[1]]))
)