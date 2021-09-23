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



# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","data.table","ggplot2","plotly","tidyr","stringr","stringi","raster","sp","sf","leaflet","d3treeR","treemap"))

# End this file with a list of target objects.
list(
  #tar_target(education_rawdata_files, initialize_datapaths()),
  tar_target(importing_data, importing_datafiles()),
  tar_target(),
  tar_target(),
  tar_target()
)
