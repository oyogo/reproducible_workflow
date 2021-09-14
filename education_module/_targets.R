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
source("R/initialize_datapaths_script.R")
source("R/education_data_import_script.R")
source("R/p.school_data_transformation_script.R")
source("R/pschool_distribution_script.R")
source("R/pschool_performance_script.R")
source("R/pschool_teachingstaff_script.R")
source("R/pschool_enrolment_script.R")
source("R/s.school_data_transformation_script.R")
source("R/sschool_enrolment_script.R")
source("R/sschool_performance_script.R")
source("R/sschool_tstaff_script.R")
source("R/sschool_distribution_script.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","data.table","ggplot2","plotly","tidyr","stringr","stringi","raster","sp","sf","leaflet","d3treeR","treemap"))

# End this file with a list of target objects.
list(
  #tar_target(education_rawdata_files, initialize_datapaths()),
  tar_target(importing_data, importing_datafiles()),
  tar_target(pschools_data_transformation, p.school_data_transformations(importing_data[[1]],importing_data[[2]],importing_data[[4]])),
  tar_target(pryschools_distribution_visualizations, pschools_distribution_visualizations(pschools_data_transformation[[1]],importing_data[[4]])),
  tar_target(pryschools_performance_visualizations, pschools_performance_visualizations(pschools_data_transformation[[1]])),
  tar_target(pryschools_tstaff, pschool_tstaff_visualizations(pschools_data_transformation[[1]],importing_data[[4]],
                                                              pschools_data_transformation[[3]],pschools_data_transformation[[4]])),
  tar_target(pryschools_enrolment_visualizations, pschools_enrolment_visualizations(pschools_data_transformation[[2]],pschools_data_transformation[[1]],importing_data[[4]])),
  tar_target(secschools_data_transformation, s.school_data_transformations(importing_data[[3]],importing_data[[4]],importing_data[[5]])),
  tar_target(secschools_enrolment_visualizations, sschools_enrolment_visualizations(secschools_data_transformation[[2]],
                                                                                    secschools_data_transformation[[5]],secschools_data_transformation[[1]],importing_data[[4]])),
  tar_target(secschools_performance_visualizations, sschools_perfomance_visualizations(secschools_data_transformation[[1]])),
  tar_target(secschools_tstaff_visualizations,sschools_tstaff_visualizations(secschools_data_transformation[[3]],
                                                                             secschools_data_transformation[[4]],secschools_data_transformation[[1]],importing_data[[4]])),
  tar_target(secschools_distribution,
             sschools_distribution_visualizations(secschools_data_transformation[[1]],importing_data[[4]]))
)
