sschools_tstaff_visualizations <- function(sec_sch_teachers,sec_treemap_data,edep_sschools_cleaned,wards){
  
  # Secondary school teachers - treemap-----
  
  sec.teachers <- treemap::treemap(sec_sch_teachers,
                        index = c("wards_new","teacher","gender"),
                        type = "value",
                        title = " Number of teaching staff per ward as of 2019",
                        vSize = "teacher.count",
                        vColor = "teacher.count",
                        palette = "YlGn",
                        bg.labels = "white",
                        #title.legend = "Number of teachers",
                        position.legend = "none",
                        sortID = "-size",
                        align.labels = list(c("center", "center"), 
                                            c("right", "bottom")))
  
  sec.teachers.plot <- d3treeR::d3tree2(sec.teachers,rootname = "TeachingStaff")
  
  # Student teacher ration----
  
  studteacher.ratio.tmap <-  treemap::treemap(sec_treemap_data,
                         index = c("wards_new"),
                         type = "value",
                         title = "Student teacher ratio per ward as of 2019",
                         vSize = "studteach_ratio",
                         palette = "Blues",
                         vColor = "studteach_ratio",
                         bg.labels = "white",
                         title.legend = "Average ratio",
                         position.legend = "bottom",
                         fontsize.legend = 8)
  studteacher.ratio.plot <- d3treeR::d3tree2(studteacher.ratio.tmap,rootname = "Wards")
  
  # leaflet map----
  # The leaflet map filters schools in response to a slider in shiny. 
 studteacher.ratio.map <-  leaflet(data=edep_sschools_cleaned) %>%
    addTiles(group = "OSM") %>%
    setView(lng = 37.5, lat = -2.3, zoom = 8) %>% 
    addPolygons(data = wards, opacity = 1, fill = FALSE,weight = 2, group = "Subcounty") %>%
    addMarkers(
      lng = ~longitudes, 
      lat = ~latitudes, 
      group = "Schools", 
      clusterOptions = markerClusterOptions(),
      popup = paste0(
        "Name: ", edep_sschools_cleaned$school_name, "<br/>", 
        "Status: ",edep_sschools_cleaned$school_type, "<br/>",
        "Registration status: ", edep_sschools_cleaned$registration_status,"<br/>", 
        "Ward: ", edep_sschools_cleaned$wards_new
      )
    )
  return(list(sec.teachers.plot,studteacher.ratio.plot,studteacher.ratio.map))
}