pschool_tstaff_visualizations <- function(primary_schools_data,wards = wards_shapefile,pry_treemap_data,pry_sch_teachers){
  
  # student teacher ration treemap-----
  st_ratio <-  treemap::treemap(pry_treemap_data,
                         index = c("wards_new"),
                         type = "value",
                         title = "Student teacher ratio per ward as of 2019",
                         vSize = "studteach_ratio",
                         palette = "Blues",
                         vColor = "studteach_ratio",
                         bg.labels = "white",
                         title.legend = "Average ratio",
                         position.legend = "none")
  
  pry_studteach_ratio_treemap <- d3treeR::d3tree2(st_ratio,rootname = "Wards")
  
  
  # Number of teaching staff per ward-----
  
  primary_sch_teachers <-  pry_sch_teachers[wards_new !=""]
 
  pry_tstaff <-  treemap::treemap(primary_sch_teachers,
                   index = c("wards_new","teacher","gender"),
                   type = "value",
                   title = " Number of teaching staff per ward as of 2019",
                   vSize = "teacher.count",
                   vColor = "teacher.count",
                   palette = "YlGn",
                   bg.labels = "white",
                   #title.legend = "Number of teachers",
                   position.legend = "none",
                   align.labels = list(c("center", "center"), 
                                       c("right", "bottom")))
  
  pry_tstaff_plot <- d3treeR::d3tree2(pry_tstaff,rootname = "TeachingStaff")
  
  
  # Leaflet map----
  
 pry_tstaff_map <- leaflet() %>%
    addTiles(group = "OSM") %>%
    setView(lng = 37.5, lat = -2.3, zoom = 9) %>% 
    addPolygons(data = wards, opacity = 1, fill = FALSE,weight = 2, group = "Subcounty") %>%
    #addMarkers(data = dams2,lng = ~Longitude,lat = ~Latitude, popup = ~as.character(Dam_project_name))
    
    addMarkers(data = primary_schools_data, lng = ~primary_schools_data$longitudes, lat = ~primary_schools_data$latitudes, group = "Schools", clusterOptions = markerClusterOptions(),
               popup = paste0("Name: ",primary_schools_data$school_name, "<br/>",
                              "Status: ",primary_schools_data$type_gender, "<br/>",
                              "Registration status: ", primary_schools_data$registration_status,"<br/>",
                              "Ward: ",primary_schools_data$wards_new))
   
  return(list(pry_studteach_ratio_treemap,pry_tstaff_plot,pry_tstaff_map))
  
}