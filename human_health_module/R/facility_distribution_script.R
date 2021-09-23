facility_distribution_visualizations <- function(hospitals,wards){
  
  # The graph and map for this section uses filters which is implemented in shiny (reactive environment)
  colnames(hospitals) = stringr::str_replace(colnames(hospitals), " ", ".")
  
  hosp.icon <- list(iconUrl = "https://static.thenounproject.com/png/368042-200.png", iconSize = c(32, 32))
  
 facility_distributionMap <- leaflet()%>%
                                  addTiles(group = "OSM")%>%
                                  setView(lng = 37.5, lat = -2.0, zoom = 9) %>% 
                                  addPolygons(
                                    data=wards,
                                    opacity = 1,
                                    fill=FALSE,
                                    weight=2, 
                                    popup = paste0(wards$CAW," ward"),
                                    options=popupOptions(keepInView = TRUE,closeButton = FALSE)
                                  ) %>%
                                  addMarkers(
                                    data = hospitals, 
                                    lng =~Longitude,
                                    lat =~Latitude,
                                    group = "locations",
                                    clusterOptions = markerClusterOptions(),
                                    icon = hosp.icon,
                                    popup= paste0(
                                      "Name: ",hospitals$Facility.Name,"<br/>",
                                      "Ward: ",hospitals$Ward,"<br/>",
                                      "Type: ",hospitals$Facility.type,"<br/>",
                                      "Level: ",hospitals$Keph.level,"<br/>",
                                      "Owner: ",hospitals$Owner,"<br/>",
                                      "Regulatory body: ",hospitals$Regulatory.body,"<br/>",
                                      "Beds: ",hospitals$Beds,"<br/>",
                                      "Cots: ",hospitals$Cots,"<br/>"
                                    )
                                  ) %>%
                                  addLayersControl(
                                    baseGroups = "OSM",
                                    position = "bottomleft",
                                    overlayGroups = c("locations","heatmap"),
                                    options = layersControlOptions(collapsed = TRUE)
                                  )
 
 
                       fsummary <- setDT(hospitals)[,.(percentages=round(100*.N/nrow(hospitals),0)), by=Facility.type]
                      
                       fsummary[, label := paste0(Facility.type, "\n", percentages,"%")]
 
                       summary_facilities_type <- fsummary
                       packing.services <- packcircles::circleProgressiveLayout(summary_facilities_type[, percentages], sizetype='area')
                       # Add these packing information to the initial data frame
                       facility.packing <- cbind(summary_facilities_type, packing.services)
                       facility.data <- packcircles::circleLayoutVertices(packing.services, npoints=50)
                       facility.packing$id<-1:nrow(facility.packing)
                       facility.data<-dplyr::left_join(facility.data,facility.packing,by="id")
                       facility.data<-facility.data[,c(1:5)]
                       colnames(facility.data)[1:2]<-c("x","y")
                       
                   facility_bubblechart <-    ggplot2::ggplot(data = facility.data) +
                         geom_polygon(
                           aes(x, y, group = id, fill=stringr::str_wrap(Facility.type,15), text = paste0("Facility type: ", Facility.type)),
                           alpha=.9
                         ) +
                         geom_text(
                           data = facility.packing, 
                           aes(x, y), 
                           size=5,
                           label = paste0(summary_facilities_type[, percentages],"%")
                         ) +
                         theme_void() +
                         coord_equal()+
                         ggtitle(paste0("Health facilities count according to type"))+
                         theme(plot.title = element_text(size=14,face="bold",hjust=0.5),
                               legend.text = element_text(size=12),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               legend.position="bottom",
                               axis.line = element_blank()) +
                         scale_fill_brewer(palette = "Set3",name="") 
 
 return(list(facility_distributionMap,facility_bubblechart))
}