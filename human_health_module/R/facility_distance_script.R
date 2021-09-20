facility_distance_visualizations <- function(locs,hosp_geo,hospitals){
  
  locs <- locs %>% mutate(Distance = Distance * 0.001)
  
  colnames(hospitals) = stringr::str_replace(colnames(hospitals), " ", ".")
  
  
 distance_distribution <-  ggplot(data = locs, aes(x=Distance)) + 
    geom_histogram(fill = "#548B54", color = "black") +
    labs(
      y = "Number of health facilities", 
      x = "Distance in Kilometres", 
      title = "Distribution of distances between health facilities"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "#79CDCD", color = "#79CDCD"),
      plot.background = element_rect(fill = "#79CDCD", color = "#79CDCD")
    )
  
 
 line_labels<- base::sprintf(
   "<strong>Origin:</strong> %s<br/> <strong>Destination:</strong> %s<br /> <strong>Distance:</strong> %g kilometres</sup>",
   locs$Hospital, locs$Target, round(locs$Distance,2)
 )%>% lapply(htmltools::HTML)

 line_pal<-leaflet::colorBin('RdYlGn', locs$Distance, bins = 5, reverse = T)

 hosp_geo <- hosp_geo %>%
   mutate(marker_color = "cadetblue") %>%
   mutate(marker_color = ifelse(Facility.type == "Comprehensive health centre", "beige",
                                ifelse(Facility.type  == "Dispensary", "lightgreen",
                                       ifelse(Facility.type  == "Basic health centre", "lightblue",
                                              ifelse(Facility.type  == "Primary care hospitals", "pink", marker_color)))))
 
 icons <- leaflet::awesomeIcons(
   icon = 'hospital-o',
   iconColor = 'black',
   lib = 'fa',
   markerColor = hosp_geo$marker_color
 )
 

facility_distanceMap <-  hosp_geo %>%
   leaflet() %>%
   addTiles() %>%
   addAwesomeMarkers(
     label = hosp_geo$Facility.Name, 
     popup = hosp_geo$Facility.type, 
     icon = icons,
     group = "Hospitals and health centers"
   ) %>% 
   addPolylines(
     data = locs,
     color = ~line_pal(Distance),
     weight = 5,
     opacity = .8,
     label = line_labels,
     group = "Lines showing distance to nearest",
     labelOptions = labelOptions(
       style = list("font-weight" = "normal", padding = "3px 8px"),
       textsize = "15px",
       direction = "auto"
     )
   ) %>%
   addLayersControl(
     overlayGroups = c("Hospitals and health centers", "Lines showing distance to nearest"),
     options = layersControlOptions(collapsed = FALSE)
   ) %>% 
   addLegend(
     pal = line_pal, 
     values = locs$Distance, 
     position = 'bottomleft',
     labFormat = labelFormat(suffix = ' Kilometres'),  
     title = 'Distance Between Hospitals'
   )
 

 facility_count_bargraph <- plot_ly(
                         data = hospitals[, .(count = .N), by = Facility.type],
                         x = ~reorder(Facility.type,-count), 
                         y = ~count, 
                         color = 'rgb(158,202,225)',
                         type = "bar",
                         text = ~paste0("Number of facilities: ", count, "\n", "Facility type: ", Facility.type),
                         hoverinfo = 'text'
                      ) %>%
                         layout(
                            title = "Facility type",
                            xaxis = list(title = "Facility type"),
                            yaxis = list(title = "Number of health facilities"),
                            dragmode = "select",
                            #font = f,
                            plot_bgcolor  = "rgba(0, 0, 0, 0)",
                            paper_bgcolor = "rgba(0, 0, 0, 0)"
                         ) %>%
                         config(
                            displayModeBar = FALSE,
                            displaylogo = FALSE,
                            scrollZoom = FALSE,
                            showAxisDragHandles = TRUE,
                            showSendToCloud = FALSE
                         ) %>%
                         hide_legend()

  return(list(distance_distribution,facility_distanceMap,facility_count_bargraph))
  
}