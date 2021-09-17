boreholes_visualizations <- function(makueni.boreholes){
  
  boreholeDensity <- makueni.boreholes[, .(count = .N),
                                       by = "Ward"] %>% 
    plot_ly() %>%
    add_trace(x = ~reorder(Ward, -count), 
              y = ~count,
              size = ~count,
              color = ~Ward,
              text = ~paste0("Number of ", " boreholes in ", Ward," ward: ", count),
              hoverinfo = 'text',
              type = "scatter",
              mode = "markers",
              marker = list(symbol = 'circle', sizemode = 'diameter',
                            line = list(width = 2, color = '#FFFFFF'), opacity=0.6)) %>%
    layout(
      showlegend = FALSE,
      #font = f.rf,
      xaxis = list(
        title = "Ward"
      ),
      yaxis = list(
        title = "Number of boreholes"
      )
    ) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE, 
           scrollZoom = FALSE, showAxisDragHandles = TRUE, 
           showSendToCloud = FALSE)
  
  boreholeYields <- makueni.boreholes %>% 
    plot_ly() %>%
    add_trace(x = ~reorder(Ward, -Yield, median), 
              y = ~Yield,
              size = ~Yield,
              color = ~Ward,
              #alpha = 0.5,
              text = ~paste0("Yield of ", Borehole, " borehole: (", Ward," ward) ", Yield, " m3/hr"),
              hoverinfo = 'text',
              #sizes = c(200,4000),
              type = "scatter",
              mode = "markers",
              marker = list(symbol = 'circle', sizemode = 'diameter',
                            line = list(width = 2, color = '#FFFFFF'), opacity=0.6)) %>%
    layout(
      showlegend = FALSE,
      #font = f.rf,
      #barmode = 'stack',
      xaxis = list(
        title = "Ward"
      ),
      yaxis = list(
        title = "Yield (m3/hr)"
      )
    ) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE, 
           scrollZoom = FALSE, showAxisDragHandles = TRUE, 
           showSendToCloud = FALSE)
  
  
  boreholesMap <- makueni.boreholes %>%
    filter(!is.na(Longitude)) %>%
    leaflet() %>%
    addTiles(group = "Open Street Map") %>%
    addTiles(
      "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
      group = "Satellite image"
    ) %>%
    addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~borehole_name) %>%
    addLayersControl(
      baseGroups = c("Open Street Map", "Satellite image"),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "topleft")
  
  makueni.boreholes <- makueni.boreholes[, depth_metres := 
                                           as.numeric(as.character(depth_metres))]
  boreholeDepth <-  makueni.boreholes %>%
    filter(!is.na(depth_metres)) %>%
    plot_ly() %>%
    add_trace(x = ~reorder(stringr::str_to_title(borehole_name), depth_metres), 
              y = ~(0-depth_metres),
              color = ~borehole_name,
              colors = "Blues",
              type = "bar",
              text = ~paste0("Depth of ", borehole_name, " borehole (", wards_new," ward): ", depth_metres, " m"),
              hoverinfo = 'text') %>%
    layout(
      showlegend = FALSE,
      xaxis = list(
        title = "Borehole name"
      ),
      yaxis = list(
        title = "Depth (m)"
      )
    ) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE, 
           scrollZoom = FALSE, showAxisDragHandles = TRUE, 
           showSendToCloud = FALSE)
 
   
    
    
    return(list(boreholeDensity,boreholeYields,boreholeDepth,boreholesMap))
}