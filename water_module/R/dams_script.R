dams_visualizations <- function(makueni.dams){
  
  # County dams----
  
  damsOverview <- makueni.dams %>%
    plot_ly() %>%
    add_trace(x = ~reorder(Sub.County, -damsSubCounty), 
              y = ~damsSubCounty,
              size = ~damsSubCounty,
              color = ~Sub.County,
              alpha = 0.5,
              #sizes = c(200,4000),
              type = "scatter",
              mode = "markers",
              marker = list(symbol = 'circle', sizemode = 'diameter',
                            line = list(width = 2, color = '#FFFFFF'), opacity=0.4)) %>%
    add_text(x = ~reorder(Sub.County, -damsSubCounty), 
             y = ~damsSubCounty, text = ~damsSubCounty,
             showarrow = FALSE,
             color = I("black")) %>%
    layout(
      showlegend = FALSE,
      #font = f.rf,
      #barmode = 'stack',
      xaxis = list(
        title = "Sub County"
      ),
      yaxis = list(
        title = "Number of dams"
      )
    ) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE, 
           scrollZoom = FALSE, showAxisDragHandles = TRUE, 
           showSendToCloud = FALSE)
  
  # dams per ward----
  damsWard <- makueni.dams %>%
    plot_ly() %>%
    add_trace(x = ~reorder(Ward, -damsWard), 
              y = ~damsWard,
              size = ~damsWard,
              color = ~Sub.County,
              alpha = 0.5,
              text = ~paste0("Number of dam projects in ", Ward," (",Sub.County,"): ", damsWard),
              hoverinfo = 'text',
              #sizes = c(200,4000),
              type = "scatter",
              mode = "markers",
              marker = list(symbol = 'circle', sizemode = 'diameter',
                            line = list(width = 2, color = '#FFFFFF'), opacity=0.4)) %>%
    layout(
      showlegend = FALSE,
      #font = f.rf,
      #barmode = 'stack',
      xaxis = list(
        title = "Ward"
      ),
      yaxis = list(
        title = "Number of dams"
      )
    ) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE, 
           scrollZoom = FALSE, showAxisDragHandles = TRUE, 
           showSendToCloud = FALSE)
  
  # dams map----
  
  icons <- awesomeIcons(
    icon = 'tint',
    "fa",
    iconColor = 'black'
  )
  
 #  makueni.dams.sf <- makueni.dams %>%
 #    dplyr::filter(!is.na(Latitude)) %>%
 #    dplyr::mutate(Latitude = as.numeric(as.character(Latitude)),
 #           Longitude = as.numeric(as.character(Longitude))) %>%
 #    st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
 # 
 # damsMap <-  makueni.dams.sf %>%
 #    leaflet() %>%
 #    addTiles(group = "Open Street Map") %>%
 #    addTiles(
 #      "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
 #      group = "Satellite image"
 #    ) %>%
 #    addMarkers(lng = ~Longitude, lat = ~Latitude, label = ~Dam.project.name, icon = icons, labelOptions = labelOptions(noHide = TRUE)) %>%
 #    addLayersControl(
 #      baseGroups = c("Open Street Map", "Satellite image"),
 #      position = "topright",
 #      options = layersControlOptions(collapsed = FALSE)
 #    ) %>%
 #    addScaleBar(position = "topleft")
  #,damsMap
  return(list(damsOverview,damsWard))
}