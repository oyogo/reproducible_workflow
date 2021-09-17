earthdams_visualizations <- function(makueni.earthdams){
  
  icons <- awesomeIcons(
    icon = 'tint',
    "fa",
    iconColor = 'black'
  )
  
  makueni.earthdams.sf <- makueni.earthdams %>%
    filter(!is.na(Latitude)) %>%
    mutate(Latitude = as.numeric(as.character(Latitude)),
           Longitude = as.numeric(as.character(Longitude))) %>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
  
earthdamsMap <-  makueni.earthdams.sf %>%
    leaflet() %>%
    addTiles(group = "Open Street Map") %>%
    addTiles(
      "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
      group = "Satellite image"
    ) %>%
    addMarkers(label = ~project_name, icon = icons, popup = ~project_name) %>%
    addLayersControl(
      baseGroups = c("Open Street Map", "Satellite image"),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "topleft")

earthdamsSublocation <- 
  data.table::setDT(makueni.earthdams)[, wards_new := ifelse(wards_new == "NA", "Not specified...", wards_new)] %>% ### Earth-dams sub-location ----
plot_ly() %>%
  add_trace(x = ~reorder(wards_new, -earthdamsWard), 
            y = ~earthdamsWard,
            size = ~earthdamsWard,
            color = ~wards_new,
            alpha = 0.5,
            text = ~paste0("Number of earth-dams in ", 
                           wards_new," ward: ", earthdamsWard),
            hoverinfo = 'text',
            type = "scatter",
            mode = "markers",
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF'), opacity=0.4)) %>%
  layout(
    showlegend = FALSE,
    #font = f.rf,
    xaxis = list(
      title = "Ward"
    ),
    yaxis = list(
      title = "Number of earth-dams"
    )
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE, 
         scrollZoom = FALSE, showAxisDragHandles = TRUE, 
         showSendToCloud = FALSE)


return(list(earthdamsMap,earthdamsSublocation))
  
}