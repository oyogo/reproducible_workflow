sanddams_visualizations <- function(sand_dams){
  
  sand_dams_perc <-  data.table::setDT(sand_dams)[,.N, by=c("wards_new")]
  
  sdams_plot <- plot_ly(sand_dams_perc,
          y = ~N,
          x = ~reorder(wards_new,-N), 
          #color = ~village.population ,
          showscale = FALSE,
          #colors = 'YlOrBr',
          text = ~paste0("Ward: ", wards_new, "\n", "Number of sand dams: ", N)) %>%
    add_bars(showlegend=FALSE, hoverinfo='text') %>%
    hide_colorbar() %>%
    layout(bargap = 0.5, 
           title =  "Sand dams in makueni county", 
           xaxis = list(title = "Ward", tickangle = 60),
           #yaxis = list(title= "sand dams",ticksuffix = "%"),
           margin = list(r=100, t = 100),
           yaxis = list(title= "Number of sand dams"),
           autotick = FALSE,
           tick0 = 0,
           dtick = 1,
           tickmode = "linear"#,
           # plot_bgcolor  = "rgba(0, 0, 0, 0)",
           # paper_bgcolor = "rgba(0, 0, 0, 0)",
           # font = list(color = 'white')
    ) 
  
  
  # sand dams map---
 sanddams_map <-  leaflet() %>% 
    setView(lng = 37.6, lat = -2.2, zoom = 9) %>%
    #addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addMarkers(data = sand_dams, popup = paste0(
      "Village: ",sand_dams$village,"<br/>",
      "Village population: ",sand_dams$village.population,"<br/>",
      "Community: ",sand_dams$communty,"<br/>",
      "Ward: ",sand_dams$wards_new,"<br/>" )) %>%
    addHeatmap(data = sand_dams, radius = 10) # use sanddams_combined
 
 return(list(sdams_plot,sanddams_map))
}