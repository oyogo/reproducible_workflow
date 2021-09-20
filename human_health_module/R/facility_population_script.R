facility_population_visualizations <- function(wards,join_pop){
  
  pal <- leaflet::colorBin("Blues", domain = join_pop$pop_per_hos, bin = 5) 
  
 facility_populationMap <- leaflet::leaflet() %>%
                                        addTiles() %>%
                                        setView(lng = 38, lat = -2.2, zoom = 9) %>%
                                        addPolygons(
                                          data = wards,
                                          opacity = 1,
                                          fillColor = ~pal(as.numeric(join_pop$pop_per_hos)),
                                          popup = paste(
                                            "Ward: ",join_pop$Ward ,
                                            "<br>","Population per Hospital: ",join_pop$pop_per_hos,
                                            "<br>","Population: ",join_pop$sumtotal,
                                            "<br>","Number of health facilities: ", join_pop$hpop_count
                                          ),
                                          fillOpacity = 0.7,
                                          dashArray = "3",
                                          color = "black",
                                          weight = 2,
                                          group = "Wards",
                                          highlight = highlightOptions(
                                            weight = 5,
                                            color = "#666",
                                            dashArray = "",
                                            fillOpacity = 0.7,
                                            bringToFront = TRUE
                                          ))  %>%
                                        addLegend(
                                          data = join_pop,
                                          "topright", 
                                          pal = pal, 
                                          values = ~pop_per_hos, 
                                          opacity = 0.7, 
                                          title = "Population per health facility"
                                        )
 
 
 population_facilityBargraph <-  plot_ly(
                                     join_pop,
                                     x = ~reorder(Ward,pop_per_hos),
                                     y = ~pop_per_hos,
                                     color = "#E0EEE0",
                                     showscale = FALSE,
                                     text = ~paste0("Ward: ", Ward, "\n", pop_per_hos, " people per hospital or\nhealth facility")
                                    ) %>%
                                   add_bars(showlegend=FALSE, hoverinfo='text') %>%
                                   layout(
                                     bargap = 0.5,
                                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                                     title = "Population per health facility",
                                     xaxis = list(title = "Ward",tickangle = 45),
                                     yaxis = list(title= "Population per health facility"),
                                     dtick = 1,
                                     tick0 = 0,
                                     tickmode = "linear"
                                   )
 
 return(list(facility_populationMap,population_facilityBargraph))
  
}