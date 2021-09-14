sschools_distribution_visualizations <- function(edep_sschools_cleaned,wards){
  
  # School distribution - all schools plot-----
  
  edep_sschools_cleaned <- data.table::copy(edep_sschools_cleaned)
  
  allschools_data <-  edep_sschools_cleaned[,.(count=.N), by=c("wards_new")]
  
 secschools_distribution_plot <-  plot_ly(
    allschools_data,
    y = ~count,
    x = ~wards_new, 
    color = ~count ,
    showscale = FALSE,
    colors = 'YlOrBr',
    text = ~paste0("Ward: ", wards_new, "\n", "Number of schools: ", count)
  ) %>%
    #hovertext = ~paste("Ward: ", gender_data$wards_new, '<br> No of schools: ', n, '<br> Type of school: ', gender_data$school_type),hoverinfo='text') %>%
    add_bars(showlegend=FALSE, hoverinfo='text') %>%
    hide_colorbar() %>%
    layout(
      bargap = 0.5, group = ~count ,
      margin = list( b = 200), 
      #yaxis = list(tickformat = "%"),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      title =  "All schools as of 2019", 
      xaxis = list(title = "Ward", tickangle = 45),
      yaxis = list(title= "Number of schools"),
      autotick = FALSE,
      tick0 = 0,
      dtick = 1,
      tickmode = "linear")
 
 # school distribution - gender ----
 
 
 secschool.dist.gender.data  <-  edep_sschools_cleaned[school_type=="Mixed",.(count=.N), by=c("school_type","wards_new")]
 
 secschool.dist.gender.plot <- plot_ly(secschool.dist.gender.data,
                                   y = ~count, 
                                   x = ~wards_new, 
                                   color = ~count , 
                                   showscale = FALSE,
                                   colors = 'YlOrBr',
                                   text = ~paste0("Ward: ", wards_new, "\n", "Number of schools: ", count)) %>%
                                   add_bars(showlegend=FALSE, hoverinfo='text') %>%
                                   hide_colorbar() %>%
                                   layout(
                                     bargap = 0.5, group = ~wards_new ,
                                     margin = list( b = 200), 
                                     #yaxis = list(tickformat = "%"),
                                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                                     title = paste0("Mixed"," schools as of 2019"), # On shiny replace Mixed with input$gender
                                     xaxis = list(title = "Ward", tickangle = 45),
                                     yaxis = list(title= "Number of schools",tick0 = 0,dtick = 1,tickmode = "linear"),
                                     autotick = FALSE)
 
 # school distribution -  ownership-----
 
 secschool.dist.ownership.data <- edep_sschools_cleaned[type_priv_pub == "Public",.(count=.N), by=c("type_priv_pub","wards_new")] 
 
 secschool.dist.ownership.plot <-  plot_ly(
                       secschool.dist.ownership.data, 
                       y = ~count, 
                       x = ~wards_new, 
                       color = ~count , 
                       showscale = FALSE,
                       colors = 'Oranges',
                       text = ~paste0("Ward: ", wards_new, "\n", "Number of schools: ", count)) %>%
                       add_bars(showlegend=FALSE, hoverinfo='text' ) %>%
                       hide_colorbar() %>%
                       layout(
                         bargap = 0.5, 
                         group = ~wards_new,
                         margin = list( b = 150),
                         plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = "rgba(0, 0, 0, 0)",
                         title = paste0("Public"," schools as of 2019"), 
                         xaxis = list(title = "Ward",tickangle = 45),
                         yaxis = list(title= "Number of schools", tick0 = 0,dtick = 1,tickmode = "linear"))
                     
 # school distribution - boarding facility----
 
 secschool.dist.boarding.data <-  edep_sschools_cleaned[type_day_boarding=="day",.(count=.N), by=c("type_day_boarding","wards_new")] 
 
 secschool.dist.boarding.plot <-  plot_ly(secschool.dist.boarding.data, 
                       y = ~count, 
                       x = ~wards_new, 
                       color = ~count , 
                       showscale = FALSE,
                       colors = 'BuPu',
                       text = ~paste0("Ward: ", wards_new, "\n", "Number of schools: ", count)) %>%
                       add_bars(showlegend=FALSE, hoverinfo='text') %>%
                       hide_colorbar() %>%
                       layout(
                         bargap = 0.5, group = ~wards_new ,
                         margin = list( b = 200), 
                         plot_bgcolor  = "rgba(0, 0, 0, 0)",
                         paper_bgcolor = "rgba(0, 0, 0, 0)",
                         title = paste0("Day"," schools as of 2019"), 
                         xaxis = list(title = "Ward",tickangle = 45),
                         yaxis = list(title= "Number of schools",tick0 = 0,dtick = 1,tickmode = "linear")) 
 
 # school distribution - leaflet map-----
 
 secschool.dist.boarding.map <-  leaflet(data=edep_sschools_cleaned) %>%
             addTiles(group = "OSM") %>%
             setView(lng = 37.5, lat = -2.3, zoom = 9) %>% 
             addPolygons(data = wards, opacity = 1, fill = FALSE,weight = 2, group = "Subcounty") %>%
             addMarkers(
               lng = ~longitudes, 
               lat = ~latitudes, 
               group = "Schools", 
               clusterOptions = markerClusterOptions(),
               popup = paste0(
                 "Name: ", wards$school_name, "<br/>", "Status: ", wards$school_type, "<br/>",
                 "Registration status: ", wards$registration_status,"<br/>", "Ward: ", wards$wards_new
               )
             )
 
 return(list(secschools_distribution_plot,secschool.dist.gender.plot,secschool.dist.ownership.plot,secschool.dist.boarding.plot,secschool.dist.boarding.map))
}