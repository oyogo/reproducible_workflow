pschools_distribution_visualizations <- function(primary_schools_data,wards = wards_shapefile){
  
  ## all schools
  allschools_data <- primary_schools_data %>% dplyr::group_by(wards_new)
  
  allschools_data  %>% dplyr::count() 
  allschools_data <-  data.table::setDT(allschools_data)[,round(100*.N/nrow(allschools_data),0), by=c("wards_new")]
  
  colnames(allschools_data)[2] <- "percentage"
  
  
 allschools_plot <-  plot_ly(allschools_data,
          y = ~percentage,
          x = ~wards_new, 
          color = ~percentage ,
          showscale = FALSE,
          colors = 'YlOrBr',
          text = ~paste0("Ward: ", wards_new, "\n", "Percentage number of schools: ", percentage)) %>%
    add_bars(showlegend=FALSE, hoverinfo='text') %>%
    hide_colorbar() %>%
    layout(bargap = 0.5, group = ~percentage ,
           title =  "All schools in 2019", 
           xaxis = list(title = "Ward", tickangle = 45),
           yaxis = list(title= "Percentage number of schools",ticksuffix = "%"),
           autotick = FALSE,
           tick0 = 0,
           dtick = 1,
           tickmode = "linear"
    ) %>%
    layout(width=3, 
           margin = list( b = 200),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")
 
 # convert to datatable
 primary_schools_data <- data.table::setDT(primary_schools_data)
 
 ## school distribution according to gender
 ## Only boys schools are selected however in shiny there is a select button for choosing either boys, girls or mixed type of schools.
 
 primaryschools_gender  <-  primary_schools_data[c(wards_new !="" & type_gender=="boys"),.(count=.N), by=c("type_gender","wards_new")]
 
 ## The commented lines below can only be run in a reactive context - shiny 
 # validate(
 #   need(nrow(primaryschools_gender) > 1,
 #        "There are no schools for this category"))
 
 gender_grouping_plot <- plot_ly(primaryschools_gender,
         y = ~count, 
         x = ~wards_new, 
         color = ~count , 
         showscale = FALSE,
         colors = 'YlOrBr',
         text = ~paste0("Ward: ", wards_new, "\n", "Number of schools: ", count)) %>%
   add_bars(showlegend=FALSE, hoverinfo='text') %>%
   hide_colorbar() %>%
   layout(bargap = 0.5, group = ~count ,
          
          title = paste0("boys"," schools as of 2019"), 
          xaxis = list(title = "Ward", tickangle = 45),
          yaxis = list(title= "Number of schools"),
          autotick = FALSE,
          tick0 = 0,
          dtick = 1,
          tickmode = "linear",
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)"
   ) 
 
 
 ## School distrubution according to school type (private, public)
 ## Only public schools are selected given that we're not working in a reactive context. On the shiny there are selection buttons.
 
 primaryschools_type1 <- primary_schools_data[c(wards_new !="" & type_priv_pub=="public"),.(count=.N), by=c("type_priv_pub","wards_new")] # on shiny use input$primary_category_select in place of "public"
 
 category_grouping_plot <- plot_ly(primaryschools_type1, 
                                   y = ~count, 
                                   x = ~wards_new, 
                                   color = ~count , 
                                   showscale = FALSE,
                                   colors = 'Oranges',
                                   text = ~paste0("Ward: ", wards_new, "\n", "Number of schools: ", count)) %>%
   add_bars(showlegend=FALSE, hoverinfo='text' ) %>%
   hide_colorbar() %>%
   layout(bargap = 0.5, 
          group = ~count,
          title = paste0("public"," schools as of 2019"), # on shiny use input$primary_category_select in place of "public"
          xaxis = list(title = "Ward",tickangle = 45),
          yaxis = list(title= "Number of schools",tick0 = 0,dtick = 2,tickmode = "linear"),
          
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)" 
   )
 
 ## School distribution according to boarding facility
 ## Only day schools are selected. on Shiny there is a selection button for choosing between day and boarding schools. 
 
 primaryschools_type2 <-  primary_schools_data[c(wards_new !="" & type_day_boarding=="day"),.(count=.N), by=c("type_day_boarding","wards_new")]
 
 schooltype_grouping_plot <- plot_ly(primaryschools_type2, 
                                     y = ~count, 
                                     x = ~wards_new, 
                                     color = ~count , 
                                     showscale = FALSE,
                                     colors = 'BuPu',
                                     text = ~paste0("Ward: ", wards_new, "\n", "Percentage number of schools: ", count) ) %>%
   add_bars(showlegend=FALSE, hoverinfo='text') %>%
   hide_colorbar() %>%
   layout(bargap = 0.5, group = ~count ,
          
          title = paste0("day"," schools as of 2019"), # on shiny use input$primary_type_select in place of "day"
          xaxis = list(title = "Ward",tickangle = 45),
          yaxis = list(title= "Number of schools"),
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)") 
 
 
 ## schoold facility distribution leaflet map----
 ## On shiny, the map updates in response to the selection from the filters
 
 schools_distribution_map <- leaflet(data=primary_schools_data) %>%
   addTiles(group = "OSM") %>%
   setView(lng = 37.5, lat = -2.3, zoom = 9) %>% 
   addPolygons(data = wards, opacity = 1, fill = FALSE,weight = 2, group = "Subcounty") %>%
   addMarkers(data = primary_schools_data, lng = primary_schools_data$longitudes, lat = primary_schools_data$latitudes, group = "Schools", clusterOptions = markerClusterOptions(),
              popup = paste0("Name: ",primary_schools_data$school_name, "<br/>",
                             "Status: ",primary_schools_data$type_gender, "<br/>",
                             "Registration status: ", primary_schools_data$registration_status,"<br/>",
                             "Ward: ",primary_schools_data$wards_new))
 
 return(list(allschools_plot,gender_grouping_plot,category_grouping_plot,schooltype_grouping_plot,schools_distribution_map))
  
}