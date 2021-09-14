pschools_performance_visualizations <- function(primary_schools_data){
  
  primary_schools_data <- data.table::setDT(primary_schools_data)
  
  # school performance according to gender----
  pry_perf_gender_data <- primary_schools_data[,.(percentage=round(100*.N/nrow(primary_schools_data),0)), by=c("type_gender","average_marks")] 
  
  pry_perf_gender_plot <- plot_ly(pry_perf_gender_data, 
                                      y = ~percentage,
                                      x = ~average_marks, 
                                      color = ~type_gender , 
                                      showlegend = TRUE, 
                                      colors = "#8B7D6B",
                                      text = ~paste0("Average marks: ", average_marks, "\n", "Percentage number of schools: ", percentage, "\n", "School gender: ", type_gender)) %>%
    add_bars( hoverinfo='text' ) %>%
    layout(bargap = 0.5, group = ~percentage ,
           title = "Primary schools performance by school gender in 2019", 
           xaxis = list(title = "Average marks",
                        categoryorder = "array"
                        
           ),
           yaxis = list(title = "Percentage number of schools", ticksuffix = "%"),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)") 
  
  # school performance according to boarding facility----
  
  pry_perf_ifboarding  <- primary_schools_data[,.(percentage=round(100*.N/nrow(primary_schools_data),0)), 
                                                                 by=c("type_day_boarding","average_marks")]
  
  pry_perf_ifboarding_plot <- plot_ly( pry_perf_ifboarding, 
                                           y = ~percentage, 
                                           x = ~average_marks, 
                                           color = ~type_day_boarding , 
                                           #showlegend=TRUE, 
                                           colors = c("#00FFFF", "#F08080", "#FFFF00"),
                                           text = ~paste0("Average marks: ", average_marks, "\n", "Percentage number of schools: ", percentage, "\n", "School category: ", type_day_boarding)) %>%
                                     add_bars( hoverinfo='text') %>%
                                     layout(bargap = 0.5, group = ~type_day_boarding , 
                                       title = "Day and boarding schools performance in 2019", 
                                       xaxis = list(title = "Average marks",categoryorder = "array"),
                                       margin = list(l = 50),
                                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                       paper_bgcolor = "rgba(0, 0, 0, 0)",
                                       yaxis = list(title = "Percentage number of schools", ticksuffix = "%")) 
  
  # school performance according to school ownership (public,private)----
  
  pry_perf_ownership_data <-  primary_schools_data[,.(percentage=round(100*.N/nrow(primary_schools_data),0)), by=c("type_priv_pub","average_marks")]
  
  pry_perf_ownership_plot <- plot_ly(pry_perf_ownership_data, 
                                     y = ~percentage, 
                                     x = ~average_marks, 
                                     color = ~type_priv_pub ,
                                     colors = c("#7CFC00", "#E066FF"),
                                     text = ~paste0("Average marks: ", average_marks, "\n", "Percentage number of schools: ", percentage, "\n", "School type: ", type_priv_pub)) %>%
                              add_bars(  hoverinfo='text') %>%
                              layout(bargap = 0.5, group = ~type_priv_pub , 
                                     title = "Public and private schools performance in 2019", 
                                     xaxis = list(title = "Average marks", categoryorder = "array"),
                                     yaxis = list(title= "Percentage number of schools", ticksuffix = "%"),
                                     width = 3,
                                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                     paper_bgcolor = "rgba(0, 0, 0, 0)") 
  
  # school performance analysis by ward-----
  # note that on shiny the graph reacts to the selection of range marks from slider. Can't be done here seeing that its not a reactive context.
  
  primary_perf_ward_data <- primary_schools_data[c(wards_new !="" & primary_schools_data$kcpe_primary >= 100 & primary_schools_data$kcpe_primary <= 300),
                                                 .(percentage=round(100*.N/nrow(primary_schools_data),0)), by=c("wards_new")] 
  
  pry_perf_wards_plot <- plot_ly(primary_perf_ward_data, 
                                 y = ~percentage, 
                                 x = ~wards_new, 
                                 color = ~percentage, 
                                 colors = 'PuRd',
                                 showscale = FALSE,
                                 text = ~paste0("Ward: ", wards_new, "\n", "Percentage number of schools: ", percentage)) %>%
                          add_bars( hoverinfo='text') %>%
                          hide_colorbar() %>%
                          layout(
                            group = ~wards_new , 
                            plot_bgcolor  = "rgba(0, 0, 0, 0)",
                            paper_bgcolor = "rgba(0, 0, 0, 0)",
                            title = paste0("School performance per ward for range of marks ", 100 ," - ",300," in 2019"), #On shiny, replace 100 with input$primary_averagemarks[1] and 300 with input$primary_averagemarks[2]
                            xaxis = list(title = "Ward",tickangle = 45),
                            yaxis=list(title="Percentage number of schools", ticksuffix = "%"))
  
  return(list(pry_perf_gender_plot,pry_perf_ifboarding_plot,pry_perf_ownership_plot,pry_perf_wards_plot))
}