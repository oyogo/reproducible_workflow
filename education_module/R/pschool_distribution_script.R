pschools_distribution_visualizations <- function(primary_schools_data){
  
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
    
    #hovertext = ~paste("Ward: ", gender_data$wards_new, '<br> No of schools: ', n, '<br> Type of school: ', gender_data$school_type),hoverinfo='text') %>%
    add_bars(showlegend=FALSE, hoverinfo='text') %>%
    hide_colorbar() %>%
    layout(bargap = 0.5, group = ~percentage ,
           
           #legend = list(title = list(text = "<b>Percentage</b>")),
           title =  "All schools in 2019", 
           xaxis = list(title = "Ward", tickangle = 45
                        # categoryorder = "array",
                        # categoryarray = ~n
           ),
           yaxis = list(title= "Percentage number of schools",ticksuffix = "%"),
           autotick = FALSE,
           tick0 = 0,
           dtick = 1,
           tickmode = "linear"
    ) %>%
    layout(width=3, 
           margin = list( b = 200), 
           #yaxis = list(tickformat = "%"),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")
 
 return(allschools_plot)
  
}