
sschools_perfomance_visualizations <- function(edep_sschools_cleaned){
  
  
  edep_sschools_cleaned <- data.table::copy(edep_sschools_cleaned)
  # school performance gender-wise----
  sec.perf.gender.data <- data.table::setDT(edep_sschools_cleaned)[,.(percentage=round(100*.N/nrow(edep_sschools_cleaned),0)), by=c("school_type","mean_category")] 
  
 sec.perf.gender.plot <- plot_ly(sec.perf.gender.data, 
          y = ~percentage,
          x = ~mean_category, 
          color = ~school_type , 
          # showlegend=TRUE, 
          colors = c("#00EEEE", "#8B7D6B", "#0000EE"),
          text = ~paste0("Mean grade: ", mean_category, "\n", "Percentage number of schools: ", percentage, "\n", "School gender: ", school_type)) %>%
    add_bars( hoverinfo='text'
              #colors = c("#458B74", "#6495ED", "#8B7355", "#483D8B", "#00EE00", "#FF69B4")
    ) %>%
    layout(bargap = 0.5, group = ~percentage , autosize = T,
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           title = "School performance per gender in 2019", 
           xaxis = list(title = "Performance (mean points)",
                        categoryorder = "array",
                        categoryarray = c("[1-2]","[2-3]","[3-4]","[4-5]","[5-6]","[6-7]","[7-8]","[8-9]","[9-10]","[10-11]","[11-12]")),
           yaxis = list(title = "Percentage number of schools", ticksuffix = "%")) 
 
 # school performance according to boarding facility----
 
 sec.perf.boarding.data  <- edep_sschools_cleaned[,.(percentage=round(100*.N/nrow(edep_sschools_cleaned),0)), by=c("type_day_boarding","mean_category")]
 sec.perf.boarding.plot <- plot_ly(sec.perf.boarding.data, 
           y = ~percentage, 
           x = ~mean_category, 
           color = ~type_day_boarding , 
           #showlegend=TRUE, 
           colors = c("#68228B", "#EE7AE9", "#00CD00"),
           text = ~paste0("Mean grade: ", mean_category, "\n", "Percentage number of schools: ", percentage, "\n", "School category: ", type_day_boarding)) %>%
   add_bars( hoverinfo='text') %>%
   layout(bargap = 0.5, group = ~type_day_boarding , autosize = T,
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          title = "School performance per boarding facilities in 2019", 
          xaxis = list(title = "Performance (mean points)",
                       categoryorder = "array",
                       categoryarray = c("[1-2]","[2-3]","[3-4]","[4-5]","[5-6]","[6-7]","[7-8]","[8-9]","[9-10]","[10-11]","[11-12]")),
          yaxis = list(title = "Percentage number of schools", ticksuffix = "%"))
 
 # school performance according to ownership----
 
 sec.perf.ownership.data <-  edep_sschools_cleaned[,.(percentage=round(100*.N/nrow(edep_sschools_cleaned),0)), by=c("type_priv_pub","mean_category")] 
 
 sec.perf.ownership.plot <- plot_ly(sec.perf.ownership.data, 
                   y = ~percentage, 
                   x = ~mean_category, 
                   color = ~type_priv_pub , 
                   #showlegend=TRUE, 
                   colors = c("#7CFC00", "#E066FF", "#CDCD00", "#CD3278", "#00F5FF", "#8B6914"),
                   text = ~paste0("Mean grade: ", mean_category, "\n", "Percentage number of schools: ", percentage, "\n", "School type: ", type_priv_pub)) %>%
             add_bars(  hoverinfo='text') %>%
             layout(bargap = 0.5, group = ~type_priv_pub , autosize = T,
                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)",
                    title = "School performance per school type (public,private..) in 2019", 
                    xaxis = list(title = "Performance (mean points)",
                                 categoryorder = "array",
                                 categoryarray = c("[1-2]","[2-3]","[3-4]","[4-5]","[5-6]","[6-7]","[7-8]","[8-9]","[9-10]","[10-11]","[11-12]")
                    ),
                    yaxis = list(title= "Percentage number of schools", ticksuffix = "%"))
 
 # school performance per ward. Note that this graph uses a reactive slider on shiny 
 sec.perf.ward.data <- edep_sschools_cleaned[edep_sschools_cleaned$kcse_sec >= 2 & edep_sschools_cleaned$kcse_sec <= 8, # On shiny app, replace 2 and 8 with input$meancategory[1] and input$meancategory[2] respectively.
                                             .(percentage=round(100*.N/nrow(edep_sschools_cleaned),0)), by=c("wards_new")]
 
 sec.perf.ward.plot <- plot_ly(sec.perf.ward.data , 
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
                 xaxis = list(title = "Ward",tickangle = 45),
                 yaxis=list(title="Percentage number of schools", ticksuffix = "%"),
                 #yaxis = list(ticksuffix = "%"),
                 dtick = 1,
                 tick0 = 0,
                 tickmode = "array")
 
 return(list(sec.perf.gender.plot,sec.perf.boarding.plot,sec.perf.ownership.plot,sec.perf.ward.plot))
}