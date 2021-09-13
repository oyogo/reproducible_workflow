

sschools_perfomance_visualizations <- function(edep_sschools_cleaned){
  
  perf_gender_data <- data.table::setDT(edep_sschools_cleaned)[,.(percentage=round(100*.N/nrow(edep_sschools_cleaned),0)), by=c("school_type","mean_category")] 
  
  
  plot_ly(perf_gender_data, 
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
}