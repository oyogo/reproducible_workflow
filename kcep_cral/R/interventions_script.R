interventions_visualizations <- function(cropprod_data){
  
  
  # Crop production----
  cropprod_data <- data.table::setDT(cropprod_data)
  
  ## Crop production training status
  trained_cropprod_makueni <-  cropprod_data[,.(hh_count=.N, cropProduction_training = cropProduction_training),
                                             by=.(subcounty)][cropProduction_training == "Yes",.(per=round(.N/hh_count*100,0)), 
                                                              by=.(subcounty,cropProduction_training)]
  
 
  
  trained <-  trained_cropprod_makueni %>%
              plot_ly() %>%
              add_trace(x = ~reorder(subcounty, per), 
                        y = ~per,
                        size = ~per,
                        color = ~subcounty,
                        alpha = 1.5,
                        #sizes = c(200,4000),
                        type = "scatter",
                        mode = "markers",
                        marker = list(symbol = 'circle', sizemode = 'diameter',
                                      line = list(width = 2, color = '#FFFFFF'), opacity=0.4)) %>%
              add_text(x = ~reorder(subcounty, -per), 
                       y = ~subcounty, text = ~per,
                       showarrow = FALSE,
                       color = I("black")) %>%
              layout(
                showlegend = FALSE,
                title="Crop production training",
                xaxis = list(
                  title = "Subcounty"
                ),
                yaxis = list(
                  title = "Percentage households", ticksuffix = "%"
                )
              ) %>%
              config(displayModeBar = FALSE, displaylogo = FALSE, 
                     scrollZoom = FALSE, showAxisDragHandles = TRUE, 
                     showSendToCloud = FALSE)
 
  # Household member trained-----
 
  hhmember_trained_makueni <- cropprod_data %>% tidyr::separate_rows(HHmember_trainedcrop, sep = ",")
 
 
  hhmember_trained_makueni <- data.table::setDT(hhmember_trained_makueni)[HHmember_trainedcrop != "NA", 
                                                              .(count=.N, HHmember_trainedcrop=stringr::str_trim(HHmember_trainedcrop)),
                                                              by=.(subcounty)]
 
  hhmember_trained <-  ggplot(hhmember_trained_makueni,
                            aes(x = HHmember_trainedcrop, y = count)) +
                       geom_col() +
                       theme_bw() +
                       ggtitle("Crop production trainee") +
                       labs(y = "Count", x = "Household member trained") + #, caption = "Count is the number of times the trainee was mentioned in the household responses."
                       theme(
                         axis.text.x = element_text(
                           angle = 65,
                           hjust = 1,
                           vjust = 0.9
                         ),
                         strip.placement = "outside",
                         plot.title = element_text(hjust = 0.5),
                         text = element_text(size = 16)
                       ) +
                       facet_wrap( ~ subcounty)
  
  
  # Trainer----
  
  cprod_trainer_makueni <- cropprod_data[who_trainedcrop != "NA",.(count=.N),by=.(subcounty, who_trainedcrop)]
  
  trainer <-  cprod_trainer_makueni %>%
                  ggplot(aes(y = count, x = who_trainedcrop)) +
                  geom_col() +
                  coord_flip() +
                  theme_bw() +
                  labs(y = "Count", x = "Trainer", caption = "Count is the number of times the trainer was mentioned in the household responses.") +
                  ggtitle("Crop production trainer") +
                  theme(
                    axis.text.x = element_text(
                      angle = 75,
                      hjust = 0.9,
                      vjust = 0.9
                    ),
                    strip.placement = "outside",
                    plot.title = element_text(hjust = 0.5),
                    text = element_text(size = 16)
                  ) +
                  facet_wrap( ~ subcounty)
  
  # Training area----
  
  cprod_training <- cropprod_data %>% tidyr::gather(areaTrainedcrop, who_trainedcrop,key = "trainer_area", value = "value") %>%
    tidyr::separate_rows(value, sep = ",") #%>%
  
  cprod_training$value <- str_trim(cprod_training$value)
  
  cprod_training_makueni <- data.table::setDT(cprod_training)[trainer_area == "areaTrainedcrop" & value != "NA",
                                                                       .(count=.N),by=.(subcounty, trainer_area, value)]
  
  
 training_area<-  cprod_training_makueni %>%
    ggplot(aes(y = count, x = value)) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    labs(y = "Count", x = "Training area", caption = "Count is the number of times the training area was mentioned in the household responses.") +
    ggtitle("Crop production training across surveyed Subcounties") +
    theme(
      axis.text.x = element_text(
        angle = 75,
        hjust = 0.9,
        vjust = 0.9
      ),
      strip.placement = "outside",
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 14)
    ) +
    facet_wrap( ~ subcounty)
  
 
 # Post harvest----
 
 # Market access----
 
  return(list(trained,hhmember_trained,trainer,training_area))
}