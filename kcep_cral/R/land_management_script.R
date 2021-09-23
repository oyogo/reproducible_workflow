land_management_visualizations <- function(nresource_clean,management_techniques){
  
  nresource_clean <- data.table::setDT(nresource_clean)
  management_techniques <- data.table::setDT(management_techniques)
  
  # Land management - crop technology-----
  ## on shiny (reactive environment) Type == "cropmngt" is replace with Type == input$technologies for choosing other technologies from options given.
  cropmngt_pcent <- nresource_clean[ c( Type == "cropmngt" & value != "NA" & measure == "apply"), .(count=.N), by=.(subcounty,value)]
  crop_mngt <- plot_ly(cropmngt_pcent,
                          x = ~ subcounty,
                          y = ~ count,
                          color = ~ value,
                          colors = "Paired",
                          type = "bar",
                          mode = "none",
                          showlegend = TRUE,
                          text = ~paste0("Applied technology:  ",value,  "\n","Number of households: ",count, "\n","Subcounty: ",subcounty),
                          hoverinfo="text"
                          ) %>%
                            layout(legend = list(title = list(text = "<b>Technology application status</b>"), orientation = "h"),
                                   xaxis = list(title = "Subcounty"),
                                   yaxis = list(title = "Number of households")
                            )
  
   
  
  # Land proportions upon which the technology was used/applied-----
  
  nresource_clean$value <- as.numeric(nresource_clean$value)
  
  tmap_makueni <- nresource_clean[c( Type == "cropmngt" & measure == "areaunder" &  value != "NA"),
                                        .(value=sum(as.numeric(value))), by=.(subcounty)]
  
  land_proportion <- treemap(
                        tmap_makueni,
                        index = "subcounty",
                        vSize = "value",
                        type = "index",
                        title = "Technology application on land portions accross different Subcounties"
                      )
  
  
  # land management techniques-----
  
  management_techniques <- management_techniques %>%
                            dplyr::filter(Type == "cropmngt") %>%
                            dplyr::group_by(subcounty) %>%
                            dplyr::mutate(per=paste0(round(100*n/sum(n,0)))) %>% dplyr::ungroup() %>%
                            unique() %>% as.data.frame()
  
  management_techniques$per <- as.integer(management_techniques$per)
  mngt_techniques <- ggplot(management_techniques,aes(y = per, x = value, fill = per)) +
                        geom_col() +
                        theme_bw() +
                        theme(
                          plot.title = element_text(hjust = 0.5),
                          text = element_text(size = 16, colour = "black"),
                          legend.position = "none") +
                    
                        scale_y_continuous(
                    
                          limits = c(0, 100),
                          breaks = seq(0, 100, 10)) +
                        labs(title = "Technology management methods used", y = "Percentage household count", x = "Crop management method")+
                    
                        coord_flip() +
                        facet_wrap( ~ subcounty)
  
  return(list(crop_mngt,land_proportion,mngt_techniques))
  
}