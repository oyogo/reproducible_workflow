marketing_visualizations <- function(crops_sold){
  
  crops_sold <- data.table::setDT(crops_sold)
  
  # market losses-----
  ## market loss rating----
  
  marketlossrating_makueni <- crops_sold[marketinglossrating != "NA",.(count=.N),by=.(subcounty,crop,marketinglossrating)] 
  
  mkt_loss <-  ggplot(marketlossrating_makueni,
                     aes(
                       x = crop,
                       y = count,
                       fill = marketinglossrating
                     )) +
                geom_col() +
                theme_bw() +
                theme(
                  strip.placement = "outside",
                  plot.title = element_text(hjust = 0.5),
                  text = element_text(size = 16)
                ) +
                ggtitle("Marketing loss rating") +
                scale_fill_manual(
                  values = c("#8B6508", "#008B8B", "#00CD00"),
                  labels = c(
                    "High (above 30%)" ,
                    "Medium (10-20%)",
                    "Low (below 10%)"
                  )
                ) +
                labs(fill = "Rating", y = "Household count") +
                coord_flip() +
                facet_wrap( ~ subcounty)
  
  ## market loss causes-----
  
  marketingloss_causes_makueni <- crops_sold[marketinglosscauses != "NA",.(count=.N), by=.(subcounty,marketinglosscauses,marketinglossrating)]
  
  mkt_loss_causes <-  ggplot(
                          marketingloss_causes_makueni,
                          aes(
                            x = marketinglosscauses,
                            y = count,
                            fill = marketinglossrating
                          )
                        ) +
                          geom_col() +
                          coord_flip() +
                          theme_bw() +
                          scale_fill_manual(
                            values = c("#8B6508", "#008B8B", "#00CD00"),
                            labels = c(
                              "High (above 30%)" ,
                              "Medium (10-20%)",
                              "Low (below 10%)"
                            )
                          ) +
                          labs(
                            fill = "Rating",
                            y = "Household count",
                            x = "Marketing loss causes",
                            title = "Marketing loss causes"
                          ) +
                          theme(
                            plot.title = element_text(hjust = 0.5),
                            strip.placement = "outside",
                            text = element_text(size = 16)
                          ) +
                          facet_wrap( ~ subcounty)
 
 # Marketing challenges----
 ## percentage households facing marketing challenges---
 
 mchallenge_perc <- crops_sold[,.(hh_count=.N, lst12mnthsfacemarketingchallenge = lst12mnthsfacemarketingchallenge),
                               by=.(subcounty)][,.(per=round(.N/hh_count * 100,0)), 
                                                by=.(subcounty, lst12mnthsfacemarketingchallenge)][lst12mnthsfacemarketingchallenge == "Yes",]
 
 mkt_challenge <-  mchallenge_perc[,.(per=per),by=c("subcounty")] %>%
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
                       title="Percentage number of households that faced marketing challenges",
                       xaxis = list(
                         title = "Subcount "
                       ),
                       yaxis = list(
                         title = "Percentage households", ticksuffix = "%"
                       )
                     ) %>%
                     config(displayModeBar = FALSE, displaylogo = FALSE, 
                            scrollZoom = FALSE, showAxisDragHandles = TRUE, 
                            showSendToCloud = FALSE)
 
 ## which market challenge did they face----
     crops_sold <- crops_sold %>% dplyr::select(subcounty, marketingchallenges) %>%
       tidyr::separate_rows(marketingchallenges, sep = ",")
     
     which_challenge_makueni <- data.table::setDT(crops_sold)[marketingchallenges != "NA" & marketingchallenges != "0",.(count=.N ), 
                                                              by=.(subcounty, marketingchallenges)]
     
     
   which_challenge <- ggplot(which_challenge_makueni,
                              aes(x = marketingchallenges, y = count)) +
                         geom_col() +
                         theme_bw() +
                         ggtitle("Marketing challenges") +
                         labs(y = "Frequency", x = "Challenge") +
                         theme(strip.placement = "outside",
                               plot.title = element_text(hjust = 0.5)) +
                         coord_flip() +
                         facet_wrap( ~ subcounty)
 
 return(list(mkt_loss,mkt_loss_causes,mkt_challenge,which_challenge))
  
}