crop_management_visualizations <- function(separate_stagecol,crop_mngtdata){
  
  # crop losses and causes----
  # Crop loss rating----
  
  lossrating_crops_makunei <- separate_stagecol %>% filter(grepl("lossrating", measurement)) %>%
    dplyr::filter(value !="NA") %>% 
    dplyr::select(stage,measurement,crop,value) %>% group_by(stage,crop,value,measurement) %>%
    count(value) %>% ungroup() %>% as.data.frame()
  
  ratingcrops <-   ggplot(lossrating_crops_makunei,aes(x = crop, y = n, fill = value)) + 
    geom_col(position = "stack") +
    ggtitle("Loss Rating") +
    #coord_polar(theta = "y") +
    ylab('Number of households') +
    xlab("Crops")+
    labs(fill = "Rating") +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 0.9)) +
    scale_fill_manual(values = c("#8B3626", "#483D8B", "#8EE5EE")) +
    scale_y_continuous(limits = c(0,100)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~stage)
  
  # Subcounty loss rating----
  
  lossdata_makueni <- separate_stagecol %>% filter(grepl("lossrating", measurement)) %>%
    dplyr::filter(value !="NA" & county == "Makueni") %>% dplyr::select(stage,measurement,subcounty,value) %>% group_by(subcounty,value,measurement) %>%
    count(value) %>% ungroup() %>% as.data.frame()
  
  subcounty_loss_rating <-  ggplot(lossdata_makueni,aes(x = measurement, y = n, fill = value)) + 
                            geom_col(position = "stack") +
                            ggtitle("Loss Rating") +
                            ylab('Number of households') +
                            xlab("Stage")+
                            scale_fill_manual(name = "Rating",values = c("#CDC9A5", "#00CDCD", "#008B00")) +
                            scale_x_discrete(labels = c("dryinglossrating" = "Drying", 
                                                        "shellthreshlossrating" = "Shellthresh",
                                                        "storagelossrating" = "Storage",
                                                        "transportationlossrating" = "Transportation")) +
                            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 0.9)) +
    
                            facet_wrap(~subcounty)
  
  # Subcounty loss causes----
  losscauses_makueni <- separate_stagecol %>% filter( value != "NA" & value != "7") %>% 
    filter(grepl("losscauses", measurement)) %>% group_by(value,subcounty,measurement,stage)%>% count(value) #%>% 
  
  losscauses_subcounty <- ggplot(losscauses_makueni,aes(y = value, x = n)) + geom_col() + 
    xlab("Number of households")+
    ylab("Causes of Loss") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 0.9),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Loss causes") +
    facet_wrap(~subcounty)
  
  # Stage loss causes----
  
  stages <- c("drying" = "Drying stage",
              "shellthresh" = "Shellthreshing stage",
              "storage" = "Storage stage",
              "transportation" = "Transportation stage")
  
  losscauses_stage <- ggplot(losscauses_makueni,aes(y = value, x = n)) + geom_col() + 
    ylab("Number of households")+
    xlab("Causes of Loss") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 0.9),
          plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("Loss causes") +
    facet_wrap(~stage, labeller = as_labeller(stages))
  
  
  # Storage methods----
  
  sankey_storage_makueni <- crop_mngtdata %>% dplyr::select(subcounty, storage_cropproducesstoragemethod) 
  
  sankey_storage_makueni <- sankey_storage_makueni %>% 
    separate_rows(storage_cropproducesstoragemethod,sep = ",") %>% 
    dplyr::filter(storage_cropproducesstoragemethod != "0" )
  
  sankey_storage_makueni <- sankey_storage_makueni %>% dplyr::group_by(storage_cropproducesstoragemethod) %>% mutate(count = n()) %>% unique() %>% dplyr::ungroup()

  colnames(sankey_storage_makueni) <- c("source", "target", "value")
  
  sankey_storage_makueni$source <- stri_trim_both(sankey_storage_makueni$source)
  sankey_storage_makueni$target <- stri_trim_both(sankey_storage_makueni$target)
  
  sankey_storage_makueni$target <- paste(sankey_storage_makueni$target, " ", sep = "")
  
  nodes <- data.frame(name=c(as.character(sankey_storage_makueni$source), as.character(sankey_storage_makueni$target)) %>% unique())
  
  sankey_storage_makueni$IDsource=match(sankey_storage_makueni$source, nodes$name)-1 
  sankey_storage_makueni$IDtarget=match(sankey_storage_makueni$target, nodes$name)-1
  
  
  storage_methods <- plot_ly(
    type = "sankey",
    
    node = list(
      label = nodes$name,
      color = c("#76EEC6", "#483D8B", "#6495ED", "#008B00", "#CD8162", "#9ACD32", "#FFC0CB", "#97FFFF", "#C1FFC1", "#A52A2A", "#0000EE", "#FF6A6A", "#1C86EE", "#B23AEE", "#48D1CC"),
      pad = 15,
      thickness = 15,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = sankey_storage_makueni$IDsource,
      target = sankey_storage_makueni$IDtarget,
      value =  sankey_storage_makueni$value,
      label = nodes$name
    )
  ) 
  
  storage_methods <- storage_methods %>% layout(
    title = "Storage Methods",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )
  
  # Protection methods-----
  
  sankey_protection <- crop_mngtdata %>% dplyr::select(subcounty, storage_protectionmethod)
  
  sankey_protection <- sankey_protection %>% dplyr::filter(storage_protectionmethod != "NA") %>%  
    dplyr::group_by(storage_protectionmethod) %>% mutate(count = n()) %>% unique() %>% dplyr::ungroup()
  
  
  colnames(sankey_protection) <- c("source", "target", "value")
  
  sankey_protection$source <- stri_trim_both(sankey_protection$source)
  sankey_protection$target <- stri_trim_both(sankey_protection$target)
  
  sankey_protection$target <- paste(sankey_protection$target, " ", sep = "")
  
  nodes <- data.frame(name=c(as.character(sankey_protection$source), as.character(sankey_protection$target)) %>% unique())
  
  sankey_protection$IDsource=match(sankey_protection$source, nodes$name)-1 
  sankey_protection$IDtarget=match(sankey_protection$target, nodes$name)-1
  
  
  protection_methods <- plot_ly(
    type = "sankey",
    
    node = list(
      label = nodes$name,
      color = c("#76EEC6", "#483D8B", "#6495ED", "#008B00", "#CD8162", "#9ACD32", "#FFC0CB", "#97FFFF", "#C1FFC1", "#A52A2A", "#0000EE", "#FF6A6A", "#1C86EE", "#B23AEE", "#48D1CC"),
      pad = 15,
      thickness = 15,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = sankey_protection$IDsource,
      target = sankey_protection$IDtarget,
      value =  sankey_protection$value,
      label = nodes$name
    )
  ) 
  
  protection_methods <- protection_methods %>% layout(
    title = "Protection Methods",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )
  
  return(list(ratingcrops,subcounty_loss_rating,losscauses_subcounty,losscauses_stage,storage_methods,protection_methods))
}