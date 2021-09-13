
pschools_enrolment_visualizations <- function(primary_long_split,edepartment_data){
  
  # enrolment trends from statistical abstract----
  enrolments_county <- primary_long_split[,c(sum_enrolment=sum(enrolment)),by=c("year","subcounty")] %>%
    plot_ly(
      x = ~subcounty, 
      y = ~V1, 
      size = ~V1,  
      frame = ~year, 
      text = ~subcounty, 
      hoverinfo = "text",
      type = 'bar',
      #mode = 'markers',
      showlegend=FALSE
    ) %>%
    layout( title = "Primary school enrolments per County",
            yaxis=list(title="Number of students"),
            xaxis=list(title="Subcounty")
    )
  
  
  # enrolments according to gender from data obtained from Education department Makueni County----
  
  enrollment_data <- edepartment_data %>% dplyr::group_by(wards_new) %>% 
    dplyr::summarise(boys=sum(enrol_boys),girls=sum(enrol_girls),total=sum(enrol_total))
  
  enrollment_sankey_data <- enrollment_data %>% dplyr::select(-total) %>% tidyr::pivot_longer(cols = c(boys,girls), names_to = "gender", values_to = "count")
  
  
  colnames(enrollment_sankey_data) <- c("source", "target", "value")
  
  enrollment_sankey_data$source <- stringr::str_trim(enrollment_sankey_data$source)
  enrollment_sankey_data$target <- stringr::str_trim(enrollment_sankey_data$target)
  
  enrollment_sankey_data$target <- paste(enrollment_sankey_data$target, " ", sep = "")
  
  nodes <- data.frame(name=c(as.character(enrollment_sankey_data$source), as.character(enrollment_sankey_data$target)) %>% unique())
  
  enrollment_sankey_data$IDsource=match(enrollment_sankey_data$source, nodes$name)-1
  enrollment_sankey_data$IDtarget=match(enrollment_sankey_data$target, nodes$name)-1
  
  
  enrolments_gender <- plotly::plot_ly(
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
      source = enrollment_sankey_data$IDsource,
      target = enrollment_sankey_data$IDtarget,
      value =  enrollment_sankey_data$value,
      label = nodes$name
    )
  )
  
  enrolments_gender <- enrolments_gender %>% plotly::layout(
    title = "Enrollment",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )
  
  enrolments_gender
  
  
  return(list(enrolments_county,enrolments_gender))
}