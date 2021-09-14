
pschools_enrolment_visualizations <- function(pryschool_abstract_data,edepartment_data,wards = wards_shapefile){
  
  # enrolment trends from statistical abstract----
  pryschool_abstract_data <- data.table::copy(pryschool_abstract_data)
  pryschool.trends <- pryschool_abstract_data[,c(sum_enrolment=sum(enrolment)),by=c("year","sex")]
  enrolment.trends <- ggplot(pryschool.trends, aes(x=year,y=V1,group=sex, color= sex)) +
    geom_line(size=2) +
    labs(title="Primary school enrolments trend",y="Number of students",group="Gender") +
    scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
    theme(plot.title=element_text(hjust=0.5, size=16, color="white"),
          axis.text=element_text(color="white", face="bold",size=14),
          axis.title=element_text(size=14),
          panel.background = element_rect(fill = "#00868B"), 
          plot.background = element_rect(fill = "#00868B"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) 
  
  
  
  enrolments_county <- pryschool_abstract_data[,c(sum_enrolment=sum(enrolment)),by=c("year","subcounty")] %>%
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
    layout( title = "Primary school enrolments per SubCounty",
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
  
  # enrolments per ward
  #primary_enrollment_sankey <- data.table::copy(enrollment_sankey_data) 
  
  primary_enrollment_sankey <- data.table::melt(
    data = as.data.table(enrollment_data),
    id.vars = "wards_new",
    measure.vars = c("boys","girls"), 
    variable.name = "gender", 
    .name = "count"
  )
  colnames(primary_enrollment_sankey)[3] <- "enrollments"
  
 pry.enrol.ward <- primary_enrollment_sankey%>%
    plot_ly( y = ~enrollments, x = ~reorder(wards_new,enrollments), color = ~gender , showlegend=TRUE,
             text = ~paste0("Ward: ", wards_new, "\n", "Enrollment: ", enrollments, "\n", "Gender: ", gender)) %>%
    add_bars( hoverinfo='text'
              #colors = c("#458B74", "#6495ED", "#8B7355", "#483D8B", "#00EE00", "#FF69B4")
    ) %>%
    layout(bargap = 0.5, group = ~primary_enrollment_sankey , 
           barmode = 'stack',
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           margin = list( l = 50),
           title = "Primary school enrollments per ward for 2019",
           xaxis = list(title = "ward",tickangle = 45),
           yaxis = list(title= "Number of students"))
  
  
  # leaflet map
  enrol_map <- leaflet(data=edepartment_data) %>%
    addTiles(group = "OSM") %>%
    setView(lng = 37.5, lat = -2.3, zoom = 9) %>% 
    addPolygons(data = wards, opacity = 1, fill = FALSE,weight = 2, group = "Subcounty") %>%
    addMarkers(data = edepartment_data, lng = edepartment_data$longitudes, lat = edepartment_data$latitudes, 
               group = "Schools", clusterOptions = markerClusterOptions(),
               popup = paste0("Name: ",edepartment_data$school_name, "<br/>",
                              "Status: ",edepartment_data$school_type, "<br/>",
                              "Registration status: ", edepartment_data$registration_status,"<br/>",
                              "Ward: ",edepartment_data$wards_new))
  
  return(list(enrolment.trends,enrolments_county,enrolments_gender,enrol_map,pry.enrol.ward))
}