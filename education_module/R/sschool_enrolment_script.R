
sschools_enrolment_visualizations <- function(sabstract_cleaned,sec.enrol.sankeydata,edep_sschools_cleaned,wards){
  
  # Gender enrolment trends----
  enroltrends.gender.data <- data.table::setDT(sabstract_cleaned)[,c(sum_enrolment=sum(enrolment)),by=c("year","Gender")]
  
  enroltrends.gender.plot <-  ggplot(enroltrends.gender.data, aes(x=year,y=V1,group=Gender, color= Gender)) +
    geom_line(size=2) +
    labs(title="Secondary school enrolments trend",y="Number of students") +
    scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
    theme(plot.title=element_text(hjust=0.5, size=16, color="white"),
          axis.text=element_text(color="white", face="bold",size=14),
          axis.title=element_text(size=14),
          panel.background = element_rect(fill = "#00868B"), 
          plot.background = element_rect(fill = "#00868B"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) 
 
  # Enrolment trends per subcounty----
 enroltrends.scounty.plot <- sabstract_cleaned[,c(sum_enrolment=sum(enrolment)),by=c("year","subcounty")] %>%
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
   layout( title = "Secondary school enrolments per SubCounty",
           yaxis=list(title="Number of students"),
           xaxis=list(title="Subcounty"),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)"
   )
 
 # Ward enrolment trends----
  enroltrends.ward.plot <- sec.enrol.sankeydata %>%
    plot_ly( y = ~count, x = ~reorder(wards_new,count), color = ~gender , showlegend=TRUE,
             text = ~paste0("Ward: ", wards_new, "\n", "Enrollment: ", count, "\n", "Gender: ", gender) ) %>%
    add_bars( hoverinfo='text') %>%
    layout(bargap = 0.5, group = ~sec.enrol.sankeydata , 
           barmode = 'stack',
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           margin = list( r = 100), 
           title = "Enrollments per ward for 2019", 
           xaxis = list(title = "ward",tickangle = 45),
           yaxis = list(title= "Number of students"))
  
 # Enrolment map for secondary schools-----
  sec.enrol.map <- leaflet(data=edep_sschools_cleaned) %>%
    addTiles(group = "OSM") %>%
    setView(lng = 37.5, lat = -2.3, zoom = 9) %>% 
    addPolygons(data = wards, opacity = 1, fill = FALSE,weight = 2, group = "Subcounty") %>%
    addMarkers(data = edep_sschools_cleaned, lng = edep_sschools_cleaned$longitudes, lat = edep_sschools_cleaned$latitudes, 
               group = "Schools", clusterOptions = markerClusterOptions(),
               popup = paste0("Name: ",edep_sschools_cleaned$school_name, "<br/>",
                              "Status: ",edep_sschools_cleaned$school_type, "<br/>",
                              "Registration status: ", edep_sschools_cleaned$registration_status,"<br/>",
                              "Ward: ",edep_sschools_cleaned$wards_new))
 
  # Enrolment trends sankey diagram----
  
  enrollment_sankey_data <- data.table::copy(sec.enrol.sankeydata)
  colnames(enrollment_sankey_data) <- c("source", "target", "value")
  
  enrollment_sankey_data$source <- stringr::str_trim(enrollment_sankey_data$source)
  enrollment_sankey_data$target <- stringr::str_trim(enrollment_sankey_data$target)
  
  enrollment_sankey_data$target <- paste(enrollment_sankey_data$target, " ", sep = "")
  
  nodes <- data.frame(name=c(as.character(enrollment_sankey_data$source), as.character(enrollment_sankey_data$target)) %>% unique())
  
  enrollment_sankey_data$IDsource = match(enrollment_sankey_data$source, nodes$name)-1
  enrollment_sankey_data$IDtarget = match(enrollment_sankey_data$target, nodes$name)-1
  
  
 enrol.sankey.plot <-  plot_ly(
    type = "sankey",
    node = list(
      label = nodes$name,
      #color = c("#76EEC6", "#483D8B", "#6495ED", "#008B00", "#CD8162", "#9ACD32", "#FFC0CB", "#97FFFF", "#C1FFC1", "#A52A2A", "#0000EE", "#FF6A6A", "#1C86EE", "#B23AEE", "#48D1CC"),
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
      #color = viridis_pal(option = "E")(3000)
    )
  )  %>%
    layout(
      title = "Enrollments for 2019",
      #font = f.b,  # uncomment this when transferring to the dashboard
      xaxis = list(showgrid = F, zeroline = F),
      yaxis = list(showgrid = F, zeroline = F),
      margin = list( t = 50, r = 100)#,  # uncomment below lines when transferring to the dashboard
      # plot_bgcolor  = "rgba(0, 0, 0, 0)",
      # paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) 
  
return(list(enroltrends.gender.plot,enroltrends.scounty.plot,enroltrends.ward.plot,sec.enrol.map,enrol.sankey.plot))
 
}