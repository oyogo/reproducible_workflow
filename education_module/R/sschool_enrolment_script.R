
sschools_enrolment_visualizations <- function(sabstract_cleaned){
  
  sabstract_cleaned <- data.table::setDT(sabstract_cleaned)[,c(sum_enrolment=sum(enrolment)),by=c("year","Gender")]
  
  
 enrolment_secschools_trend <-  ggplot(sabstract_cleaned, aes(x=year,y=V1,group=Gender, color= Gender)) +
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
 
return(enrolment_secschools_trend)
 
}