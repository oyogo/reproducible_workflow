production_visualizations <- function(kcep.agricultural_assets){
  
  kcep.agricultural_assets %>%
    dplyr::filter(asset != "NA" & county == "Makueni") %>%
    dplyr::group_by(subcounty, asset, no_asset) %>%
    ggplot(aes(x = asset, y = as.integer(no_asset))) +
    geom_col() +
    theme_bw() +
    coord_flip() +
    labs(y = "Assets count", x = "Asset", title = "Agricultural assets") +
    theme(
      strip.placement = "outside",
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 16)
    ) +
    facet_wrap( ~ subcounty)
}