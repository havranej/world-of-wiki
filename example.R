
library("rjson")
library("ggplot2")
library("dplyr")
library("viridis")

source("functions.R")
source("constants.R")

theme.map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#FFF8E7", color = NA), 
      panel.background = element_rect(fill = "#FFF8E7", color = NA), 
      legend.background = element_rect(fill = "#FFF8E7", color = NA),
      panel.border = element_rect(fill = NA, color = "gray80", size = 0.5),
      ...
    )
}

page.name <- "Prague"
x <- get.base.page.json(page.name) %>% get.language.variations() %>% filter.and.add.states() %>% add.values()
x.coordinates <- x %>% add.coordinates()
labels <- make.labels(x)


ggplot(x.coordinates, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill=value), color="grey") + 
  coord_cartesian(xlim = c(-11,36), ylim = c(36, 70)) +
  theme.map() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(option = "viridis", 
                     direction = -1,
                     na.value = "grey50",
                     name = NULL,
                     guide = guide_colorbar(barwidth = unit(80, units = "mm"),
                                            barheight = unit(2, units = "mm"),
                                            direction = "horizontal",
                                            title.position = "top",
                                            title = "Page size in characters",
                                            title.hjust = 0.5,
                                            title.theme = element_text(angle = 0, size = 10)
                                            )) + 
  labs(x = NULL, 
       y = NULL, 
       title = as.character(labels[labels$region == "UK", "title"]),
       subtitle = "Wikipedia page size in major language of the state") +
  
  geom_label(data=labels, aes(long, lat, label = title), size=3)
  




