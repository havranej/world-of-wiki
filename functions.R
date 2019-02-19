
library("rjson")
library("ggplot2")
library("dplyr")
library("viridis")

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

get.links.here.number <- function(page.name){
  # For normal users limited to 500 :(
  url <- sprintf("https://en.wikipedia.org/w/api.php?action=query&titles=%s&prop=linkshere&format=json&redirects=TRUE&lhlimit=max", page.name)
  json.data <- fromJSON(file = url)
  links.number <- length(json.data$query$pages[[1]]$linkshere)
  return(links.number)
}

add.native.language <- function(table, json.data){
  name <- json.data$parse$title
  url <- sprintf("https://en.wikipedia.org/wiki/%s", name)
  
  native.row <- data.frame(title = name, lang.code = "en", language = "English", url = url, row.names = "en")
  result <- rbind(table, native.row)
  return(result)
}

get.language.variations <- function(page.name){
  url <- sprintf("https://en.wikipedia.org/w/api.php?action=parse&page=%s&prop=langlinks&format=json&redirects=TRUE", page.name)
  json.data <- fromJSON(file = url)
  
  titles <- sapply(json.data$parse$langlinks, function(x)x$`*`)
  lang.codes <- sapply(json.data$parse$langlinks, function(x)x$lang)
  languages <- sapply(json.data$parse$langlinks, function(x)x$langname)
  urls <- sapply(json.data$parse$langlinks, function(x)x$url)
  
  result <- data.frame(title = titles, lang.code = lang.codes, language = languages, url = urls, row.names = lang.codes)
  result <- add.native.language(result, json.data)
  return(result)
}

get.page.content <- function(lang.code, page.name){
  url <- sprintf("https://%s.wikipedia.org/w/api.php?action=parse&page=%s&prop=text&format=json&redirects=TRUE", lang.code, page.name)
  json.data <- fromJSON(file = url)
  result <- json.data$parse$text$`*`
  return(result)
}

remove.html <- function(text){
  return(gsub("<.*?>", "", text))
}

get.page.length <- function(lang.code, page.name){
  raw.html <- get.page.content(lang.code, page.name)
  text <- remove.html(raw.html)
  len <- nchar(text)
  return(len)
}

get.checked.page.length <- function(row){
  if(is.na(row["title"])) return(NA)
  return(get.page.length(row["lang.code"], row["title"]))
}

filter.and.add.states <- function(df){
  selection <- as.character(LANG.TO.STATE$lang.code)
  df <- df[rownames(df) %in% selection,] # Lengthy selection used in order to avoid partial matching
  df <- merge(df, LANG.TO.STATE, by = "lang.code", all.y = TRUE)
  return(df)
}

add.values <- function(df){
  values <- apply(df, 1, get.checked.page.length)
  result <- cbind(df, value = values)
  return(result)
}

add.coordinates <- function(df){
  result <- merge(MAP.DATA, df, by = "region")
  result <- result[order(result$order),]
  return(result)
}

MAP.DATA <- map_data("world")
LANG.TO.STATE <- data.frame(lang.code = c("cs", "de", "de", "pl", "sk", "fr", "nl"),
                            region = c("Czech Republic", "Germany", "Austria", "Poland", "Slovakia", "France", "Netherlands"))



page.name <- "Zeus"
x <- get.language.variations(page.name) %>% filter.and.add.states() %>% add.values() %>% add.coordinates()

ggplot(x, aes(long, lat, group=group, fill=value)) + geom_polygon(color="grey") + 
  theme.map() +
  scale_fill_viridis(option = "viridis", 
                     direction = -1, 
                     na.value = "grey50",
                     name = NULL,
                     guide = guide_colorbar(barheight = unit(80, units = "mm"),
                                            barwidth = unit(2, units = "mm"),
                                            title.position = 'left',
                                            title.hjust = 0.5,
                                            label.hjust = 0.5)) + 
  labs(x = NULL, 
       y = NULL, 
       title = page.name, 
       subtitle = "Wikipedia page size in major language of the state")
