
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

get.base.page.json <- function(page.name){
  url <- sprintf("https://en.wikipedia.org/w/api.php?action=parse&page=%s&prop=langlinks&format=json&redirects=TRUE", page.name)
  json.data <- fromJSON(file = url)
  return(json.data)
}

get.language.variations <- function(json.data){
  
  titles <- sapply(json.data$parse$langlinks, function(x)x$`*`)
  lang.codes <- sapply(json.data$parse$langlinks, function(x)x$lang)
  languages <- sapply(json.data$parse$langlinks, function(x)x$langname)
  urls <- sapply(json.data$parse$langlinks, function(x)x$url)
  
  result <- data.frame(title = titles, lang.code = lang.codes, language = languages, url = urls, row.names = lang.codes)
  result <- add.native.language(result, json.data)
  return(result)
}

# Old, slower function
get.page.content <- function(lang.code, page.name){
  url <- sprintf("https://%s.wikipedia.org/w/api.php?action=parse&page=%s&prop=text&format=json&redirects=TRUE", lang.code, page.name)
  json.data <- fromJSON(file = url)
  result <- json.data$parse$text$`*`
  return(result)
}

get.page.extract <- function(lang.code, page.name){
  url <- sprintf("https://%s.wikipedia.org/w/api.php?action=query&titles=%s&prop=extracts&format=json&redirects&explaintext&exlimit=1",
                 lang.code, page.name)
  json.data <- fromJSON(file = url)
  result <- json.data$query$pages[[1]]$extract
  return(result)
}

remove.html <- function(text){
  return(gsub("<.*?>", "", text))
}

get.page.length <- function(lang.code, page.name){
  raw.text <- get.page.extract(lang.code, page.name)
  len <- nchar(raw.text)
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

make.labels <- function(data){
  data.select <- unique(data[c("region", "title", "lang.code")])
  result <- merge(CENTROIDS, data.select, by = "region")
  return(result)
}

set.russian.label.location <- function(centroids){
  centroids["Russia",c("long", "lat")] <- c(33,58)
  return(centroids)
}
