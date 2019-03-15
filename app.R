
library("shiny")

library("rjson")
library("ggplot2")
library("dplyr")
library("viridis")

source("functions.R")
source("constants.R")


ui <- fluidPage(
  
  tags$head(includeScript("enterKeyDetection.js")),
  
  titlePanel("World of Wiki"),
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput("pagename", label = "Page name", value = "Prague"),
      checkboxInput("showlabels", label = "Show labels", value = TRUE),
      actionButton("recalculate", label = "Show!")
      ),
    mainPanel(
      tags$style(type = "text/css", "#langmap {height: calc(100vh - 80px) !important; width: calc((100vh - 80px)*1.3) !important;}"),
      plotOutput("langmap")
    )
  )
)


server <- function(input, output) {
  getWikiPageData <- reactive({
    input$recalculate
    
    isolate(page.name <- input$pagename)
    
    x <- get.base.page.json(page.name) 
    validate(need(!"error" %in% names(x), "Requested page was not found"))
    
    x <- get.language.variations(x)
    x <- filter.and.add.states(x)
    
    withProgress(message = 'Making the map...', value = 0, {
      x <- add.values(x, with.progress = TRUE) 
    } )    
    
    return(x)
  })
  
  
  output$langmap <- renderPlot({
    
    isolate(page.name <- input$pagename)
    show.labels <- input$showlabels
    
    x <- getWikiPageData()
    x.coordinates <- add.coordinates(x)
    labels <- make.labels(x)   
    
    plot <- ggplot(x.coordinates, aes(long, lat)) + 
            theme_bw() +
            geom_polygon(aes(group=group, fill=value), color="grey") + 
            coord_cartesian(xlim = c(-11,36), ylim = c(36, 70)) +
            scale_fill_viridis(option = "viridis", 
                               direction = -1, 
                               na.value = "grey50",
                               name = NULL,
                               guide = guide_colorbar(barheight = unit(80, units = "mm"),
                                                      barwidth = unit(2, units = "mm"),
                                                      title.position = 'top')) + 
            labs(x = NULL,
                 y = NULL,
                 title = get.english.name(x), 
                 subtitle = "Wikipedia page size in major language of the state")
    
    if(show.labels){
      plot <- plot + geom_label(data=labels, aes(long, lat, label = title), size=3) 
    }
    
    plot
  })
}

shinyApp(ui, server)