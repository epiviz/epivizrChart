library(epivizrChart)
library(shiny)

data(sumexp)

app <- shinyApp(
  ui=fluidPage(
    uiOutput("epivizChart")
  ),
  server=function(input, output, session) {
    
    epivizNav <- epivizNav(chr="chr11", start=118000000, end=121000000, interactive=TRUE)
    # gene_info <- rowRanges(sumexp)
    library(Homo.sapiens)
    genes_track <- epivizNav$add_genome(Homo.sapiens)
    heatmap <- epivizNav$plot(sumexp, datasource_name="sumExp", columns=c("normal", "cancer"))
 
    output$epivizChart <- renderUI({
      epivizNav$render_component(shiny=TRUE)
    })
    
    epivizNav$add_shiny_handler(session)
  }
)

app
