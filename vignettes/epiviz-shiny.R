library(epivizrChart)
library(shiny)
library(Homo.sapiens)

data(tcga_colon_blocks)
data(tcga_colon_curves)

app <- shinyApp(
  ui=fluidPage(
    uiOutput("epivizChart")
  ),
  server=function(input, output, session) {
    
    epivizNav <- epivizNav(chr="chr11", start=118000000, end=121000000, interactive=TRUE)
    genes_track <- epivizNav$add_genome(Homo.sapiens)
    blocks_track <- epivizNav$plot(tcga_colon_blocks, datasource_name="450kMeth")
    means_track <- epivizNav$plot(tcga_colon_curves, datasource_name="450kMeth", type="bp", columns=c("cancerMean","normalMean"))
    
    output$epivizChart <- renderUI({
      epivizNav$render_component(shiny=TRUE)
    })
    
    epivizNav$add_shiny_handler(session)
  }
)

app
