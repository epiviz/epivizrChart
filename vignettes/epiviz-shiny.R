library(epivizrChart)
library(shiny)
library(Homo.sapiens)

data(tcga_colon_blocks)
data(tcga_colon_curves)
data(sumexp)

epivizNav <- epivizNav(chr="chr11", start=118000000, end=121000000, interactive=TRUE)
genes_track <- epivizNav$add_genome(Homo.sapiens)
blocks_track <- epivizNav$plot(tcga_colon_blocks, datasource_name="450kMeth")
means_track <- epivizNav$plot(tcga_colon_curves, datasource_name="450kMeth", type="bp", columns=c("cancerMean","normalMean"))
region_scatterplot <- epivizNav$plot(sumexp, datasource_name="sumExp", columns=c("normal", "cancer"))

app <- shinyApp(
  ui=fluidPage(
    textInput('gene_loc', 'Enter Genomic Location (example: chr11:118000000 - 121000000', "chr11:118000000-121000000"),
    uiOutput("epivizChart")
  ),
  server=function(input, output, session) {
    
    renderEpiviz <- function() {
      output$epivizChart <- renderUI({
        epivizNav$render_component(shiny=TRUE)
      })
    }
    
    observeEvent(input$gene_loc, {
      loc <- input$gene_loc
      if(loc != "") {
        chr_split <- strsplit(loc, ":")
        chr <- chr_split[[1]][1]
        range_split <- strsplit(chr_split[[1]][2], "-")
        
        epivizNav$navigate(chr = chr, 
                           start = strtoi(range_split[[1]][1]), 
                           end = strtoi(range_split[[1]][2]))
      }
      renderEpiviz()
    })
    
    epivizNav$register_shiny_handler(session)
  }
)

app

