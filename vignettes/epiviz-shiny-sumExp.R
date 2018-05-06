library(epivizrChart)
library(shiny)
library(Homo.sapiens)

data(sumexp)

epivizEnv <- epivizEnv(interactive = TRUE)
scatterplot <- epivizEnv$plot(sumexp, datasource_name="sumExp", columns=c("normal", "cancer"))

# add a navigational browser
epivizNav <- epivizNav(chr="chr11", start=118000000, end=121000000, parent=epivizEnv, interactive = TRUE)

genes_track <- epivizNav$add_genome(Homo.sapiens, datasource_name="genes")
region_scatterplot <- epivizNav$plot(sumexp, datasource_name="sumExp", columns=c("normal", "cancer"))

app <- shinyApp(
  ui=fluidPage(
    textInput('gene_loc', 'Enter Genomic Location (example: chr11:118000000 - 121000000', "chr11:118000000-121000000"),
    uiOutput("epivizChart")
  ),
  server=function(input, output, session) {
    
    renderEpiviz <- function() {
      output$epivizChart <- renderUI({
        epivizEnv$render_component(shiny=TRUE)
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
    
    epivizEnv$register_shiny_handler(session)
  }
)

app

