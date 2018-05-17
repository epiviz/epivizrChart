library(shiny)

app <- shinyApp(
  ui = fluidPage(
    tags$script(HTML("
        $(document).ready(function() {
          var counter = 0;

          function clickButton(){
            counter++;
            console.log(counter);
            if(counter%2 == 0) {
              Shiny.onInputChange('buttonClickedEven', {
                'counter': counter,
                '.nounce': Math.random()
              });
            }
            else {
              Shiny.onInputChange('buttonClickedOdd', {
                'counter': counter,
                '.nounce': Math.random()
              });
            }
          }

          function clickMultiButton() {
            for(var i=0; i<10; i++) {
              clickButton();
            }
          }

          $('#clicker').click(clickMultiButton);
          
          function receivedMsg(msg) {
            console.log('message received from R');
            console.log(msg);
          }

          Shiny.addCustomMessageHandler('RMsg', receivedMsg);

        })
       ")),
    actionButton("clicker", "click!!")
  ),
  server = function(input, output, session) {
    observeEvent(session$input[["buttonClickedOdd"]], {
      message("ODD - button is clicked. sending back response")
      
      counter <- session$input[['buttonClickedOdd']][["counter"]]
      
      session$sendCustomMessage(type = "RMsg", paste("Odd ", counter))
      
    })
    
    observeEvent(session$input[["buttonClickedEven"]], {
      message("EVEN - button is clicked. sending back response")
      
      counter <- session$input[['buttonClickedEven']][["counter"]]
      
      session$sendCustomMessage(type = "RMsg", paste("Even ",  counter))
      
    })
  }
)

app






library(shiny)

app <- shinyApp(
  ui = fluidPage(
    tags$script(HTML("
                     $(document).ready(function() {
                     var counter = 0;
                     
                     function clickButton(){
                     counter++;
                     console.log(counter);
                     Shiny.onInputChange('buttonClicked', {
                     'counter': counter,
                     '.nounce': Math.random()
                     });
                     }
                     
                     function clickMultiButton() {
                     for(var i=0; i<10; i++) {
                     clickButton();
                     }
                     }
                     
                     $('#clicker').click(clickMultiButton);
                     
                     function receivedMsg(msg) {
                     console.log('message received from R');
                     console.log(msg);
                     }
                     
                     Shiny.addCustomMessageHandler('RMsg', receivedMsg);
                     
                     })
                     ")),
    actionButton("clicker", "click!!")
  ),
  server = function(input, output, session) {
    observeEvent(session$input[["buttonClicked"]], {
      message("button is clicked. sending back response")
      
      counter <- session$input[['buttonClicked']][["counter"]]
      
      session$sendCustomMessage(type = "RMsg",  counter)
      
    })
  }
)

app







