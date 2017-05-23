library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("e", 
               "f",
               "e range",
               "f range",
               "e loop",
               "f loop"),
      Value = as.character(c(input$e, 
                             input$f,
                             paste(input$Erange, collapse=' '),
                             paste(input$Frange, collapse=' '),
                             input$Eanimate,
                             input$Fanimate)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
})
