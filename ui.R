library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput("e", "e:", 
                min=0, max=1, value=0.5),
    
    # Decimal interval with step value
    sliderInput("f", "f:", 
                min = 0, max = 1, value = 0.5, step= 0.1),
    
    # Specification of range within an interval
    sliderInput("Erange", "e range:",
                min = 0, max = 1, value = c(0.2,0.5)),
    
    # Specification of range within an interval
    sliderInput("Frange", "f range:",
                min = 0, max = 1, value = c(0.2,0.5)),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    sliderInput("Eanimate", "Looping Animation:", 0, 1, 0.5, step = 0.1, 
                animate=animationOptions(interval=1000, loop=T)),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    sliderInput("Fanimate", "Looping Animation:", 0, 1, 0.5, step = 0.1, 
                animate=animationOptions(interval=1000, loop=T))
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("values")
  )
))