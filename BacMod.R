#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)


# Define UI for slider demo application
ui <- shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Use the sliders to set the parameters"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(

    # set g using slider
    sliderInput("g", "Bacterial growth rate (g):", 
                min=0, max=3, value=1, step= 0.1,
                animate=animationOptions(interval=500, loop=T)),
    
    # set Bmax using slider
    sliderInput("Bmax",
                "Max bacteria load (Bmax): 10^",
                min = 3, max = 7, value = 5,
                animate=animationOptions(interval=1000, loop=T)),
    
    # set d using slider
    sliderInput("d", "Bacterial death rate (d):", 
                min = 0, max = 1, value = 0.1, step= 0.1,
                animate=animationOptions(interval=1000, loop=T)),

    # set k using slider
    sliderInput("k",
                "Bacterial killing rate (k): 10^",
                min = -9, max = -5, value = -7,
                animate=animationOptions(interval=1000, loop=T)),    
    
    # set k using slider
    sliderInput("r",
                "Immune response growth rate (r): 10^",
                min = -5, max = -1, value = -3,
                animate=animationOptions(interval=1000, loop=T)),    
    
    # set d using slider
    sliderInput("delta", "Immune response decay rate (delta):", 
                min = 0, max = 1, value = 0.2, step= 0.1,
                animate=animationOptions(interval=1000, loop=T))
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    withMathJax(includeMarkdown("BacModEq.Rmd")),
    tableOutput("values"),
    plotOutput("odePlot")
  )
))



#functions come first, main program below

###################################################################
#function that specificies the ode model called by lsoda (the ode solver) 
###################################################################
odeequations=function(t,y,pars) 
{ 
  #Note: y is a vector containing the variables of our system, pars is a vector containing the parameters
  #It's not necessary to give them names like B, I, g, etc. We could just work with y[1], par[1] etc.
  #But assigning them easy to understand names often helps, though it slows down the code a bit 
  B=y[1]; I=y[2];  #bacteria and immune response
  g=pars[1]; Bmax=pars[2]; d=pars[3]; k=pars[4]; #model parameters, passed as vector "par" into function by main program
  r=pars[5]; delta=pars[6];  
  
  #these are the differential equations
  dBdt=g*B*(1-B/Bmax)-d*B-k*B*I;
  dIdt=r*B*I-delta*I;
  
  #these is how the differential equations would need to look
  #if we were to skip the step of assigning easy to understand names to the variables and paramters  
  #dBdt=par[1]*y[1]*(1-y[1]/par[2])-par[3]*y[1]-par[4]*y[1]*y[2];
  #dIdt=par[5]*y[1]*y[2]-par[6]*y[2];
  
  return(list(c(dBdt,dIdt))); #this is returned to the calling function, i.e. lsoda
  
} #end function specifying the ODEs

# Define server logic for odeplot
server <- shinyServer(function(input, output) {
 
  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("g",
               "Bmax",
               "d",
               "k",
               "r",
               "delta"),
      Value = as.character(c(input$g, 
                             10^input$Bmax,
                             input$d,
                             10^input$k,
                             10^input$r,
                             input$delta)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
  output$odePlot <- renderPlot({
    
    B0=1e2; #initial number of bacteria 
    I0=10; #initial number of immune response
    Y0=c(B0, I0);  #combine initial conditions into a vector 
    
    #values for model parameters, units are assumed to be 1/days
    g=input$g #bacterial growth rate 
    Bmax=10^input$Bmax #max bacteria load
    d=input$d #bacteria death rate
    k=10^input$k #bacterial killing rate
    r=10^input$r #Immune Respone activation/growth rate
    delta=input$delta #Immune Respone death/decay rate
    pars=c(g,Bmax,d,k,r,delta); #vector of parameters which is sent to the ODE function
    
    tmax=10; #number of days for which to run the simulation
    timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated (from 0 to 10 days in steps of 0.1)
    
    #call ode-solver to integrate ODEs
    #integrate for time "timevec", starting with initial condition 'Y0'. 
    odeoutput=lsoda(Y0,timevec,odeequations,pars);
    
    
    
    
    # generate plot based on input$bins from ui.R
    plot(odeoutput[,1],odeoutput[,2],type="l",xlab="time (days)",ylab="",col="blue",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,1e9))
    lines(odeoutput[,1],odeoutput[,3],type="l",col="red",lwd=2)
    legend("topleft", c("Bacteria","Immune Response"),col = c("blue","red"),lwd=2)
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)