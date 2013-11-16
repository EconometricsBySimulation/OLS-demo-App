library(shiny)

# Define UI for OLS demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Ordinary Least Squares"),
  
  sidebarPanel(
    
    tags$h3("Data Generation"),
    
    sliderInput("nobs", "Number of Observations:", 
                min=10, max=500, value=20),
    
    sliderInput("rseed", "Random Seed:", 
                min=1, max=100, value=1),
    
    sliderInput("sdx", "Standard deviation of X:", 
                min=.25, max=5, value=1, step=.25),
    
    sliderInput("sdu", "Standard deviation of u:", 
                min=1, max=50, value=3, step=1),
    
    selectInput("dgp", "Data Generating Process:",
                list("y=2+3*x+u", 
                     "y=2+3*x-.5*x2+u",
                     "y=-10-1.5*x+x2+u",
                     "y=-2*x-.5*x2+u",
                     "y=5+1*x-.1*expx+u",
                     "y=.2*expx+u*10",
                     "y=-4-2*x2+.4*expx-x*2+u*5",
                     "y=-1-2*x+1.5*x2-1.5*x3-.05*x4
                     +.0001*x5+u*5"
                )),
    checkboxInput("yscalar", "y=y*(-1)", FALSE),
    
    
    br(),
    h3("Estimation"),
    
    selectInput("regression", "regression:",
                list("y~x", 
                     "y~x2",
                     "y~expx",
                     "y~x+x2",
                     "y~x+expx",
                     "y~x+x2+expx",
                     "y~x+x2+x3+x4+x5"
                )),
    
    checkboxInput("constant", "Include Constant", TRUE),
    
    
    h5("Plot"),
    checkboxInput("showdata", "Show Data Points", TRUE),
    
    checkboxInput("predict", "Show Predicted Values", TRUE),
    checkboxInput("resid", "Show Residuals", FALSE),
    
    tags$br(),
    h5("Created by:"),
    tags$a("Econometrics by Simulation", 
           href="http://www.econometricsbysimulation.com"),
    h5("For details on how data is generated:"),
    tags$a("Blog Post", 
           href="http://www.econometricsbysimulation.com/2013/11/a-shiny-app-for-playing-with-ols.html"),
    h5("For details on how data is generated:"),
    tags$a("Github Repository: Econometrics-Apps", 
           href="https://github.com/EconometricsBySimulation/OLS-demo-App/"),
    h5(textOutput("counter"))
    ),
  
  # Show the main display
  mainPanel(
    plotOutput("scatter"),
    tableOutput("lmStats"),
    tableOutput("lmResults"),
    h5(textOutput("datacaption")),
    tableOutput("values")
    )
))
