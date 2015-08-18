library(shiny)
           
shinyUI(pageWithSidebar(
  headerPanel(HTML('<p style="color:darkred; font-size:30px">Which Wine Grower?</p>')),
  sidebarPanel(

    sliderInput("clusters", "K-means Cluster count:", 
                min=1, max=8, value=1),
    br(),
    br(),
    checkboxInput(inputId = "growers",
      label = strong("Show Wine Growers"),
      value = FALSE),
      br(),
      br(),
    numericInput("seedval", "Change Seed Value:", 
                value=999)      
   ),
   
  mainPanel(
    plotOutput('plot1')
  ))
)