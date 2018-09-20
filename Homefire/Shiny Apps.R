# Every Shiny app has the same structure: two R scripts saved together in a directory. At a minimum, a Shiny app has ui.R and server.R files.
# You can create a Shiny app by making a new directory and saving a ui.R and server.R file inside it. Each app will need its own unique directory.
# You can run a Shiny app by giving the name of its directory to the function runApp. For example if your Shiny app is in a directory called my_app, run it with the following code:
#   
# > library(shiny)
# > runApp("my_app")

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))