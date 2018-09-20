shinyUI(pageWithSidebar(
  headerPanel('HomeFire k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(homefire)),
    selectInput('ycol', 'Y Variable', names(homefire),
                selected=names(homefire)[[2]]),
    numericInput('clusters', 'Cluster count', 4,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
))