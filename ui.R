library(shiny)

shinyUI(fluidPage(

    sidebarLayout(
        sidebarPanel(
            numericInput('k', 'No of securities', 3),
            textInput('names', 'Security Names', 'Alpha Beta Gamma'),
            textInput('mean', 'Expected Returns (%)', '10 15 20'),
            textInput('sigmas', 'Standard Deviations (%)', '20 25 30'),
            textInput('correl',
                      'Correlations (R2,1 .. Rk,1 R3,2.. Rk,2 ... Rk,k-1)',
                      '0.3 0.2   0.1'),
            selectInput('type', 'Plot type',
                         list('Efficient Frontier',
                              'Portfolio Composition'),
                         selected = 'Efficient Frontier'),
            conditionalPanel(
                condition = "input.type == 'Efficient Frontier'",
                checkboxInput('frontier.twoasset',
                              'Plot All Two Asset Frontiers',
                              value = TRUE),
                checkboxInput('montecarlo', 'Plot Random Portfolios',
                              value = FALSE),
                numericInput('npoints', 'No of Random Portfolios', 200)
                )
            ) ,
        
        mainPanel(
            h1('Efficient Frontier'),
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Input Data",
                         h4('Means and Standard Deviations'),
                         tableOutput("mu.sd"),
                         h4('Correlation Matrix'),
                         tableOutput("rho")
                         )
                )
            )
        ),
    title ="Efficient Frontier"
    ))
