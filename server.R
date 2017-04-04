library(shiny)
library(fPortfolio)

use_original_frontierPlot = TRUE  # source file my-fpoints.R is NOT needed 
## use_original_frontierPlot = FALSE # source file my-fpoints.R is needed

if (use_original_frontierPlot){
    ## Set myfrontierPlot to the original frontierPlot in fPortfolio
    myfrontierPlot = frontierPlot
}else{
    ## Use a more robust version of frontierPlot defined in my-fpoints.R
    ## This version handles degenerate efficient frontiers without errors
    source('my-fpoints.R')
}

shinyServer(
    function(input, output, session) {

        mu <- reactive(scan(text=input$mean, quiet = TRUE) / 100)
        sigmas <- reactive(scan(text=input$sigmas, quiet = TRUE) / 100)
        correl.vector <- reactive(scan(text=input$correl, quiet = TRUE))
        correl <- reactive({
            k <- input$k
            m <- matrix(0, k, k)
            j1 <- 1
            cv <- correl.vector()
            for (i in 1:(k-1)){
                j2 <- j1 + (k - i - 1)
                m[(i+1):k, i] <- cv[j1:j2]
                j1 <- j2 + 1
            }
            m <-  m + t(m)
            diag(m) <- 1
            m
        })
        names <- reactive(scan(text=input$names, what = character(),
                               quiet = TRUE))
        
        ## generate iid sample 

        iid.sample <- reactive({
            k <- input$k
            N <- k + 30 # ensure adequate degrees of freedom
            x <- rnorm(N*k) # iid standard normals (in population)
            ## Sample moments of x can differ from population moments.
            ## We now force sample mean = 0 and sample std deviation =
            ## 1 by simple centering and rescaling
            x <- scale(matrix(x, N, k))
            ## To remove sample correlations, we multiply by the
            ## inverse of the cholesky factor of the sample
            ## covariance. The cholesky factor is like the square root
            ## of the matrix
            x %*% solve(chol(cov(x)))
        })

        ## Generate sample with desired sample moments
        my.sample <- reactive({
            k <- input$k
            validate(
                need(length(mu()) == k,
                     'Wrong number of elements for Expected Returns'),
                need(length(sigmas()) == k,
                     'Wrong number of elements for Standard Deviations'),
                need(length(correl.vector()) == k * (k - 1) / 2,
                     'Wrong number of elements for Correlation Matrix'),
                need(length(names()) == k,
                     'Wrong number of elements for Security Names')
                )
            ## print(eigen(correl())$values)
            validate(
                need(all(eigen(correl())$values > 0),
                     'Correlation matrix not positive definite'),
                need(all(sigmas() > 0),
                     'Standard deviation not positive')
                )
            ## To replicate the required correlation matrix we
            ## multiply the iid sample by the cholesky factor of the
            ## sample covariance. The cholesky factor is like the
            ## square root of the matrix
            y <- iid.sample()  %*% chol(correl())
            ## We then rescale the variables to achieve the required
            ## standard deviations
            y <- scale(y, center = FALSE, scale = 1/sigmas()) 
            ## Finally, we then re-center the variables to achieve the
            ## required means
            y <- scale(y, center = - mu(), scale = FALSE)
            dimnames(y)[[2]] <- names()
            y
        })

        output$mu.sd <- renderTable({
            data.frame(Mean = mu(),
                       StdDev = sigmas(),
                       row.names = names())
        })
        output$rho <- renderTable({
            correl()
        })
        output$plot <- renderPlot({
            fp <- portfolioFrontier(as.timeSeries(my.sample()))
            if(input$type == 'Portfolio Composition'){
                weightsPlot(fp)
            }else{
                myfrontierPlot(fp, col = c('blue', 'red'), pch = 20) 
                if(input$frontier.twoasset)
                    twoAssetsLines(fp, col = 'green')
                if(input$montecarlo)
                    monteCarloPoints(fp, input$npoints,
                                     col = 'grey', pch = 20, cex=0.3)
            }
        })
    })


    
