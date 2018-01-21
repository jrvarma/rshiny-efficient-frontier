This is a [R-Shiny](https://shiny.rstudio.com/) App for computing and plotting the Markowitz mean-variance efficient frontier. Most of the hard work is done by the [`fPortfolio`](https://cran.r-project.org/web/packages/fPortfolio/index.html) package which is part of [Rmetrics](https://www.rmetrics.org/).

The App consists of three `R` files:

*  As in all Shiny Apps, `ui.R` is the user interface script handles the user experience:  it sets the page details (the way the app looks like), lists the input options and defines the output formats.

*  The server script (`server.R`) does all the `R` work, meaning it handles all the input calls and instructions given to the app and returns the output to be displayed on the page.

*  The file `my-fpoints.R` is optional (it is used only if `use_original_frontierPlot` is set to `FALSE` at the top of `server.R`). It defines a more robust version of `frontierPlot` which handles degenerate efficient frontiers without errors.

