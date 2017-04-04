## myfrontierPoints is a more robust version of frontierPlot
## that can handle degenerate frontiers
## For example, in the risk neutral case:
##   All assets have the same expected return and
##   The frontier is a single point (the MVP).
##   The upper frontier (efficient part) has one point
##   The lower frontier (inefficient part) is empty
## The empty lower frontier causes an error in frontierPlot (in fPortfolio)
## frontierPlot calls the frontierPoints function (also in fPortfolio)
## So we patch frontierPoints by adding the following lines to return NULL
## in the degenerate case:
## 
## if (NROW(ans) == 0){
##     NULL
## }else{
##      ...
##      ans
## }
## 
## We use the proto library to make frontierPlot call the patched frontierPoints
## myfrontierPlot <- with(proto(environment(frontierPlot),
##                              frontierPlot = fPortfolio::frontierPlot,
##                              frontierPoints = myfrontierPoints), frontierPlot)

library(proto)

myfrontierPoints <- function (object, frontier = c("both", "lower", "upper"),
                              return = c("mean", "mu"),
                              risk = c("Cov", "Sigma", "CVaR", "VaR"),
                              auto = TRUE) {
    frontier = match.arg(frontier)
    return = match.arg(return)
    risk = match.arg(risk)
    if (auto) {
        Type = getType(object)
        Estimator = getEstimator(object)
        if (Type == "MV") 
            risk = "Cov"
        if (Type == "MV" & Estimator != "covEstimator") 
            risk = "Sigma"
        if (Type == "QLPM") 
            risk = "Sigma"
        if (Type == "CVaR") 
            risk = "CVaR"
    }
    if (is.vector(getTargetRisk(object@portfolio))) {
        targetRisk = getTargetRisk(object@portfolio)[risk]
        targetReturn = getTargetReturn(object@portfolio)[return]
    }
    else {
        targetRisk = getTargetRisk(object@portfolio)[, risk]
        targetReturn = getTargetReturn(object@portfolio)[, return]
    }
    ans = cbind(Risk = targetRisk, Return = targetReturn)
    if (frontier == "upper") {
        index = 1:length(ans[, 1])
        test = c(-1, diff(ans[, 1]))
        index = index[test > 0]
        ans = ans[index, ]
    }
    else if (frontier == "lower") {
        index = 1:length(ans[, 1])
        test = c(-1, diff(ans[, 1]))
        index = index[test < 0]
        if (length(index) == 1) {
            ans = matrix(ans[index, ], ncol = 2)
        }
        else {
            ans = ans[index, ]
        }
    }
    if (NROW(ans) == 0){
        NULL
    }else{
        colnames(ans) = c("targetRisk", "targetReturn")
        rownames(ans) = as.character(1:NROW(ans))
        attr(ans, "control") <- c(targetRisk = risk, targetReturn = return, 
                                  auto = as.character(auto))
        ans
    }
}

myfrontierPlot <- with(proto(environment(frontierPlot),
                             frontierPlot = fPortfolio::frontierPlot,
                             frontierPoints = myfrontierPoints), frontierPlot)
