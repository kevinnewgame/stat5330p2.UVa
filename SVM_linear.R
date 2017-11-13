#### helpers ####
tuneLinear_f <- function(cost) {
    # List of cost -> SVM object
    return(
        tune(svm, 
             type ~ ., data = dat$train, 
             kernel = "linear",
             ranges = list(cost = cost)))
}

plotTuneLinear <- function(obj, n) {
    # SVM object -> NULL
    # print 10-fold CV error rate and plot
    bp <- obj$best.performance
    sprintf("Smallest Error : %.4f \n Try %d",
            bp, n) -> tt
    
    tmp.plot <- obj$performances[, 1:2]
    plot(error ~ log(cost, 2), tmp.plot, type = "b",
         main = tt)
    abline(v = log(obj$best.parameters, 2), col = "red")
}




#### Outputs ####
## Plots
for (i in seq(5)) {
    fn <- paste("tuneLinear_", i, ".png", sep = "") # filename
    png(filename = paste("image/linear/", fn, sep = ""),
        width = 640, height = 480)
    
    plotTuneLinear(tuneLinear[[i]], i)
    
    dev.off()
}

## Get the dispersion
disp.linear <- unlist(sapply(tuneLinear, function(o) o$performances$dispersion))

## Best index
bi <- which.min(unlist(sapply(tuneLinear, function(o) o$best.performance)))
log(tuneLinear[[bi]]$best.parameters, 2)

## 100 times CV of best radial
perfs.linear <- vector(mode = "numeric", length = 100)
bp <- tuneLinear[[bi]]$best.parameters
for (i in seq(100)) {
    tuneLinear_f(bp) -> best.linear
    perfs.linear[i] <- best.linear$best.performance
}




#### Searching ####
# COST = 2^seq(-10, 7, by = 1)
# tuneLinear.0 <- tuneLinear(COST)
# plotTuneLinear(tuneLinear.0, 1)
# 
# COST = 2^seq(7, 16, by = 1)
# tuneLinear.1 <- tuneLinear(COST)
# plotTuneLinear(tuneLinear.1, 2)
# 
# COST = 2^seq(-8, 9, by = .5)
# tuneLinear.2 <- tuneLinear(COST)
# plotTuneLinear(tuneLinear.3, 3)
# 
# COST = 2^seq(-8, -4, by = .25)
# tuneLinear.3 <- tuneLinear(COST)
# plotTuneLinear(tuneLinear.4, 4)
# 
# COST = 2^seq(-8, -4, by = .1)
# tuneLinear.5 <- tuneLinear_f(COST)
# plotTuneLinear(tuneLinear.5, 5)

