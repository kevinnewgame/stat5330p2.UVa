
#### Linear kernel ####
tuneLinear <- function(cost) {
    # List of cost -> SVM object
    return(
        tune(svm, 
             type ~ ., data = dat$train, 
             kernel = "linear",
             ranges = list(cost = cost)))
}

plotTuneLinear <- function(obj) {
    # SVM object -> NULL
    # print 10-fold CV error rate and plot
    print(obj$best.performance)
    tmp.plot <- obj$performances[, 1:2]
    plot(error ~ log(cost, 2), tmp.plot, type = "b")    
}

COST = 2^seq(-10, 7, by = 1)
tuneLinear.0 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.0)

COST = 2^seq(7, 14, by = 1)
tuneLinear.1 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.1)
# WARNING: reaching max number of iterations

COST = 2^seq(15, 20, by = 1)
tuneLinear.2 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.2)

COST = 2^seq(-8, 15, by = .5)
tuneLinear.3 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.3)

COST = 2^seq(-7, 5, by = .25)
tuneLinear.4 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.4)

COST = 2^seq(-8, -4, by = .1)
tuneLinear.5 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.5)

COST = 2^seq(-8, -7, by = .05)
tuneLinear.6 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.6)

COST = 2^seq(-7.6, -6.8, by = .01)
tuneLinear.7 <- tuneLinear(COST)
plotTuneLinear(tuneLinear.7)
