library(e1071)
# library(lattice)


#### Radial Kernel ####

tuneRadial <- function(c, g) {
    return(
        tune(svm, 
             type ~ ., 
             data = dat$train, 
             kernel = "radial",
             ranges = list(
                 cost = c,
                 gamma = g)))
}

plotTuneRadial <- function(obj, n) {
    bp <- obj$best.performance
    # gsub("[^0-9]", "",
    #      # get the name of object
    #      deparse(substitute(obj))) -> try.num
    # Title of plot
    sprintf("Smallest Error : %.4f \n Try %d",
            bp, n) -> tt
    
    plot(obj, 
         transform.x = log2, transform.y = log2,
         xlab = "log_2(cost)", ylab = "log_2(gamma)",
         nlevels = 60,
         main = tt)
         # type = "perspective", theta = 45, phi = 45)
    # abline(h = log(obj$best.parameters$gamma, 2), 
    #        v = log(obj$best.parameters$cost, 2),
    #        col = "red") !!! NOT FIT INTO HEATMAP
}


### Outputs

## Plots
for (i in seq(12)) {
    fn <- paste("tuneRadial_", i, ".png", sep = "") # filename
    png(filename = paste("image/radial/", fn, sep = ""),
        width = 640, height = 480)
    
    plotTuneRadial(tuneRadial[[i]], i)
    
    dev.off()
}

## Get the dispersion
disp.radial <- unlist(sapply(tuneRadial, function(o) o$performances$dispersion))

## Best Radial index
bi <- which.min(unlist(sapply(tuneRadial, function(o) o$best.performance)))
log(tuneRadial[[bi]]$best.parameters, 2)

## 100 times CV of best radial
perfs.radial <- vector(mode = "numeric", length = 100)
for (i in seq(100)) {
    tune(svm, 
         type ~ ., 
         data = dat$train, 
         kernel = "radial",
         ranges = tuneRadial[[bi]]$best.parameters) -> best.radial
    
    perfs.radial[i] <- best.radial$best.performance
}


#### Search for best parameters
# COST <- 2^seq(-5, 18, by = 1)
# GAMMA <- 2^seq(-15, 3, by = 1)
# tuneRadial.0 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.0)
# 
# COST <- 2^seq(5, 30, by = 1)
# GAMMA <- 2^seq(-20, -15, by = 1)
# tuneRadial.1 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.1)
# 
# COST <- 2^seq(30, 40, by = 1)
# GAMMA <- 2^seq(-20, -15, by = 1)
# tuneRadial.2 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.2)
# 
# COST <- 2^seq(10, 30, by = 1)
# GAMMA <- 2^seq(-25, -20, by = 1)
# tuneRadial.3 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.3)
# 
# COST <- 2^seq(30, 40, by = 1)
# GAMMA <- 2^seq(-25, -20, by = 1)
# tuneRadial.4 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.4)
# 
# COST <- 2^seq(15, 35, by = 1)
# GAMMA <- 2^seq(-30, -25, by = 1)
# tuneRadial.5 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.5)
# 
# COST <- 2^seq(35, 40, by = 1)
# GAMMA <- 2^seq(-30, -25, by = 1)
# tuneRadial.6 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.6)
# 
# COST <- 2^seq(40, 44, by = 1)
# GAMMA <- 2^seq(-30, -25, by = 1)
# tuneRadial.7 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.7)
# 
# COST <- 2^seq(20, 44, by = 1)
# GAMMA <- 2^seq(-35, -30, by = 1)
# tuneRadial.8 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.8)
# 
# COST <- 2^seq(44, 54, by = 1)
# GAMMA <- 2^seq(-35, -30, by = 1)
# tuneRadial.9 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.9)
# 
# COST <- 2^seq(55, 57, by = 1)
# GAMMA <- 2^seq(-35, -30, by = 1)
# tuneRadial.10 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.10)
# 
# COST <- 2^seq(26, 50, by = 1)
# GAMMA <- 2^seq(-40, -35, by = 1)
# tuneRadial.11 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.11)
# 
# COST <- 2^seq(51, 55, by = 1)
# GAMMA <- 2^seq(-40, -35, by = 1)
# tuneRadial.12 <- tuneRadial(COST, GAMMA)
# plotTuneRadial(tuneRadial.12)