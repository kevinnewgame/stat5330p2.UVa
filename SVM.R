library(e1071)
library(lattice)
library(scatterplot3d)
library(rgl)


#### Search for best parameters

perfRadial <- function(cost, gamma) {
    # vector, vector -> data.frame
    # Get (cost, gamma, error) linear formula
    out <- tune(svm, 
                type ~ bone_length + rotting_flesh + 
                    hair_length + has_soul + color, data = dat$train, 
                kernel = "radial",
                ranges = list(cost = cost,
                              gamma = gamma))
    
    return(out$performances[, 1:3])
}

names(dat$train)

# # Initial
# COST <- 25
# GAMMA <- 0.01
# plot.matrix <- perfRadial(COST, GAMMA)

# COST <- seq(.1, 1000, length.out = 10)
# GAMMA <- seq(.05, 2, length.out = 10)

# COST <- 2^seq(-5, 15, by = 1)
# GAMMA <- 2^seq(-15, 3, by = 1)

# COST <- 2^seq(-5, 17, by = 1)
# GAMMA <- 2^seq(-15, 1, by = 1)

# # Try loop limit
# COST <- 2^seq(17, 20, by = 1)
# GAMMA <- 2^seq(-15, -4, by = 1)

COST <- 2^seq(-5, 20, by = 1)
GAMMA <- 2^seq(-15, 1, by = 1)

# system.time(plot.matrix <- rbind(plot.matrix, perfRadial(COST, GAMMA)))
system.time(tmp.plot <- perfRadial(COST, GAMMA))
# 1 point .3 sec

attach(tmp.plot)
CUTS = 50
# levelplot(error ~ log(cost, 2) * log(gamma, 2), 
#           data = tmp.plot,
#           cuts = CUTS,
#           col.regions = gray(seq(0, 1, length.out = CUTS * 4)),
#           panel = function(...) {
#               panel.fill(col = "grey")
#               panel.levelplot(...)
#           })
contourplot(error ~ log(cost, 2) * log(gamma, 2), 
            data = tmp.plot,
            cuts = CUTS,
            region = TRUE,
            labels = FALSE)
detach(tmp.plot)

min(tmp.plot$error)



### Linear kernel
perfLinear <- function(cost) {
    out <- tune(svm, 
                type ~ ., data = dat$train, 
                kernel = "linear",
                ranges = list(cost = cost))
    
    return(out$performances[, 1:3])
}

# COST = 2^seq(11, 13, by = 1)
COST = 2^seq(-8, 8, by = 1)
# out <- tune(svm, 
#             type ~ ., data = dat$train, 
#             kernel = "linear",
#             ranges = list(cost = COST))
# out <- tune(svm, 
#             type ~ .:., data = dat$train, 
#             kernel = "linear",
#             ranges = list(cost = COST))
out <- tune(svm, 
            type ~ .^2, data = dat$train, 
            kernel = "linear",
            ranges = list(cost = COST))

tmp.plot <- out$performances[, 1:2]
plot(error ~ log(cost, 2), tmp.plot,
     type = "l")
min(tmp.plot$error)







### Other way of plotting
# scatterplot3d(plot.matrix, pch = ".")
contourplot(error ~ cost * gamma, 
            data = plot.matrix[cost < 200 & gamma < 2,],
            cuts = 30,
            region = TRUE,
            labels = FALSE)

plot3d()