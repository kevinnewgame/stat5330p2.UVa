### Other way of plotting

scatterplot3d(plot.matrix, pch = ".")
levelplot(error ~ log(cost, 2) * log(gamma, 2),
          data = tmp.plot,
          cuts = CUTS,
          col.regions = gray(seq(0, 1, length.out = CUTS * 4)),
          panel = function(...) {
              panel.fill(col = "grey")
              panel.levelplot(...)
          })
CUTS = 50
contourplot(error ~ log(cost, 2) * log(gamma, 2),
            data = tmp.plot,
            cuts = CUTS,
            region = TRUE,
            labels = FALSE)

tuneLinear <- list()
for (i in 3:5) {
    tuneLinear[[i]] <- get(paste("tuneLinear.", i, sep = ""))
}
save(tuneLinear, file = "tuneLinear.Rdata")

identical(tuneLinear[[1]], tuneLinear.0)

rm(list=ls(pattern="tuneLinear."))

rm("best.linear", "best.radial", "bi", "fn", "i", "tmp")
save(tuneRadial, file = "tuneRadial.Rdata")
