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