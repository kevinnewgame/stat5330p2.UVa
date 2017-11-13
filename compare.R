list(error.radial = perfs.radial, error.linear = perfs.linear
     # , dispersion.radial = disp.radial, dispersion.linear = disp.linear
) -> cmp.box


#### 100 k-fold CV of linear, radial; k = 10 ####
png(filename = "image/100CVcompare.png"
    , width = 320, height = 480)

boxplot(cmp.box
        , ylab = "Estimated Test Error Rate" 
        # , xlim = c("Radial", "Linear")
        , xlab = "SVM Kernel"
        , main = sprintf("SVM: %d Times CV", length(cmp.box$error.radial)))

dev.off()

lapply(lapply(cmp.box, sd), round, 4)