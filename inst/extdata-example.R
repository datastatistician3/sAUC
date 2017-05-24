fasd <- read.csv("inst/extdata/fasd.csv")
fasd[, c("x1", "x2", "group")] <- lapply(fasd[, c("x1", "x2", "group")], function(x) factor(x))

sAUC::sAUC(x = y ~ x1, treatment_group = "group", data = fasd)

sAUC::sAUC(x = y ~ x1 + x2, treatment_group = "group", data = fasd)

