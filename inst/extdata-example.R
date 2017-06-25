fasd <- read.csv("inst/extdata/fasd.csv")
fasd[, c("x1", "x2", "group")] <- lapply(fasd[, c("x1", "x2", "group")], function(x) factor(x))

sAUC::sAUC(x = y ~ x1, treatment_group = "group", data = fasd)

sAUC::sAUC(x = y ~ x1 + x2, treatment_group = "group", data = fasd)



fasd_label <- read.csv("inst/extdata/fasd-labels.csv")
fasd_label[, c("smoke", "vitamin", "group")] <- lapply(fasd_label[, c("smoke", "vitamin", "group")], function(x) factor(x))

sAUC::sAUC(x = y ~ smoke, treatment_group = "group", data = fasd_label)

sAUC::sAUC(x = y ~ smoke + vitamin, treatment_group = "group", data = fasd_label)
