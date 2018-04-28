fasd <- read.csv("./inst/extdata/fasd.csv")
vars <- c("x1", "x2", "group")
fasd[, vars] <- lapply(fasd[, vars], function(x) factor(x))

sAUC::sAUC(formula = y ~ x2, treatment_group = "group", data = fasd)

sAUC::sAUC(formula = y ~ x1 + x2, treatment_group = "group", data = fasd)


fasd_label <- read.csv("./inst/extdata/fasd-labels.csv")
fasd_label[, c("smoke", "vitamin", "group")] <- lapply(fasd_label[, c("smoke", "vitamin", "group")], function(x) factor(x))

sAUC::sAUC(formula = y ~ smoke, treatment_group = "group", data = fasd_label)

sAUC::sAUC(formula = y ~ smoke + vitamin, treatment_group = "group", data = fasd_label)



# Run simulation

simulate_one_predictor(iter = 300, m = 100, p = 200)
