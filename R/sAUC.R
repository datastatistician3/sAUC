#' @name sAUC
#'
#' @export
#'
#' @title Run semiparametric AUC regression model adjusting for categorical covariates
#'
#' @description Ask for data frame that contains only required variables in the model,
#' Request to define response and treatment group, convert variables other than response
#' into factors, estimate model parameters, and display results.
#'
#' @param x A formula with response and covariates such as response ~ x1 + x2
#'
#' @param treatment_group A treatment group for which a comparision is to be made
#'
#' @param data A dataframe that contains only variables needed for the analysis. At this point,
#' this dataframe should not contain any extra variables
#'
#' @return A list of model summary, coefficients, AUC details, and session information.
#'
#' @author Som Bohora
#'
#' @examples
ds <- NULL
for (x1 in 0:1){
 for (x2 in 0:2){
   for (x3 in 0:2){
     for (group in 0:1){
         response <- round(rnorm(n = 100, mean = 0, sd = 1),4)
         column <- cbind(x1,x2,x3, group, response)
         ds <- as.data.frame(rbind(ds, column))
   }
  }
 }
}
ds[,c("x1", "x2", "x3", "group")] <- lapply(ds[,c("x1", "x2", "x3", "group")], function(x) factor(x))

sAUC(x = response ~ x1 + x2 + x3, treatment_group = "group", data = ds)

df <- read.csv("data/one_final.csv")
df[,c("x1", "x2", "group")] <- lapply(df[,c("x1", "x2", "group")], function(x) factor(x))
sAUC(x = y ~ x2 + x1, treatment_group = "group", data = df)

# NOTE: Remove < from <<-
sAUC <- function(x = FALSE, treatment_group = FALSE, data = FALSE) {
  if (missing(x)){
    stop(paste0("Argument x (for e.g. response ~ x1 + x2) is missing."))
  } else if (missing(treatment_group)){
    stop(paste0("Argument treatment_group (treatment group) is missing."))
  } else if (missing(data)){
    stop(paste0("Argument data is missing. Please, specify name of dataframe."))
  } else {
    message(" ")
  }

  if ("formula" %in% is(x)){
    x_vars <<- attr(terms(x), "term.labels")
    y_var <- as.character(x)[2]
    input_covariates <- x_vars
    input_response <- y_var
  } else {
    testit::assert("Please put response and input as formula. For example, response ~ x1 + x2",
                   !"formula" %in% is(x))
  }

  input_treatment <- treatment_group
  if (any(sapply(data[c(treatment_group, input_covariates)], function(x) is.numeric(x)))){
    stop("Covariates including treatment group should be factor.")
  }

  message("Data are being analyzed. Please, be patient.\n\n")

  d <- as.data.frame(data)
  group_covariates <- as.vector(c(input_treatment,input_covariates))

  set1 <-  set2 <-  list()
  grouped_d <<- with(d, split(d, d[group_covariates]))

  #index <- seq(from=1, to = length(grouped_d), by = 1)
  index_first_set <- seq(from=1, to = length(grouped_d), by = 1) %in%
    seq(from=1, to = length(grouped_d), by = 2)
  set1 <- grouped_d[index_first_set]
  set2 <- grouped_d[!index_first_set]

  auch <- temp_data_frame <- result <- x_matrix <- NULL
  length_auc <- length(set1)
  finvhat <- rep(NA,length_auc)
  auchat <- rep(NA,length_auc)
  v_finv_auchat <- rep(NA,length_auc)
  var_logitauchat <- rep(NA,length_auc)
  logitauchat <- rep(NA,length_auc)

  for (k in seq_along(set1)){
    result_auc <<-  calculate_auc(as.numeric(unlist(set1[[k]][input_response])), as.numeric(unlist(set2[[k]][input_response])))
    label_A <- names(set1)[[1]]
    label_B <- names(set2)[[1]]

    PA <- unlist(strsplit(label_A, "[.]"))[1]
    PB <- unlist(strsplit(label_B, "[.]"))[1]

    auchat[k] <- result_auc$auchat
    finvhat[k]  <-  result_auc$finvhat
    v_finv_auchat[k]  <-  result_auc$v_finv_auchat
    var_logitauchat[k]  <-  result_auc$var_logitauchat
    logitauchat[k] <-  result_auc$logitauchat
    temp_data_frame  <-  cbind(auchat,finvhat,logitauchat, v_finv_auchat,var_logitauchat)
    auch  <-  as.data.frame(cbind(x_matrix,temp_data_frame))
    gamma1  <-  auch$logitauchat
    var_logitauchat <<- auch$var_logitauchat
  }

  unique_x_levels <<- lapply(d[,names(d) %in% input_covariates, drop=F], function(x) levels(x))
  matrix_x <<- expand.grid(unique_x_levels)

  Z <- model.matrix(~., matrix_x)

  tau  <-  diag(1/var_logitauchat, nrow = length(var_logitauchat))

  ztauz <- solve(t(Z)%*%tau%*%Z)
  var_betas <- diag(ztauz)
  std_error <- sqrt(var_betas)
  betas <- ztauz%*%t(Z)%*%tau%*%gamma1

  lo <- betas - qnorm(.975)*std_error
  up <- betas + qnorm(.975)*std_error
  ci <- cbind(betas,lo,up)

  p_values <- 2*pnorm(-abs(betas), mean=0,  sd=std_error)

  results <- round(cbind(betas, std_error, lo, up, p_values),4)

  colnames(results) <- c("Coefficients","Std. Error", "2.5%", "97.5%", "Pr(>|t|)")

  varNames <- colnames(Z)
  rownames(results) <- varNames

  list_items <- list("Model summary" = results,"Coefficients" = betas, "AUC details" = auch,
                     "Session information" = sessionInfo(), "Matrix of unique X levels " = matrix_x, "Design matrix" = Z)
  invisible(list_items)

  betas_label <- c(paste0("beta_", 1:length(colnames(Z[,-1])), "*", colnames(Z[,-1]), " +"))

  last_beta_label <- betas_label[max(length(betas_label))]
  all_remain_betas_label <- setdiff(betas_label, last_beta_label)

  all_betas_labels <- c(all_remain_betas_label,gsub("\\+", "\\1", last_beta_label))

  cat("The model is: ","logit","[","p","(",treatment_group,PA, " > ",PB,")","]", " = ", "beta_0 + ",

  cat("The model is: ","logit","[","p","(",paste0("Y_",PA), " > ",paste0("Y_",PB),")","]", " = ", "beta_0 + ",
      all_betas_labels,"\n\n")
  cat("Model Summary\n\n")
  list_items$"Model summary"
}
