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
#' @param y A treatment group for which a comparision is to be made
#'
#' @param data A dataframe that contains only variables needed for the analysis. At this point,
#' this dataframe should not contain any extra variables
#'
#' @return A list of model summary, coefficients, AUC details, and session information.
#'
#' @author Som Bohora
#'
#' @examples
#' d <- NULL
#' for (x1 in 0:1){
#'  for (x2 in 0:4){
#'    for (x3 in 0:2){
#'      for (group in 0:1){
#'          response <- round(rnorm(n = 100, mean = 0, sd = 1),4)
#'          column <- cbind(x1,x2,x3, group, response)
#'          d <- as.data.frame(rbind(d, column))
#'    }
#'   }
#'  }
#' }
#' sAUC(x = response ~ x1 + x2 + x3, y = "group", data = d)

sAUC <- function(x = FALSE, y = FALSE, data = NULL) {
    # stopifnot(!missing(data), "Input data argument cannot be missing.")

    if ("formula" %in% is(x)){

    x_vars <- attr(terms(x), "term.labels")
    y_var <- as.character(x)[2]
    input_covariates <- x_vars
    input_response <- y_var
    } else {
      testit::assert("Please put response and input as formula. For example, response ~ x1 + x2",
                     !"formula" %in% is(x))
    }

    input_treatment <- y

    # readline(prompt = "What is your response variable?")
    # if (input_response == ""){
    #   stop("The response needs to be defined.")
    # }
    #
    # input_treatment <- readline(prompt = "What is your treatment/grouping variable?")
    # if (input_treatment == ""){
    #   stop("The treatment group need to be defined.")
    # }

    # if (!exists(x = input_covariates)){
    #     warning("Make sure that covariates are comma-separated.")
    # }
    # input_message <- "What are your covariates (comma-separated list)?"
    # input_covariates <- gsub("\\s+", "", unlist(strsplit(readline(input_message),",")))
    message("Data are being analyzed. Please, be patient.")

    d <- as.data.frame(data)
    group_covariates <- as.vector(c(input_treatment,input_covariates))
    d[,group_covariates] <- lapply(d[,group_covariates], factor)

    set1 <-  set2 <-  list()
    grouped_d <- with(d, split(d, d[group_covariates]))

    index <- seq(from=1, to = length(grouped_d), by = 1)
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
      result_auc <-  calculate_auc(as.numeric(unlist(set1[[k]][input_response])), as.numeric(unlist(set2[[k]][input_response])))
      auchat[k] <- result_auc$auchat
      finvhat[k]  <-  result_auc$finvhat
      v_finv_auchat[k]  <-  result_auc$v_finv_auchat
      var_logitauchat[k]  <-  result_auc$var_logitauchat
      logitauchat[k] <-  result_auc$logitauchat
      temp_data_frame  <-  cbind(auchat,finvhat,logitauchat, v_finv_auchat,var_logitauchat)
      auch  <-  as.data.frame(cbind(x_matrix,temp_data_frame))
      gamma1  <-  auch$logitauchat
      var_logitauchat <- auch$var_logitauchat
    }

      unique_x_levels <- sapply(d[,names(d) %in% input_covariates, drop=F], function(x) levels(x))
      matrix_x <- expand.grid(unique_x_levels)

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

}
