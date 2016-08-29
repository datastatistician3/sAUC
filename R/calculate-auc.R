#' @name calculate_auc
#' @export
#'
#' @title Calculate different estimates related to AUC
#'
#' @description Calculate variance of predicted AUC, logit of predicted AUC, and variance of logit of predicted AUC responses passed
#' to calculate_auc variables to snake_case, even if it's already in snake_case.
#'
#' @param x A vector responses at each level of covariates for treatment A.
#'
#' @param y A vector responses at each level of covariates for treatment B.
#'
#' @param data An R dataframe object
#'
#' @return A list of AUC estimates.
#'
#' @author Som Bohora
#'
#' @examples
#' calculate_auc(mpg ~ am, data = mtcars)

calculate_auc <- function (x, y=NULL, data=NULL){
  if(is.numeric(x)){
    ya <- x
    yb <- y
  } else if("formula" %in% is(x)){
    cx <- as.character(x)
    # if(){}# error processing and checking for x being a two-sided formula with a numeric outcome and a single two-valued factor right hand side
    theFactor <- as.factor(data[,cx[3]])
    theNumber <- data[,cx[2]]
    ya <- theNumber[theFactor %in% levels(theFactor)[1]]
    yb <- theNumber[theFactor %in% levels(theFactor)[2]]
  }

  finv <- function(x){
    -log( (1/x) -1)
  }
  m <- length(ya)
  p <- length(yb)
  I <- matrix(NA,m,p)

  for (i in 1:m){
    for (j in 1:p){
      if(ya[i] > yb[j]) {I[i,j] <- 1 }
      else if (ya[i] == yb[j]) {I[i,j] <- 0.5 }
      else {I[i,j] <- 0}
    }
  }
  auchat <- mean(I)
  finvhat <- finv(auchat)
  vya <- apply(I,1,mean)
  vyb <- apply(I,2,mean)
  svarya <- var(vya)
  svaryb <- var(vyb)
  vhat_auchat <- (svarya/m) + (svaryb/p)
  v_finv_auchat <- vhat_auchat/((auchat^2)*(1-auchat)^2)
  logitauchat <- log(auchat/(1-auchat))
  var_logitauchat <- vhat_auchat /((auchat^2)*(1-auchat)^2)

  list(auchat = auchat, finvhat = finvhat, vhat_auchat = vhat_auchat,
           v_finv_auchat=v_finv_auchat, logitauchat=logitauchat, var_logitauchat=var_logitauchat)
}
# calculate_auc(response ~ group,  data = d)
