#' @name simulate_one_predictor
#'
#' @export
#'
#' @title Simulate Semi-parametric AUC regression adjusting for one categorical predictor
#'
#' @description Ask for number of iterations to run the simulation for Semiparametric AUC regression
#' adjusting for one discrete covariate. In this simulation, true parameters are as follows: beta0 = 0.15,
#' beta1 = 0.50, beta2 = 1.
#'
#' @param iter Number of realizations to run
#'
#' @param m Number of observations on treatment condition
#'
#' @param p number of observations on control condition
#'
#' @author Som Bohora
#'
#' @examples
#' simulate_one_predictor(iter = 200, m = 100, p = 120)

simulate_one_predictor <- function(iter=100, m=20, p=30, b0 = 0.15, b1 = 0.50, b2 = 1.00){
  iter = iter
  finvhat=rep(NA,3)
  AUChat=rep(NA,3)
  Vhat_auchat=rep(NA,3)
  lo = up = matrix(NA,iter,3)
  d=matrix(NA,m,3)
  nd=matrix(NA,p,3)

  m_betas<-matrix(NA,iter,3)
  sd_betas<-matrix(NA,iter,3)
  lower = upper =score =  cov_b = matrix(NA,iter,3)
  v_finv_auchat = gamma=rep(NA,3)
  all_var = rep(NA,3)
  var_finv_auchat = matrix(NA,iter,3)

  for (z in 1:iter){
    d1<-c(0,0,0)
    d2<-c(0,b1, b2)
    d0<- b0
    y = c(1:3)
    for (k in y){
      u1 <- stats::rexp(p,1); u2 <- stats::rexp(m,1)
      d[,k]=-log(u2) + d0 +(d1[k] + d2[k])
      nd[,k]=-log(u1) + d1[k]
      result=compute_auc(d[,k],nd[,k])
      AUChat[k]=result$AUChat
      finvhat[k]=result$finvhat
      Vhat_auchat[k] = result$Vhat_auchat
      v_finv_auchat[k]= Vhat_auchat[k]/(((AUChat[k])^2)*(1-(AUChat[k]))^2)  #Variance of F inverse
      #all_var = c(v_finv_auchat[1],sum(v_finv_auchat[1],v_finv_auchat[2]),sum(v_finv_auchat[1],v_finv_auchat[3]))
      gamma = finvhat
    }

    Z<-matrix(c(1,0,0,1,1,0,1,0,1),byrow=T,3,3)
    E = diag(c(v_finv_auchat),3,3)
    tau<-solve(E)
    ztauz <-solve(t(Z)%*%tau%*%Z)
    var_betas<-diag(ztauz)
    betas<-ztauz%*%t(Z)%*%tau%*%gamma
                                            #betas<-solve(x,finvhat)
    m_betas[z,] <- betas
    var_finv_auchat[z,]<-var_betas

    lo[z,] = m_betas[z,] - 1.96*sqrt(var_finv_auchat[z,])
    up[z,] = m_betas[z,] + 1.96*sqrt(var_finv_auchat[z,])
    ci =cbind(lo,up)
    ci_betas = ci[,c(1,4,2,5,3,6)]
  }

  meanbeta = round(apply(m_betas,2,mean),4) # mean betas
  meanvar = round(apply(var_finv_auchat,2,mean),4)  # mean variances
  meansd = round(sqrt(meanvar),4)

  #Calculating 95% CI coverage
  b0 <- ifelse(b0 >= ci_betas[,1] & b0 <= ci_betas[,2],1,0)
  b1 <- ifelse(b1 >= ci_betas[,3] & b1 <= ci_betas[,4],1,0)
  b2 <- ifelse(b2 >= ci_betas[,5] & b2 <= ci_betas[,6],1,0)

  cov_prob_b0 = (sum(b0)/iter)
  cov_prob_b1 = (sum(b1)/iter)
  cov_prob_b2 = (sum(b2)/iter)
  all_coverage<- round(c(cov_prob_b0,cov_prob_b1,cov_prob_b2),4)
  ci_b0 <- paste0("(",paste(round(apply(ci_betas[,1:2], 2, function(x) mean(x)),4), collapse = ", "), ")")
  ci_b1 <- paste0("(",paste(round(apply(ci_betas[,3:4], 2, function(x) mean(x)),4), collapse = ", "), ")")
  ci_b2 <- paste0("(",paste(round(apply(ci_betas[,5:6], 2, function(x) mean(x)),4), collapse = ", "), ")")

  ci_betass <- c(ci_b0, ci_b1, ci_b2)

  list_items <- list(all_coverage = all_coverage, meanbeta = meanbeta, meanvar = meanvar,
                     meansd = meansd, m_betas = m_betas, var_finv_auchat = var_finv_auchat,
                     ci_betas = ci_betas, iter= iter, ci_betass = ci_betass)
  invisible(list_items)
  # cat("Regression coefficients B0, B1, B2\n")

  return(list_items)
}

