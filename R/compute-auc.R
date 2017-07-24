#' @name compute_auc
#'
#' @export
#'
#' @title Compute AUC and its variance
#'
#' @description A function compute to compute AUC and its variance
#'
#' @param d  A vector of response from treatment group
#'
#' @param nd A vector of response from control group
#'
#' @author Som Bohora
#'
#' @examples
#' compute_auc(d = c(0.3,0.2,0.6), nd = c(0.2, 0.9,0.1, 0.5))

compute_auc <- function (d, nd){
  auc=0
  m = length(d)
  p = length(nd)
  I=matrix(NA,m,p)

  for (i in 1:m)
    for (j in 1:p){
      if(d[i]>nd[j]){I[i,j]=1}
      else if (d[i]==nd[j]){I[i,j]=0.5}
      else {I[i,j]=0}
    }
  AUChat=mean(I)
  finvhat=compute_inverse(AUChat)
  vd=apply(I,1,mean)
  vnd=apply(I,2,mean)
  svard = stats::var(vd)
  svarnd = stats::var(vnd)
  Vhat_auchat = (svard/m) + (svarnd/p)
  list (AUChat = AUChat, finvhat = finvhat, Vhat_auchat = Vhat_auchat)
}
