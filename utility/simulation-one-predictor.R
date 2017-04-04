# write a function compute to compute AUC and its variance
finv<-function(x){
  -log((1/x)-1)
}
compute=function (d, nd){
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
  finvhat=finv(AUChat)
  vd=apply(I,1,mean)
  vnd=apply(I,2,mean)
  svard = var(vd)
  svarnd = var(vnd)
  Vhat_auchat = (svard/m) + (svarnd/p)
  list (AUChat = AUChat, finvhat = finvhat, Vhat_auchat = Vhat_auchat)
}

simulate_one_predictor <- function(iter=500, m=100, p=120){
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
    d2<-c(0,0.50, 1.00)
    d0<-0.15
    y = c(1:3)
    for (k in y){
      u1<-rexp(p,1); u2<-rexp(m,1)
      d[,k]=-log(u2) + d0 +(d1[k] + d2[k])
      nd[,k]=-log(u1) + d1[k]
      result=compute(d[,k],nd[,k])
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
  invisible(list(m_betas = m_betas, var_finv_auchat = var_finv_auchat, ci_betas = ci_betas, iter= iter))
}

t1 <- proc.time()
ds_betas = simulate_one_predictor(iter = 500, m = 120, p = 100)
proc.time() - t1

t2 <- proc.time()
simulate_one_predictor(iter = 1000, m = 220, p = 200)
proc.time() - t2

t3 <- proc.time()
simulate_one_predictor(iter = 1000, m = 400, p = 300)
proc.time() - t3

t4 <- proc.time()
simulate_one_predictor(iter = 10000, m = 120, p = 100)
proc.time() - t4

meanbeta = apply(ds_betas$m_betas,2,mean) # mean betas
meanvar = apply(ds_betas$var_finv_auchat,2,mean)  # mean variances
meansd = sqrt(meanvar)

#Calculating 95% CI coverage
b0<-ifelse(0.15 >= ds_betas$ci_betas[,1] & 0.15 <= ds_betas$ci_betas[,2],1,0)
b1<-ifelse(0.50 >= ds_betas$ci_betas[,3] & 0.50 <= ds_betas$ci_betas[,4],1,0)
b2<-ifelse(1.00 >= ds_betas$ci_betas[,5] & 1.00 <= ds_betas$ci_betas[,6],1,0)

cov_prob_b0 = (sum(b0)/ds_betas$iter)
cov_prob_b1 = (sum(b1)/ds_betas$iter)
cov_prob_b2 = (sum(b2)/ds_betas$iter)
all_coverage<-c(cov_prob_b0,cov_prob_b1,cov_prob_b2)
