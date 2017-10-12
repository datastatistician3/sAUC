
[![Build Status](https://travis-ci.com/sbohora/sAUC.svg?token=shyYTzvvbsLRHsRAWXTg)](https://travis-ci.com/sbohora/sAUC)

## Semi-parametric Area Under the Curve (sAUC) Regression
Perform AUC analyses with discrete covariates and a semi-parametric estimation

### What is sAUC model and why?

In many applications, comparing two groups while adjusting for multiple covariates is desired for the statistical analysis.  For instance, in clinical trials, adjusting for covariates is a necessary aspect of the statistical analysis in order to improve the precision of the treatment comparison and to assess effect modification. sAUC is a semi-parametric AUC regression model to compare the effect of two treatment groups in the intended non-normal outcome while adjusting for discrete covariates. More detailed reasons on what it is and why it is proposed are outlined in [this paper](https://sbohora.github.io/sAUC/articles/bohora-etal-sauc-paper.pdf). A major reason behind the development of this method is that this method is computationally simple and is based on closed-form parameter and standard error estimation.

### Model

We consider applications that compare a response variable y between two groups (A and B) while adjusting for k categorical covariates ![](http://latex.codecogs.com/gif.latex?X_1,X_2,...,X_k).  The response variable y is a continuous or ordinal variable that is not normally distributed.  Without loss of generality, we assume each covariate is coded such that ![](http://latex.codecogs.com/gif.latex?X_i%3D1,...,n_i),for ![](http://latex.codecogs.com/gif.latex?i%3D1,...,k). For each combination of the levels of the covariates, we define the Area Under the ROC curve (AUC) in the following way:

![](http://latex.codecogs.com/gif.latex?%5Cpi_%7Bx_1%20x_2...x_k%7D%3DP(Y%5EA%3EY%5EB%7CX_1%3Dx_1,X_2%3Dx_2,...,X_k%3Dx_k%20)+%5Cfrac%7B1%7D%7B2%7D%20P(Y%5EA%3DY%5EB%7CX_1%3Dx_1,X_2%3Dx_2,...,X_k%3Dx_k%20),)

where ![](http://latex.codecogs.com/gif.latex?x_1%3D1,...,n_1,...,x_k%3D1,...,n_k), and ![](http://latex.codecogs.com/gif.latex?Y%5EA) and ![](http://latex.codecogs.com/gif.latex?Y%5EB) are two randomly chosen observations from Group A and B, respectively.  The second term in the above equation is for the purpose of accounting ties.

For each covariate ![](http://latex.codecogs.com/gif.latex?X_i), without loss of generality, we use the last category as the reference category and define (![](http://latex.codecogs.com/gif.latex?n_i-1)) dummy variables ![](http://latex.codecogs.com/gif.latex?X_i%5E%7B(1)%7D,X_i%5E%7B(2)%7D,...,X_i%5E%7B(n_i-1)%7D) such that 

![](http://latex.codecogs.com/gif.latex?X_i%5E%7B(j)%7D%20(x)%3D%201,%20if%20j%20%3D%20x) and ![](http://latex.codecogs.com/gif.latex?0,%20if%20j%20%5Cne%20x.)

where ![](http://latex.codecogs.com/gif.latex?i%3D1,...,k;%20j%3D1,...,n_i-1;%20x%3D1,...,n_i).   We model the association between AUC ![](http://latex.codecogs.com/gif.latex?%5Cpi_(x_1%20x_2...x_k)) and covariates using a logistic model.  Such a model specifies that the logit of ![](http://latex.codecogs.com/gif.latex?%5Cpi_(x_1%20x_2...x_k)) is a linear combination of terms that are products of the dummy variables defined above.  Specifically,

![](http://latex.codecogs.com/gif.latex?logit(%5Cpi_%7Bx_1%20x_2...x_k%20%7D%20)%3DZ_%7B(x_1%20x_2...x_k%20)%7D%20%5Cboldsymbol%7B%5Cbeta%7D,) 

where ![](http://latex.codecogs.com/gif.latex?Z_%7B(x_1%20x_2...x_k)%7D) is a row vector whose elements are zeroes or ones and are products of ![](http://latex.codecogs.com/gif.latex?X_1%5E%7B(1)%7D%20(x_1%20),...,X_1%5E%7B(n_i-1)%20%7D%20(x_1),...,X_k%5E%7B(1)%7D%20(x_k),...,X_k%5E%7B(n_k-1)%7D%20(x_k)), and ![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Cbeta%7D) is a column vector of nonrandom unknown parameters.  Now, define a column vector ![](http://latex.codecogs.com/gif.latex?%5Cpi) by stacking up ![](http://latex.codecogs.com/gif.latex?%5Cpi_(x_1%20x_2...x_k%20)) and define a matrix Z by stacking up ![](http://latex.codecogs.com/gif.latex?Z_%7B(x_1%20x_2...x_k%20)%7D), as ![](http://latex.codecogs.com/gif.latex?x_i) ranges from 1 to ![](http://latex.codecogs.com/gif.latex?n_i,%20i%3D1,...,k), our final model is  

![](http://latex.codecogs.com/gif.latex?logit(%5Cpi)%3DZ%5Cboldsymbol%7B%5Cbeta%7D%20...(1))

The reason for us to use a logit transformation of the AUC instead of using the original AUC is for variance stabilization.  We will illustrate the above general model using examples.


### Estimation

First, we denote the number of observations with covariates ![](http://latex.codecogs.com/gif.latex?X_1%3Di_1,...,X_k%3Di_k) in groups A and B by ![](http://latex.codecogs.com/gif.latex?N_(i_1...i_k)%5EA) and ![](http://latex.codecogs.com/gif.latex?N_(i_1...i_k)%5EB), respectively.  We assume both ![](http://latex.codecogs.com/gif.latex?N_(i_1...i_k)%5EA) and ![](http://latex.codecogs.com/gif.latex?N_(i_1...i_k)%5EB) are greater than zero in the following development.  An unbiased estimator of ![](http://latex.codecogs.com/gif.latex?%5Cpi_(i_1...i_k)) proposed by Mann and Whitney (1947) is

![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cpi%7D_(i_1...i_k)%3D%5Cfrac%7B%5Csum_%7Bl%3D1%7D%5E%7BN_%7Bi_1...i_k%7D%5EA%7D%20%5Csum_%7Bj%3D1%7D%5E%7BN_%7Bi_1...i_k%7D%5EB%7D%20I_%7Blj%7D%7D%7BN_%7Bi_1...i_k%7D%5EA%20N_%7Bi_1...i_k%7D%5EB%7D,)

where 

![](http://latex.codecogs.com/gif.latex?I_(i_1...i_k);%20lj%20%3D%201) if ![](http://latex.codecogs.com/gif.latex?Y_%7Bi_1...i_k;l%7D%5EA%20%3E%20Y_%7Bi_1...i_k;j%7D%5EB)

and

![](http://latex.codecogs.com/gif.latex?I_(i_1...i_k);%20lj%20%3D%20%5Cfrac%7B1%7D%7B2%7D) if ![](http://latex.codecogs.com/gif.latex?Y_%7Bi_1...i_k;l%7D%5EA%20%3D%20Y_%7Bi_1...i_k;j%7D%5EB)

and

![](http://latex.codecogs.com/gif.latex?I_(i_1...i_k);%20lj%20%3D%200) if ![](http://latex.codecogs.com/gif.latex?Y_%7Bi_1...i_k;l%7D%5EA%20%3C%20Y_%7Bi_1...i_k;j%7D%5EB)


and ![](http://latex.codecogs.com/gif.latex?Y_(i_1...i_k;%20l)%5EA) and ![](http://latex.codecogs.com/gif.latex?Y_(i_1...i_k;%20j)%5EB) are observations with ![](http://latex.codecogs.com/gif.latex?X_1%3Di_1,...,X_k%3Di_k) in groups A and B, respectively.  Delong, Delong and Clarke-Pearson (1988) have shown that 

![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cpi%7D_%7Bi_1...i_k%7D%20%5Capprox%20N(%5Cpi_%7Bi_1...i_k%7D,%5Csigma_%7Bi_1...i_k%7D%5E2)).	

In order to obtain an estimator for ![](http://latex.codecogs.com/gif.latex?%5Csigma_%7Bi_1...i_k%7D%5E2), they first computed

![](http://latex.codecogs.com/gif.latex?V_%7Bi_1...i_k;%20l%7D%5EA%3D%5Cfrac%7B1%7D%7BN_%7Bi_1...i_k%7D%5EB%20%7D%20%5Csum_%7Bj%3D1%7D%5E%7BN_%7Bi_1...i_k%7D%5EB%7D%20I_%7Blj%7D,%20%20%09l%3D1,...,N_%7Bi_1...i_k%7D%5EA)

and

![](http://latex.codecogs.com/gif.latex?V_%7Bi_1...i_k;j%7D%5EB%3D%5Cfrac%7B1%7D%7BN_%7Bi_1...i_k%7D%5EA%20%7D%20%5Csum_%7Bl%3D1%7D%5E%7BN_%7Bi_1...i_k%7D%5EA%7D%20I_%7Blj%7D,%20%20%09j%3D1,...,N_%7Bi_1...i_k%7D%5EB).

Then, an estimate of the variance of the nonparametric AUC was

![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Csigma%7D_%7Bi_1...i_k%7D%5E2%3D%5Cfrac%7B(s_%7Bi_1...i_k%7D%5EA%20)%5E2%7D%7BN_%7Bi_1...i_k%7D%5EA%7D%20+%20%5Cfrac%7B(s_%7Bi_1...i_k%7D%5EB%20)%5E2%7D%7BN_%7Bi_1...i_k%7D%5EB%7D),

where 

![](http://latex.codecogs.com/gif.latex?(s_%7Bi_1...i_k%7D%5EA%20)%5E2) and ![](http://latex.codecogs.com/gif.latex?(s_%7Bi_1...i_k%7D%5EB%20)%5E2) were the sample variances of 

![](http://latex.codecogs.com/gif.latex?V_%7Bi_1...i_k;%20l%7D%5EA;%20l%3D1,...,N_%7Bi_1...i_k%7D%5EA) and ![](http://latex.codecogs.com/gif.latex?V_%7Bi_1...i_k;%20j%7D%5EB;%20j%3D1,...,N_%7Bi_1...i_k%7D%5EB,) respectively.  Clearly, we need both ![](http://latex.codecogs.com/gif.latex?N_%7Bi_1...i_k%7D%5EA) and ![](http://latex.codecogs.com/gif.latex?N_%7Bi_1...i_k%7D%5EB) are greater than two in order to compute ![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Csigma%7D_%7Bi_1...i_k%7D%5E2).

Now, in order to estimate parameters in Model (1), we first derive the asymptotic variance of ![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cgamma%7D_%7Bi_1...i_k%7D) using the delta method, which results in

![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cgamma%7D_%7Bi_1...i_k%7D%3Dlogit(%5Chat%7B%5Cpi%7D_%7Bi_1...i_k%7D)%20%5Capprox%20N(logit(%5Cpi_%7Bi_1...i_k%7D),%5Ctau_%7Bi_1...i_k%7D%5E2),)

where ![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Ctau%7D_%7Bi_1...i_k%7D%5E2%3D%5Cfrac%7B%5Chat%7B%5Cgamma%7D_%7Bi_1...i_k%7D%5E2%7D%7B%5Chat%7B%5Cpi%7D_%7Bi_1...i_k%7D%5E2%20%20(1-%5Chat%7B%5Cpi%7D_%7Bi_1...i_k%7D)%5E2%7D) 

Rewriting the above model, we obtain

![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cgamma%7D_%7Bi_1...i_k%7D%3Dlogit(%5Cpi_%7Bi_1...i_k%20%7D)%20%3DZ_%7Bi_1...i_k%7D%20%5Cboldsymbol%7B%5Cbeta%7D%20+%20%5Cepsilon_%7Bi_1...i_k%7D)
         
where, 

![](http://latex.codecogs.com/gif.latex?%5Cepsilon_%7Bi_1,...,i_k%7D%20%5Capprox%20N(0,%5Ctau_%7Bi_1,...,i_k%7D%5E2)).  Then, by stacking up the ![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cgamma%7D_%7B1_i,...,i_k%7D) to be 
![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cgamma%7D,%20Z_%7Bi_1...i_k%7D) to be ![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7BZ%7D), and ![](http://latex.codecogs.com/gif.latex?%5Cepsilon_%7Bi_1,...,i_k%7D) to be 
![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Cepsilon%7D), we have

![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Chat%7B%5Cgamma%7D%7D%20%3Dlogit%20%5Cboldsymbol%7B%5Chat%7B%5Cpi%7D%7D%20%3D%20%5Cboldsymbol%7BZ%5Cbeta%20+%20%5Cepsilon%7D), 

where, ![](http://latex.codecogs.com/gif.latex?E(%5Cepsilon)%3D0) and ![](http://latex.codecogs.com/gif.latex?%5Chat%7BT%7D%3DVar(%5Cepsilon)%3Ddiag(%5Chat%7B%5Ctau%7D_%7Bi_1...%20i_k%7D%5E2)) which is a diagonal matrix.  Finally, by using the generalized least squares method, we estimate the parameters β  and its variance-covariance matrix as follows;

![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Chat%7B%5Cbeta%7D%20%3D%7B(%5Chat%7BZ%7D%5ET%20%20%5Chat%7BT%7D%5E%7B-1%7D%20%20Z)%7D%5E%7B-1%7D%20Z%5ET%20%20%5Chat%7BT%7D%5E%7B-1%7D%20%5Chat%7B%5Cgamma%7D%7D)		

and
![](http://latex.codecogs.com/gif.latex?%5Chat%7BV%7D(%5Cboldsymbol%7B%5Chat%7B%5Cbeta%7D%7D)%20%3D%20%5Cboldsymbol%7B%7B(%5Chat%7BZ%7D%5ET%20%20%5Chat%7BT%7D%5E%7B-1%7D%20%20Z)%7D%5E%7B-1%7D%7D)

The above equations can be used to construct a 100(1-α)% Wald confidence intervals for ![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Cbeta_i%7D) using formula

![](http://latex.codecogs.com/gif.latex?%5Chat%7B%5Cbeta%7D_i%20%5Cpm%20Z_%7B1-%5Cfrac%7B%5Calpha%7D%7B2%7D%7D%20%5Csqrt%7B%5Chat%7BV%7D(%5Chat%7B%5Cbeta%7D_i)%7D),

where ![](http://latex.codecogs.com/gif.latex?Z_%7B1-%5Cfrac%7B%5Calpha%7D%7B2%7D%7D) is the ![](http://latex.codecogs.com/gif.latex?(1-%5Cfrac%7B%5Calpha%7D%7B2%7D)%5E%7Bth%7D) quantile of the standard normal distribution.  Equivalently, we reject 

![](http://latex.codecogs.com/gif.latex?H_0:%5Cbeta_i%20%3D%200)  if ![](http://latex.codecogs.com/gif.latex?%7C%5Chat%7B%5Cbeta%7D_i%7C%20%3E%20Z_%7B1-%5Cfrac%7B%5Calpha%7D%7B2%7D%7D%20%5Csqrt%7B%5Chat%7BV%7D(%5Chat%7B%5Cbeta%7D_i)%7D.),

The p-value for testing ![](http://latex.codecogs.com/gif.latex?H_0) is ![](http://latex.codecogs.com/gif.latex?2%20*%20P(Z%20%3E%20%7C%5Chat%7B%5Cbeta%7D_i%7C/%5Csqrt%7B%5Chat%7BV%7D(%5Chat%7B%5Cbeta%7D_i)%7D),)


, where Z is a random variable with the standard normal distribution.
	Now, the total number of cells (combinations of covariates ![](http://latex.codecogs.com/gif.latex?X_1,...,X_k)) is ![](http://latex.codecogs.com/gif.latex?n_1%20n_2...n_k). As mentioned earlier, for a cell to be usable in the estimation, the cell needs to have at least two observations from Group A and two observations from Group B.  As long as the total number of usable cells is larger than the dimension of ![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Cbeta%7D), then the matrix ![](http://latex.codecogs.com/gif.latex?%7B%5Cboldsymbol%7B%5Chat%7BZ%7D%5ET%20%20%5Chat%7BT%7D%5E%7B-1%7D%20%20Z%7D%7D) is invertible and consequently,![](http://latex.codecogs.com/gif.latex?%5Cboldsymbol%7B%5Chat%7B%5Cbeta%7D%7D) is computable and model (1) is identifiable.

