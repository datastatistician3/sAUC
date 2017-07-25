
[![Build Status](https://travis-ci.com/sbohora/sAUC.svg?token=shyYTzvvbsLRHsRAWXTg)](https://travis-ci.com/sbohora/sAUC)

## Semi-parametric Area Under the Curve (sAUC) Regression
Perform AUC analyses with discrete covariates and a semi-parametric estimation



### Model

We consider applications that compare a response variable y between two groups (A and B) while adjusting for k categorical covariates ![](http://latex.codecogs.com/gif.latex?X_1,X_2,...,X_k).  The response variable y is a continuous or ordinal variable that is not normally distributed.  Without loss of generality, we assume each covariate is coded such that ![](http://latex.codecogs.com/gif.latex?X_i%3D1,...,n_i),for ![](http://latex.codecogs.com/gif.latex?i%3D1,...,k). For each combination of the levels of the covariates, we define the Area Under the ROC curve (AUC) in the following way:

![](http://latex.codecogs.com/gif.latex?%5Cpi_%7Bx_1%20x_2...x_k%7D%3DP(Y%5EA%3EY%5EB%7CX_1%3Dx_1,X_2%3Dx_2,...,X_k%3Dx_k%20)+%5Cfrac%7B1%7D%7B2%7D%20P(Y%5EA%3DY%5EB%7CX_1%3Dx_1,X_2%3Dx_2,...,X_k%3Dx_k%20)),
where ![](http://latex.codecogs.com/gif.latex?x_1%3D1,...,n_1,...,x_k%3D1,...,n_k), and ![](http://latex.codecogs.com/gif.latex?Y%5EA) and ![](http://latex.codecogs.com/gif.latex?Y%5EB) are two randomly chosen observations from Group A and B, respectively.  The second term in the above equation is for the purpose of accounting ties.

For each covariate ![](http://latex.codecogs.com/gif.latex?X_i), without loss of generality, we use the last category as the reference category and define (![](http://latex.codecogs.com/gif.latex?n_i-1)) dummy variables ![](http://latex.codecogs.com/gif.latex?X_i%5E%7B(1)%7D,X_i%5E%7B(2)%7D,...,X_i%5E%7B(n_i-1)%7D) such that 

![](http://latex.codecogs.com/gif.latex?X_i%5E%7B(j)%7D%20(x)%3D%20%5Cleft%5C%7B%5Cbegin%7Barray%7D%7Brrr%7D%201,%20j%20%3D%20x%20%5C%200,%20j%20%5Cne%20x,%5Cend%7Barray%7D%5Cright.)

where $i=1,...,k; j=1,...,n_i-1; x=1,...,n_i$.   We model the association between AUC $\pi_(x_1 x_2...x_k )$ and covariates using a logistic model.  Such a model specifies that the logit of $\pi_(x_1 x_2...x_k)$ is a linear combination of terms that are products of the dummy variables defined above.  Specifically,
$$logit(\pi_{x_1 x_2...x_k } )=Z_{(x_1 x_2...x_k )} \boldsymbol{\beta},$$ 
where $Z_{(x_1 x_2...x_k)}$ is a row vector whose elements are zeroes or ones and are products of $X_1^{(1)} (x_1 ),...,X_1^{(n_i-1) } (x_1),...,X_k^{(1)} (x_k),...,X_k^{(n_k-1)} (x_k)$, and $\boldsymbol{\beta}$ is a column vector of nonrandom unknown parameters.  Now, define a column vector \pi by stacking up $\pi_(x_1 x_2...x_k )$ and define a matrix Z by stacking up $Z_{(x_1 x_2...x_k )}$, as $x_i$ ranges from 1 to $n_i$, $i=1,...,k$, our final model is  
$$logit(\pi)=Z\boldsymbol{\beta} ...(1)$$
The reason for us to use a logit transformation of the AUC instead of using the original AUC is for variance stabilization.  We will illustrate the above general model using examples.


### Estimation
