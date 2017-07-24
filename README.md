[![Build Status](https://travis-ci.com/sbohora/sAUC.svg?token=shyYTzvvbsLRHsRAWXTg)](https://travis-ci.com/sbohora/sAUC)

## Semi-parametric Area Under the Curve (sAUC) Regression
Perform AUC analyses with discrete covariates and a semi-parametric estimation

We consider applications that compare a response variable y between two groups (A and B) while adjusting for k categorical covariates ($X_1,X_2,...,X_k$).  The response variable y is a continuous or ordinal variable that is not normally distributed.  Without loss of generality, we assume each covariate is coded such that $X_i=1,...,n_i$,for $i=1,...,k$. For each combination of the levels of the covariates, we define the Area Under the ROC curve (AUC) in the following way:

$$\pi_{x_1 x_2...x_k}=P(Y^A>Y^B|X_1=x_1,X_2=x_2,...,X_k=x_k )+\frac{1}{2} P(Y^A=Y^B|X_1=x_1,X_2=x_2,...,X_k=x_k )$$,
where $x_1=1,...,n_1,...,x_k=1,...,n_k$, and $Y^A$ and $Y^B$ are two randomly chosen observations from Group A and B, respectively.  The second term in the above equation is for the purpose of accounting ties.

For each covariate $X_i$, without loss of generality, we use the last category as the reference category and define ($n_i-1$) dummy variables $X_i^{(1)},X_i^{(2)},...,X_i^{(n_i-1)}$  such that 

$$X_i^{(j)} (x)= \left\{\begin{array}
{rrr}
1, j = x \\
0, j \ne x,
\end{array}\right.
$$
where $i=1,...,k; j=1,...,n_i-1; x=1,...,n_i$.   We model the association between AUC $\pi_(x_1 x_2...x_k )$ and covariates using a logistic model.  Such a model specifies that the logit of $\pi_(x_1 x_2...x_k)$ is a linear combination of terms that are products of the dummy variables defined above.  Specifically,
$$logit(\pi_{x_1 x_2...x_k } )=Z_{(x_1 x_2...x_k )} \boldsymbol{\beta}$$, 
where $Z_{(x_1 x_2...x_k)}$ is a row vector whose elements are zeroes or ones and are products of $X_1^{(1)} (x_1 ),...,X_1^{(n_i-1) } (x_1),...,X_k^{(1)} (x_k),...,X_k^{(n_k-1)} (x_k)$, and $\boldsymbol{\beta}$ is a column vector of nonrandom unknown parameters.  Now, define a column vector \pi by stacking up $\pi_(x_1 x_2...x_k )$ and define a matrix Z by stacking up $Z_{(x_1 x_2...x_k )}$, as $x_i$ ranges from 1 to $n_i$, $i=1,...,k$, our final model is  
$$logit(\pi)=Z\boldsymbol{\beta} ...(1)$$
The reason for us to use a logit transformation of the AUC instead of using the original AUC is for variance stabilization.  We will illustrate the above general model using examples.
