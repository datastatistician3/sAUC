


### Example using Semi-parametric Area Under the Curve (sAUC) Regression

#### Perform AUC analyses with discrete covariates and a semi-parametric estimation

To illustrate how to apply the proposed method, we obtained data from a randomized and controlled clinical trial, which was designed to increase knowledge and awareness to prevent Fetal Alcohol Spectrum Disorders (FASD) in children through the development of printed materials targeting women of childbearing age in Russia. One of the study objectives was to evaluate effects of FASD education brochures with different types of information and visual images on FASD related knowledge, attitudes, and alcohol consumption on childbearing-aged women. The study was conducted in two regions in Russia including St. Petersburg (SPB) and the Nizhny Novgorod Region (NNR) from 2005 to 2008. A total of 458 women were recruited from women's clinics and were randomly assigned to one of three groups (defined by the GROUP variable): (1) a printed FASD prevention brochure with positive images and information stated in a positive way, positive group (PG), (2) a FASD education brochure with negative messages and vivid images, negative group (NG); and (3) a general health material, control group (CG). For the purpose of the analysis in this thesis, only women in the PG and CG were included. Data were obtained from the study principal investigators . The response variable was the change in the number of drinks per day (CHANGE_DRINK=number of drinks after-number of drinks before) on average in the last 30 days from one-month follow-up to baseline. Two covariates considered for the proposed method were "In the last 30 days, have you smoked cigarettes?" (SMOKE) and  "In the last 30 days, did you take any other vitamins?" (OVITAMIN). Both covariates had "Yes" or "No" as the two levels. The question of interest here was to assess the joint predictive effects of SMOKE and OVITAMIN on whether the participants reduced the number of drinks per day from baseline to one month follow-up period. A total of 210 women with no missing data on any of the CHANGE_DRINK, SMOKE, GROUP, and OVITAMIN were included in the analysis.

The response variable CHANGE_DRINK was heavily skewed and not normally distributed in each group  (Shapiro-Wilk p<0.001). Therefore, we decided to use the AUG regression model to analyze the data.  In the AUG regression model we define
$$\LARGE \pi = p(Y_{CG} > Y_{PG})$$ Note that the value of $\Large \pi$ greater than .5 means that women in the PG had a greater reduction of alcohol drinks than those in the CG. For statistical results, all p-values < .05 were considered statistically significant and 95% CIs were presented.


### Result of sAUC Regression with one discrete covariate


```r
library(sAUC)
library(DT)
library(shiny)
fasd_label <- read.csv("../../extdata/fasd-labels.csv")
fasd_label[, c("smoke", "vitamin", "group")] <- lapply(fasd_label[, c("smoke", "vitamin", "group")], function(x) factor(x))

result_one <- sAUC::sAUC(x = y ~ smoke, treatment_group = "group", data = fasd_label)
```

```
The model is:  logit [ p ( Y_Control  >  Y_Treatment ) ]  =  beta_0 +  beta_1*smokeYes 

Model Summary
```

```r
result_two <- sAUC::sAUC(x = y ~ smoke + vitamin, treatment_group = "group", data = fasd_label)
```

```
The model is:  logit [ p ( Y_Control  >  Y_Treatment ) ]  =  beta_0 +  beta_1*smokeYes + beta_2*vitaminYes  

Model Summary
```

```r
# result_one$`Model summary`
DT::datatable(as.data.frame(result_one$"Model summary"),
            caption = htmltools::tags$caption(
              style = "font-size:120%",
              strong('Model results'), '{Note: left-side of model :  ', result_one$"model_formula","}"))
```

<!--html_preserve--><div id="htmlwidget-266cb2a804c26f2c41b2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-266cb2a804c26f2c41b2">{"x":{"filter":"none","caption":"<caption style=\"font-size:120%\">\n  <strong>Model results<\/strong>\n  {Note: left-side of model :  \n  logit [ p ( Y Control  &gt;  Y Treatment ) ] \n\n\n  }\n<\/caption>","data":[["(Intercept)","smokeYes"],[-0.9099,0.7668],[0.3219,0.3629],[-1.5409,0.0555],[-0.2789,1.478],[0.0047,0.0346]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Coefficients<\/th>\n      <th>Std. Error<\/th>\n      <th>2.5%<\/th>\n      <th>97.5%<\/th>\n      <th>Pr(&gt;|z|)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### Result of sAUC Regression with two discrete covariates


```r
DT::datatable(as.data.frame(result_two$"Model summary"),
            caption = htmltools::tags$caption(
              style = "font-size:120%",
              strong('Model results'), '{Note: left-side of model :  ', result_two$"model_formula","}"))
```

<!--html_preserve--><div id="htmlwidget-fa9e17823a4ff9a7bc31" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-fa9e17823a4ff9a7bc31">{"x":{"filter":"none","caption":"<caption style=\"font-size:120%\">\n  <strong>Model results<\/strong>\n  {Note: left-side of model :  \n  logit [ p ( Y Control  &gt;  Y Treatment ) ] \n\n\n  }\n<\/caption>","data":[["(Intercept)","smokeYes","vitaminYes"],[-1.0657,0.7434,0.2189],[0.4326,0.3685,0.3379],[-1.9136,0.0212,-0.4435],[-0.2177,1.4656,0.8812],[0.0138,0.0436,0.5172]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Coefficients<\/th>\n      <th>Std. Error<\/th>\n      <th>2.5%<\/th>\n      <th>97.5%<\/th>\n      <th>Pr(&gt;|z|)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
# result_two$`Model summary`
```

