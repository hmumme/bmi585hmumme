# bmi585hmumme
BMI 585 - Final Project

### Function Descriptions:
#### newFunctions.R
- unscale(x) - unscale a numerical object from scale() function
- pcApprox(x,npc) - approximate data from n principle components
- pcLollipop(x) - create lollipop plots for each principle component of data
#### quiz02Functions.R
- sensitivity(pred,truth) - calculate sensitivity from predicted and truth vectors
- specificity(pred,truth) - calculate specificity from predicted and truth vectors
- accuracy(pred,truth) - calculate accuracy from predicted and truth vectors
- ppv(pred,truth) - calculate PPV from predicted and truth vectors
- f1(pred,truth) - calculate F1 Score from predicted and truth vectors
#### hw04Functions.R
- boxMuller(n) - use Box-Muller transformation to generate 2 vectors of random, normally distributed n values
#### hw06Functions.R
- twoSidedT(t,n) - generate p-value from two-sided t-test statistic
- twoSizedZ(z) - generate p-value from two-sided z-test statistic
- effectSize(x,g) - calculate effect size from value of vectors and grouping variable
- welchT(x,y) - perform welch t-test
#### hw07Functions.R
- minimumN(d) - find minimum number of samples needed for effect size d and power = 0.8
- chiSquareCounts(tib) - perform chi square test of homogeneity
- postHocPower(d,n1,n2) - calculate post hoc power from effect size and number of samples
#### hw08Functions.R
- bhAdjust(p) - test for significant p-values using BH adjustment
- fdrAdjust(p) - test for significant p-values using FDR adjustment
#### hw11Functions.R
- r2(pred,truth) - calculate R-Squared
- adjR2(pred,truth) - calculate Adjusted R-Squared

### R Package and Requirements Installation:
```
devtools::install_github("hmumme/bmi585hmumme")
install.packages("tidyverse")
install.packages("dplyr")
```
### Demo:
Example uses for each function in the package are located in examples.Rmd or examples.html
