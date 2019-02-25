

# HMSC
remotes::install_github("guiblanchet/HMSC") # Started in March 2015.  C++ / RcppArmadillio based implementation
# Not tests, no examples. 
## But see: https://www.helsinki.fi/sites/default/files/atoms/files/hmsc_manual_0.pdf


# Hmsc
remotes::install_github("gtikhonov/HMSC")   # Started in Feb 2018 (after paper was published). Pure R implementation? 
# (No src/, no tests/)
# Vignettes, as R scripts(?!) https://github.com/gtikhonov/HMSC/tree/master/vignettes



## 
library("HMSC")

git2r::clone("https://github.com/guiblanchet/HMSC", "guiblanchet")
## Someone didn't learn how data is packaged....
# Community matrix
Y <- read.csv("data/simulated/Y.csv")
# Covariates
X <- read.csv("data/simulated/X.csv")
### Random effects
Pi <- read.csv("data/simulated/Pi.csv")
### Covert all columns of Pi to a factor
Pi <- data.frame(apply(Pi,2,as.factor))