# The-Churning-Problem
Case Study/ DMKM

Customer churn is one of the main areas wherein banks apply data mining techniques. In 2011, Spain
was identified as one of the top European countries in terms of customer attrition, with around 19%
of banks having around 10% churn rates [1]. The idea is that using customer profile and history
with the bank (such as deposits, withdrawals, etc.), banks can predict whether a customer is likely
to churn or not. This information is very useful in that it will be able to alert managers to perform
directed marketing campaigns in order to persuade people to stay or offer their customers other
services that they may be interested in instead.
In this case study, we aim to perform the process of churn modeling with actual bank data us-
ing decision trees, logistic regressions and random forests. Moreover, we will employ the approach
proposed by [2] in finding interactions of variable modalities that are strongly linked with churning
or not churning. This is a novel approach as this application is not currently implemented in R
packages that we are familiar with. It can be treated as an extension of the catdes and condes
functions in FactoMineR.
