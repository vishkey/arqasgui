#Register in the UI the default distributions
registerDistribution(Exp, "Exponential", TRUE)
registerDistribution(Norm, "Normal", TRUE) #sd > 0
registerDistribution(Weibull, "Weibull", TRUE) 
registerDistribution(Unif, "Uniform", TRUE)
registerDistribution(Chisq, "Chi Square", TRUE)
registerDistribution(Lnorm, "Log-Normal", TRUE)
registerDistribution(Gammad, "Gamma", TRUE)
registerDistribution(Dirac, "Dirac", TRUE)

#Here you can register your own distributions
#registerDistribution(function(mate=30){Exp(mate)}, "My Exponential")