setwd("~/Documents/GitHub/EAD-ASEB-Ssolidissima-OA/data")

test_data <- read.csv("test_data_size_at_age.csv")
str(test_data)
#create a function that returns the negative log likelihood
#no plotting or other visual stuff involved
site = "Eel"
VB.three.NLL <- function(Linfinity, K, sigma, site) {
  LA <- read.csv("test_data_size_at_age.csv")
  Ages <- LA[LA$Site==site,]$Ages
  lengths <- LA[LA$Site==site,]$Lengths
  model.predL <- Linfinity*(1-exp(-K*Ages))
  ndata <- length(Ages)
  NLL <- 0.5*ndata*log(2*pi) + ndata*log(sigma) +
    1/(2*sigma*sigma) * sum((lengths-model.predL)^2)
  NLL2 <- -log(prod(dnorm(x=lengths,mean=model.predL,sd=sigma)))
  NLL3 <- -sum(dnorm(x=lengths,mean=model.predL,sd=sigma,log=TRUE))
  cat("Long form            = ", NLL, " \n")
  cat("Product of dnorm()   = ", NLL2, " \n")
  cat("Sum of -dnorm(log=T) = ", NLL3, " \n")
}
VB.three.NLL(Linfinity=100, K=0.2, sigma=10, site="Eel")

#========================================================
#Show the likelihood surface for Linf, K, fixed sigma
#males vs females, NLL vs. likelihood
#========================================================
VB.surface <- function(sigma=10, site="Eel", plotNLL=T,
                       Linfinity=seq(50,150,1), 
                       K=seq(0.1,0.5,0.01)) {
  LA <- read.csv("test_data_size_at_age.csv")
  Agess <- LA[LA$Site==site,]$Ages
  lengths <- LA[LA$Site==site,]$Lengths
  ndata <- length(Agess)
  nLinf <- length(Linfinity)
  nK <- length(K)
  NLL <- matrix(nrow=nLinf, ncol=nK,byrow=F)
  for (i in 1:nLinf) {
    for (j in 1:nK) {
      model.predL <- Linfinity[i]*(1-exp(-K[j]*Agess))
      if (plotNLL=="show -lnL") {
        NLL[i,j] <- -sum(dnorm(x=lengths,mean=model.predL,sd=sigma,log=TRUE))
      }
      else {
        NLL[i,j] <- prod(dnorm(x=lengths,mean=model.predL,sd=sigma))
      }
    }
  }
  gray.pal <- colorRampPalette(colors=c("gray20","gray90"))
  filled.contour(x=Linfinity,nlevels=30, y=K, z=NLL,
                 color.palette=gray.pal,
                 xlab="Linfinity (cm)", ylab="K (per yr)",
                 key.tiLengthse="-lnL")
  invisible(NLL)
}
VB.surface(sigma=10, Linfinity=seq(50,150,1), 
           K=seq(0.1,0.5,0.01), site="Eel")

require(manipulate)  #need this packAges
manipulate(VB.surface(sigma, site, plotNLL), 
           sigma = slider(min=2,max=30, initial=7, step=0.001),
           site = picker("Eel","Barn"),
           plotNLL= picker("show -lnL", "show likelihood"))

# optim

VB.NLL4 <- function(params, site, filename) {   #pars is a vector!
  Linfinity <- params[1]
  K <- params[2]
  sigma <- params[3]
  LA <- read.csv(file=filename)
  Ages <- LA[LA$Site==site,]$Ages
  lengths <- LA[LA$Site==site,]$Lengths
  model.predL <- Linfinity*(1-exp(-K*Ages))
  ndata <- length(Agess)
  NLL <- -sum(dnorm(x=lengths, 
                    mean=model.predL,sd=sigma,log=TRUE))
  return(NLL)
}
VB.NLL4(params=c(100, 0.2, 10), site="Eel",
        filename="test_data_size_at_age.csv")   #vector of parameters

# Using test data, we should be getting L(p)inf = 90, K = 0.23 # Should be 0.25
optim(fn=VB.NLL4,                #function to minimize
      par=c(100, 0.2, 10),       #starting values in VECTOR
      method="L-BFGS-B",         #algorithm to use
      lower=c(0.01, 0.01, 0.01), #minimum when "L-BFGS-B"
      upper=c(Inf, Inf, 100),    #upper limit for "L-BFGS-B"
      site="Eel",             #additional parameters of VB.NLL4!
      filename="test_data_size_at_age.csv") #additional parameters of VB.NLL4!  

# Using test data, we should be getting L(p)inf = 150, K = 0.26 #Should be 0.28
optim(fn=VB.NLL4,                #function to minimize
      par=c(100, 0.2, 10),       #starting values in VECTOR
      method="L-BFGS-B",         #algorithm to use
      lower=c(0.01, 0.01, 0.01), #minimum when "L-BFGS-B"
      upper=c(Inf, Inf, 100),    #upper limit for "L-BFGS-B"
      site="Barn",             #additional parameters of VB.NLL4!
      filename="test_data_size_at_age.csv") #additional parameters of VB.NLL4!  


# === use new library
library("FSA")
library("FSAdata")
library("nlstools")
library("minpack.lm")

test_data <- read.csv("test_data_size_at_age.csv")

head(test_data)
test_data$Site <-as.factor(test_data$Site)
str(test_data)



(svCom <- vbStarts(Lengths~Ages, data = test_data))
(svGen <- lapply(svCom, rep, 2))
vbGen <- Lengths~Linf[Site]*(1-exp(-K[Site]*(Ages-t0[Site])))
fitGen <- nls(vbGen, data = test_data, start = svGen)
hist(residuals(fitGen), main = "")

vb1KT <- Lengths~Linf*(1-exp(-K[Site]*(Ages-t0[Site])))
sv1KT <- mapply(rep,svCom,c(1,2,2))
vb1LT <- Lengths~Linf[Site]*(1-exp(-K*(Ages-t0[Site])))
sv1LT <- mapply(rep,svCom,c(2,1,2))
vb1LK <- Lengths~Linf[Site]*(1-exp(-K[Site]*(Ages-t0))) #
sv1LK <- mapply(rep,svCom,c(2,2,1))
vb2T <- Lengths~Linf*(1-exp(-K*(Ages-t0[Site])))
sv2T <- mapply(rep,svCom,c(1,1,2))
vb2K <- Lengths~Linf*(1-exp(-K[Site]*(Ages-t0)))
sv2K <- mapply(rep,svCom,c(1,2,1))
vb2L <- Lengths~Linf[Site]*(1-exp(-K*(Ages-t0)))
sv2L <- mapply(rep,svCom,c(2,1,1))
vbCom <- Lengths~Linf*(1-exp(-K*(Ages-t0))) #
vbComT0 <- Lengths~Linf*(1-exp(-K*(Ages))) #
vb1LKT0 <- Lengths~Linf[Site]*(1-exp(-K[Site]*(Ages))) #


 fit1KT <- nls(vb1KT,data=test_data,start=sv1KT)
 fit1LT <- nls(vb1LT,data=test_data,start=sv1LT)
 fit1LK <- nls(vb1LK,data=test_data,start=sv1LK) #
 fit2T <- nls(vb2T,data=test_data,start=sv2T)
 fit2K <- nls(vb2K,data=test_data,start=sv2K)
 fit2L <- nls(vb2L,data=test_data,start=sv2L)
 fitCom <- nls(vbCom,data=test_data,start=svCom) #
 fitComT0 <- nls(vbComT0,data=test_data,start=svCom[1:2]) #
 fit1LKT0 <- nls(vb1LKT0,data=test_data,start=sv1LK[1:2]) #

 anova(fit1LK,fitCom) # Since Linf and K are correlated it doesn't make sense for 
 # there to only be a difference in one of the parameters between the sites
 AIC(fit1LK,fitCom)
 
 anova(fit1LK,fitGen) # Lower AIC for t0 conserved among the sites
 AIC(fit1LK,fitGen)
 
 AIC(fit1LK,fitCom,fitGen)
 
 AIC(fitGen,fit1KT,fit1LT,fit1LK,fit2T,fit2K,fit2L,fitCom,fitComT0,fit1LKT0) # Still within 2 AIC
 
 # Fit 2 L's only
 overview(fit2L)
 coef(fit2L)[-2]
 plot(Lengths~jitter(Ages,0.3),data=test_data,subset=Site=="Barn",pch=19,xlab="Age (yrs)",
      ylab="Total Length (mm)", ylim = c(0,200), xlim = c(0,20))
 points(Lengths~jitter(Ages,0.3),data=test_data,subset=Site=="Eel",pch=19,col="gray")
 vbTypical <- vbFuns("typical")
 curve(vbTypical(x,Linf=coef(fit2L)[-2]),from=2,to=14,lwd=2,add=TRUE)
 curve(vbTypical(x,Linf=coef(fit2L)[-1]),from=2,to=8,col="gray",lwd=2,add=TRUE)
 legend("topleft",legend=c("Barnstable","Eel"),col=c("black","gray"),lwd=2,lty=1,cex=0.75)
 
 # Reference for reasoning to include T0 estimates (but probably a composite from all data is best 
 # rather than site specific if only 20 data points per site and no juveniles): https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/2041-210x.12020
 
 # OGP reference: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0199212
 
 overview(fit1LK) #Note that t0 is not significantly different from 0 - the confidence intervals overlap with 0
 summary(fit1LK)
 coef(fit1LK)

 Linf1 <- coef(fit1LK)[1]
 Linf2 <- coef(fit1LK)[2]
 min.Linf1 <- confint(fit1LK)[1,1]
 max.Linf1 <- confint(fit1LK)[1,2]
 min.Linf2 <- confint(fit1LK)[2,1]
 max.Linf2 <- confint(fit1LK)[2,2]
 K1 <- coef(fit1LK)[3]
 K2 <- coef(fit1LK)[4]
 min.K1 <- confint(fit1LK)[3,1]
 max.K1 <- confint(fit1LK)[3,2]
 min.K2 <- confint(fit1LK)[4,1]
 max.K2 <- confint(fit1LK)[4,2]
 (OGP1 <- as.numeric(log10(K1)+2*log10(0.1*Linf1)))
 (OGP2 <- as.numeric(log10(K2)+2*log10(0.1*Linf2)))
 
 (OGP1.min <- as.numeric(log10(min.K1)+2*log10(0.1*min.Linf1)))
 (OGP1.max <- as.numeric(log10(max.K1)+2*log10(0.1*max.Linf1)))
 (OGP2.min <- as.numeric(log10(min.K2)+2*log10(0.1*min.Linf2)))
 (OGP2.max <- as.numeric(log10(max.K2)+2*log10(0.1*max.Linf2)))
 
 