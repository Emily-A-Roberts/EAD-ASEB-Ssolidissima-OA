install.packages("seacarb")
library(seacarb)
https://cran.r-project.org/web/packages/seacarb/seacarb.pdf

carb(flag=15, var1 =2350e-6 , var2 = 2100e-6, T=15, S=35, P=0, Pt=1e-6, Sit = 10e-6, k1k2="l", pHscale = "F") #9 pH and DIC given
#P = 0 surface of the ocean, 0 bar, 10m = P=1
#assumes phosphate, silicate concentrations are 0
#k1k2=L is the default, Lueker et al. (2000): S ranging between 19 and 43 and T ranging between 2 and 35oC.

carb_out <- carb(flag=9, var1 = 7.8, var2 = 2100e-6, T=20, S=35, P=0, Pt=1e-6, Sit = 10e-6, k1k2="l", pHscale = "F") #9 pH and DIC given
errors(flag=9, var1 = 7.8, var2 = 2100e-6, T=20, S=35, P=0, Pt=1e-6, Sit = 10e-6, k1k2="l", pHscale = "F", 
       evar1 = 0.005, evar2 = 2e-6) #9 pH and DIC given

carb_out$ALK
carb(flag=15, var1 = carb_out$ALK, var2 = carb_out$DIC, T=15, S=35, P=0, Pt=1e-6, Sit = 10e-6, k1k2="l", pHscale = "F") #9 pH and DIC given
errors(flag=15, var1 = carb_out$ALK, var2 = carb_out$DIC, T=15, S=35, P=0, Pt=1e-6, 
       Sit = 10e-6, k1k2="l", pHscale = "F", evar1 = 2e-6, evar2 = 2e-6) #9 pH and DIC given

setwd("~/GitHub/EAD-ASEB-Ssolidissima-OA/data")
df.1 <- read.csv("Summer_2022_carbonate_chem.csv")

carb_out <- carb(flag=9, var1 =df.1$pH , var2 = df.1$DIC*10^-6, T=df.1$pH.temp, S=df.1$Salinity, P=0, Pt=1e-6, Sit = 10e-6, k1k2="l", pHscale = "F") #9 pH and DIC given
carb_out
carb_corr <- carb(flag=15, var1 = carb_out$ALK, var2 = carb_out$DIC, T=df.1$Water.Sed..Temp, S=df.1$Salinity, P=0, Pt=1e-6, Sit = 10e-6, k1k2="l", pHscale = "F") #9 pH and DIC given
df.new <- cbind(Date = df.1$Date,df.1$Sample.ID,df.1$Site,df.1$Stratum,df.1$Shell.treatment,carb_corr)
write.csv(df.new, file = "seacarb_output.csv")
