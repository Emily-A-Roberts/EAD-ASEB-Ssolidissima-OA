

###########################################################################
###########################################################################
###                                                                     ###
###               SECTION 1: DATA INPUT AND INITIALIZATION              ###
###                                                                     ###
###########################################################################
###########################################################################
library("ggplot2")
library("nlme")
library("MuMIn")
library("dplyr")
library("ggpubr")


theme_set(theme_bw())


library("bannerCommenter")
#section() # <--- create big banner
#banner(snug = TRUE, bandChar = "-") # <--- create a small banner
block(paste("very long comment", collapse = " "), fold = TRUE) # <--- for very long comments

setwd("~/Surfclam datasheets")

#section() # <--- create big banner


############################################################################
############################################################################
###                                                                      ###
###                         GROWTH: JUNE TO SEP                          ###
###                                                                      ###
############################################################################
############################################################################

# Read in data and do quality control check
#growth <- read.csv("Surfclam_Data_June2022_July2022_length_axis.csv", stringsAsFactors = F)
growth <- read.csv("Surfclam_Data_June2022_Sep2022.v4.csv", stringsAsFactors = F)

head(growth)
growth <- growth[!is.na(growth$Site),]
plot(growth$L_mm/growth$H_mm~growth$H_mm)
mean(growth$L_mm/growth$H_mm, na.rm = TRUE) #1.285
growth[!is.na(growth$L_mm/growth$H_mm)&(growth$L_mm/growth$H_mm)<1.1,]
growth[!is.na(growth$L_mm/growth$H_mm)&(growth$L_mm/growth$H_mm)>1.6,]

##------------------------------------------------------------
##          I still need to fill in any QC'ed data          --
##              with the ratio of length to height          --
##  But first, I want to check the notebook / frozen clams  --
##------------------------------------------------------------

growth <- growth[!is.na(growth$Start_len_mm),]
growth <- growth[!is.na(growth$L_mm),]
growth <- growth[growth$AliveOrDead=="L",]

growth$len_tot <- growth$L_mm-growth$Start_len_mm
growth$len_per_day <- growth$len_tot/growth$Elapsed_days

mean(growth$len_tot)
mean(growth$len_per_day)

plot(growth$Start_len_mm, growth$len_tot)
growth[growth$len_tot<=0,"Len"]<- NA #There is something funky going on with this one - length and height were out of order and the growth is negative .6mm

par(mfrow = c(1,1))

##------------------------------------
##  June to July growth accounting  --
##------------------------------------

growth_july <- growth[growth$Collection.month=="July",]
growth_aug <- growth[growth$Collection.month=="August",]
growth_sep <- growth[growth$Collection.month=="September",]

head(growth_aug)

#Length change through september
 mod_growth_full <- lme(len_tot~Treatment*Site+Start_len_mm+Start_len_mm:Site, 
                        random = ~1|Location_code, data = growth_sep)
  summary(mod_growth_full)
  M_int<-update(mod_growth_full, .~. - Start_len_mm:Site)
  M <- update(M_int, ~. - Treatment:Site)
  summary(Mheight_int)
  summary(M)

#banner(snug = TRUE, bandChar = "-") # <--- create a small banner

  
  
  ##---------------------------------------------------
  ##  Calculate growth between August and September  --
  ##        based on estimated August lengths        --
  ##---------------------------------------------------

  # Use lm not lme bc hard to account for individual location
  # Be careful that locations are not treated the same across site
  mod_growth_full <- lm(len_tot~Site+Start_len_mm+Start_len_mm:Site, 
                        data = growth_aug) #Aug
  Aug_growth <- predict(mod_growth_full, newdata = growth_sep)
  plot(growth_sep$Start_len_mm,Aug_growth)
  growth_sep$Start_len_mm_Aug <- growth_sep$Start_len_mm+Aug_growth
  growth_sep$Aug_Sep_tot <- growth_sep$L_mm-growth_sep$Start_len_mm_Aug
  
  
  ##----------------------------------------------
  ##  Calculate growth between July and August  --
  ##      based on estimated July lengths       --
  ##----------------------------------------------
  # Note here I'm not using the treatment in the estimation 
  # bc there are so few values for some sites
  mod_growth_full <- lm(len_tot~Site+Start_len_mm+Start_len_mm:Site,
                       data = growth_july) 
  #July growth is used to predict starting values for those collected in August
  July_growth <- predict(mod_growth_full, newdata = growth_aug)
  plot(growth_aug$Start_len_mm,July_growth)
  growth_aug$Start_len_mm_July <- growth_aug$Start_len_mm+July_growth
  growth_aug$July_Aug_tot <- growth_aug$L_mm-growth_aug$Start_len_mm_July
  
  head(growth_sep)
  growth_sep$Aug_Sep_tot
growth_aug[growth_aug$July_Aug_tot<0&!is.na(growth_aug$July_Aug_tot),]

# Remove growth measurements that are <(-2)
growth_aug <- growth_aug[growth_aug$July_Aug_tot>(-2)&!is.na(growth_aug$July_Aug_tot),]

# Remove growth measurements that are <(-2)
growth_sep[growth_sep$Aug_Sep_tot<0,]
growth_sep <- growth_sep[growth_sep$Aug_Sep_tot>(-2),]



##------------------------------------
##  Now calculate growth in Aug     --
##------------------------------------

#growth <- growth_aug
#growth_aug$Start_len_mm_July
#growth_aug$July_Aug_tot

# mod_growth_full <- lme(len_tot~Treatment*Site+Start_len_mm+Start_len_mm:Site, 
#                        random = ~1|Location_code, data = growth_aug)
# 
# M_int<-update(mod_growth_full, .~. - Start_len_mm:Site)
# M <- update(M_int, ~. - Treatment:Site)
# summary(M_int)
# summary(M)
# M_int<-update(mod_growth_full, .~. - Start_len_mm:Site)
# M <- update(M_int, ~. - Treatment:Site)
# summary(M_int)
# summary(M)

  library(gridExtra)

gg1 <- ggplot(data = growth_aug, aes(x=Site, y=July_Aug_tot, color = Treatment))+
  geom_boxplot()+
  xlab("Site") + 
  ylab("Growth per month (mm)")+
  ylim(-2,20)+
  scale_color_discrete(name="Shell hash addition")+
  ggtitle ('A. August')

gg2 <- ggplot(data = growth_sep, aes(x=Site, y=Aug_Sep_tot, color = Treatment))+
  geom_boxplot()+
  xlab("Site") + 
  ylab("Growth per month (mm)")+
  ylim(-2,20)+
  scale_color_discrete(name="Shell hash addition")+
  ggtitle ('B. September')

ggarrange(gg1,gg2, ncol=2, common.legend = TRUE, legend = "right")

# Note that the outliers go down to below -5mm growth with just must be wrong. 
# I don't believe anything below 2mm





library(dplyr)

growth_aug[growth_aug$AliveOrDead=="L","AliveOrDead"] <- 1
head(growth_aug)
growth_aug$survival <- as.numeric(growth_aug$AliveOrDead)
growth_aug$Treatment<-as.factor(growth_aug$Treatment)
growth_aug$Site<-as.factor(growth_aug$Site)
#growth_aug$survival

survive_aug <- growth_aug[growth_aug$Collection1_date!="8/1/2022",]%>%
  group_by(Treatment, Site) %>%
  summarize(num_alive = sum(survival, na.rm = TRUE))
survive_aug$perc <- survive_aug$num_alive / 39

gg3 <- ggplot(data = survive_aug, aes(x=Site, y=perc*100))+
  geom_bar(aes(fill = Treatment), stat = 'identity', position = "dodge")+
  xlab("Site") + 
  ylim(0,100)+
  ylab("Percent survival (%)")+
  scale_color_discrete(name="Shell hash addition")+
  ggtitle ('August')
gg3

growth_sep[growth_sep$AliveOrDead=="L","AliveOrDead"] <- 1
growth_sep$survival <- as.numeric(growth_sep$AliveOrDead)
growth_sep$Treatment<-as.factor(growth_sep$Treatment)
growth_sep$Site<-as.factor(growth_sep$Site)
#growth_sep$survival


survive_sep <- growth_sep%>%
  group_by(Treatment, Site) %>%
  summarize(num_alive = sum(survival, na.rm = TRUE))
survive_sep$perc <- survive_sep$num_alive / 39

gg4 <- ggplot(data = survive_sep, aes(x=Site, y=perc*100))+
  geom_bar(aes(fill = Treatment), stat = 'identity', position = "dodge")+
  xlab("Site") + 
  ylab("Percent survival (%)")+
  ylim(0,100)+
  scale_color_discrete(name="Shell hash addition")+
  ggtitle ('September')
gg4

ggarrange(gg3,gg4, ncol=2, common.legend = TRUE, legend = "right")
















