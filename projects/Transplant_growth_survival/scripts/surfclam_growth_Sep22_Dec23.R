

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

#section() # <--- create big banner


############################################################################
############################################################################
###                                                                      ###
###                         GROWTH: Sep to Dec                           ###
###                                                                      ###
############################################################################
############################################################################

# Read in data and do quality control check
#growth <- read.csv("Surfclam_Data_June2022_July2022_length_axis.csv", stringsAsFactors = F)

setwd("~/Surfclam datasheets")
growth <- read.csv("12.5.22.SURFCLAM.LENGTHS.FINAL_new_headings.csv", stringsAsFactors = F)
growth <- growth[growth$Buried_Dec=="N",]

head(growth)
growth <- growth[!is.na(growth$Site),]
plot(growth$L_mm/growth$H_mm~growth$H_mm)
mean(growth$L_mm/growth$H_mm, na.rm = TRUE) #1.285
growth[!is.na(growth$L_mm/growth$H_mm)&(growth$L_mm/growth$H_mm)<1.1,]

#growth <- growth[!is.na(growth$L_mm/growth$H_mm)&(growth$L_mm/growth$H_mm)>1.1,] #QC check

growth[!is.na(growth$L_mm/growth$H_mm)&(growth$L_mm/growth$H_mm)>1.6,]

#Mark as alive or dead
growth$AliveOrDead <- rep("Missing", nrow(growth))
growth$AliveOrDead[!is.na(growth$Dead_lengths_mm)] <- "Dead"
growth$AliveOrDead[!is.na(growth$L_mm)] <- "Alive"



##------------------------------------------------------------
##          I still need to fill in any QC'ed data          --
##              with the ratio of length to height          --
##  But first, I want to check the notebook / frozen clams  --
##------------------------------------------------------------

(growth[!is.na(growth$Start_len_mm),])
#growth <- growth[!is.na(growth$L_mm),]
#growth <- growth[growth$AliveOrDead=="L",]

growth$len_tot <- growth$L_mm-growth$Start_len_mm
growth$len_per_day <- growth$len_tot/growth$Elapsed_days

growth$height_tot <- growth$H_mm-growth$Start_height_mm
growth$height_per_day <- growth$height_tot/growth$Elapsed_days

mean(growth$len_tot)
mean(growth$len_per_day)

plot(growth$Start_len_mm, growth$len_tot)
growth[growth$len_tot<=0,"Len"]<- NA 

par(mfrow = c(1,1))

##------------------------------------
##  Sep to Dec growth accounting  --
##------------------------------------

growth_dec <- growth[growth$Collection.month=="December",]

head(growth_dec)
str(growth_dec)
growth_dec$Site <- as.factor(growth_dec$Site)
growth_dec$Treatment <- as.factor(growth_dec$Treatment)
growth_dec$AliveOrDead <- as.factor(growth_dec$AliveOrDead)


#Length change through December
 growth_dec_no_na <- growth_dec[!is.na(growth_dec$len_tot),]

 mod_growth_full <- lme(len_tot~Treatment*Site+Start_len_mm+Start_len_mm:Site, 
                        random = ~1|Location_code, data = growth_dec_no_na)
  summary(mod_growth_full)
  M_int<-update(mod_growth_full, .~. - Start_len_mm:Site)
  M <- update(M_int, ~. - Treatment:Site)
  M2 <- update(M_int, ~. - Start_len_mm)
  
  summary(M_int)
  summary(M)
  summary(M2)
AIC(mod_growth_full,M_int,M,M2)


str(growth_dec)
mod_growth_full <- lme(height_tot~Treatment*Site+Start_len_mm+Start_len_mm:Site, 
                       random = ~1|Location_code, data = growth_dec_no_na)
summary(mod_growth_full)
M_int<-update(mod_growth_full, .~. - Start_len_mm:Site)
M <- update(M_int, ~. - Treatment:Site)
M2 <- update(M_int, ~. - Start_len_mm)

summary(M_int)
summary(M)
summary(M2)
AIC(mod_growth_full,M_int,M,M2)
#banner(snug = TRUE, bandChar = "-") # <--- create a small banner


# Remove growth measurements that are <(-2)... There are none. 
growth_dec[growth_dec$growth_height<0,]
#growth_dec <- growth_dec[growth_dec$growth_height>(-2),]



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

growth_dec$Elapsed_days
gg1 <- ggplot(data = growth_dec, aes(x=Site, y=len_tot/Elapsed_days*30, color = Treatment))+
  geom_boxplot()+
  xlab("Site") + 
  ylab("Growth per month (mm)")+
  ylim(0,9)+
  scale_color_discrete(name="Shell hash addition")+
  ggtitle ('Change in length Sep - Dec')

gg1

gg2 <- ggplot(data = growth_dec, aes(x=Site, y=height_tot/Elapsed_days*30, color = Treatment))+
  geom_boxplot()+
  xlab("Site") + 
  ylab("Growth per month (mm)")+
  ylim(0,9)+
  scale_color_discrete(name="Shell hash addition")+
  ggtitle ('Change in height Sep - Dec')

gg2

ggarrange(gg1,gg2, ncol=2, common.legend = TRUE, legend = "right")

#---------- Plot for proposal
# Pool treatments
gg1 <- ggplot(data = growth_dec, aes(x=Site, y=len_tot/Elapsed_days))+
  geom_boxplot()+
  xlab("Site") + 
  ylab("Growth (mm/day)")+
  ylim(0,9/30)+
  #scale_color_discrete(name="Shell hash addition")+
  ggtitle ('Change in length Sep - Dec')
gg1
str(growth_dec)
mod_growth_full <- lme(height_tot~Site+Start_len_mm+Start_len_mm:Site, 
                       random = ~1|Location_code, data = growth_dec_no_na)
summary(mod_growth_full)
M_int<-update(mod_growth_full, .~. - Start_len_mm:Site)
M2 <- update(M_int, ~. - Start_len_mm)
summary(M2)

(P <- mean(growth_dec_no_na$len_per_day[growth_dec_no_na$Site=="Ptown"]))
(E <- mean(growth_dec_no_na$len_per_day[growth_dec_no_na$Site=="Eel Pond"]))
E/P
(P-E)/E
P/E

# Check that smaller animals aren't just surviving more often in the shell hash addition treatments



Alive_count <- growth_dec %>% count(Treatment, Site, Location_code, AliveOrDead)
mod_growth_full <- lme(n~Treatment*Site, 
                       random = ~1|Location_code, data = Alive_count)
summary(mod_growth_full) # Not really answering my question but not differences in survival between treatments

library(lme4)
growth_dec$AliveOrDead
(gm1 <- glmer(AliveOrDead ~ Treatment *Site + (1 | Location_code),
              data = growth_dec, family = binomial))
summary(gm1) #Here there is an effect of treatment but it's still not answering my question about initial lengths

growth_dec$Start_len_mm
(gm1 <- lme(Start_len_mm ~ Treatment *AliveOrDead+ Site, 
            random = ~1 | Location_code,
              data = growth_dec))
summary(gm1)

#growth_dec$AliveOrDead[growth_dec$AliveOrDead=="Missing"]<- NA
p <- ggplot(growth_dec, aes(x = Treatment, y = Start_len_mm, 
                            color = AliveOrDead))
p + geom_boxplot() + facet_grid(Site ~ .)

p <- ggplot(growth_dec, aes(x = Start_len_mm, y = AliveOrDead, 
                            color = Treatment))
p + geom_boxplot() + facet_grid(Site ~ .)


# Note that the outliers go down to below -5mm growth with just must be wrong. 
# I don't believe anything below 2mm






growth_dec[growth_aug$AliveOrDead=="L","AliveOrDead"] <- 1
head(growth_aug)
growth_aug$survival <- as.numeric(growth_aug$AliveOrDead)
growth_aug$Treatment<-as.factor(growth_aug$Treatment)
growth_aug$Site<-as.factor(growth_aug$Site)
#growth_aug$survival

survive_aug <- growth_aug[growth_aug$Collection1_date!="8/1/2022",]%>%
  group_by(Treatment, Site) %>%
  summarize(num_alive = sum(survival, na.rm = TRUE))
survive_aug$perc <- survive_aug$num_alive / 39

Alive <- as.data.frame(Alive_count[Alive_count$AliveOrDead=="Alive",])

str(Alive)

Alive$perc_alive <- Alive$n/9*100



mod_growth_full <- lm(perc_alive~Treatment*Site, 
                       data = Alive)
summary(mod_growth_full)
M <- update(mod_growth_full, ~. - Treatment:Site)

summary(M)

Alive_sum <- Alive %>% 
  group_by(Treatment, Site) %>% 
  summarise(Mean = mean(perc_alive),  
            SD = sd(perc_alive),
            N = n(),
            SE = SD/sqrt(N-1))

ggplot(as.data.frame(Alive_sum), aes(x=as.factor(Site), y=Mean, fill=Treatment)) + 
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ggtitle ('Sep - December')+
  ylab("Percent survival (%)")+  
  xlab("Site")

Alive
# Plot for proposal
Alive_sum <- Alive %>% 
  group_by(Site) %>% 
  summarise(Mean = mean(perc_alive),  
            SD = sd(perc_alive),
            N = n(),
            SE = SD/sqrt(N-1))
ggplot(as.data.frame(Alive_sum), aes(x=as.factor(Site), y=Mean)) + 
  geom_bar(position=position_dodge(), stat="identity", fill = "white", colour = "black")+
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ggtitle ('Sep - December')+
  ylab("Percent survival (%)")+  
  xlab("Site")  


growth_dec[growth_dec$AliveOrDead=="L","AliveOrDead"] <- 1
growth_dec$survival <- as.numeric(growth_dec$AliveOrDead)
growth_dec$Treatment<-as.factor(growth_dec$Treatment)
growth_dec$Site<-as.factor(growth_dec$Site)
#growth_dec$survival


survive_sep <- growth_dec%>%
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
















