#hello world

library(reshape2)
library(dplyr)
library(ggplot2)
library(tibble)

# Set graphics pallate

library("ggsci")
par(mar = c(1, 3.5, 1, 1))
barplot(1:10, col = pal_jco()(10))
new.pal <- c(pal_jco()(10)[4],pal_jco()(10)[6],pal_jco()(10)[5],pal_jco()(10)[2],'grey57','grey80')


# Read in data

dir1 <- "~/Documents/School and jobs/Milford"
setwd(dir1)
filter_TSM.read <- read.csv('Surfclam_filter_sediment_tissue_data.csv', skip = 2, header = TRUE, stringsAsFactors = FALSE)
filter_TSM <- filter_TSM.read[1:40,1:18] #WILL NEED TO CHANGE THIS - WILL HAVE MORE ROWS
str(filter_TSM)

filter_TSM$Site[filter_TSM$Site=="Cockle cove"] <- "Chatham"
filter_TSM$Site[filter_TSM$Site=="Nobscusset"] <- "Nobscussett"

filter_TSM$Site <- as.factor(filter_TSM$Site)
filter_TSM$Date <- as.Date(filter_TSM$Date, format = "%m/%d/%y")

# Plot AFDW

grouped <- group_by(filter_TSM, Site, Date)
AFDW.mg.L.df <- summarise(grouped, mean=mean(AFDW.mg.L), sd=sd(AFDW.mg.L))
AFDW.mg.L.df <- as.data.frame(AFDW.mg.L.df)[2:nrow(AFDW.mg.L.df),]

str(AFDW.mg.L.df)

p<- ggplot(AFDW.mg.L.df, aes(x=Date, y=mean, group=Site, color=Site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))
p+labs(x="Date", y = "AFWD (mg/L)")+
  theme_classic() +
  scale_color_manual(values = new.pal)


# Plot TSM

str(filter_TSM)
grouped <- group_by(filter_TSM, Site, Date)
POM.mg.L.df <- summarise(grouped, mean=mean(POMmg.L), sd=sd(POMmg.L))
POM.mg.L.df <- as.data.frame(POM.mg.L.df)[2:nrow(POM.mg.L.df),]

str(POM.mg.L.df)

p<- ggplot(POM.mg.L.df, aes(x=Date, y=mean, group=Site, color=Site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))
p+labs(x="Date", y = "TSS (mg/L)")+
  theme_classic() +
  scale_color_manual(values = new.pal)

# Proportion organic content

# Plot organic fraction

str(filter_TSM)
grouped <- group_by(filter_TSM, Site, Date)
fraction_ash <- summarise(grouped, mean=mean(mg.ash.mg.POM), sd=sd(mg.ash.mg.POM))
fraction_org.df <- as.data.frame(fraction_ash)[2:nrow(fraction_ash),]

str(fraction_org.df)

p<- ggplot(fraction_org.df, aes(x=Date, y=1-mean, group=Site, color=Site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=1-(mean-sd), ymax=1-(mean+sd)), width=.2,
                position=position_dodge(0.05))
p+labs(x="Date", y = "Fraction organic (mg/mg)")+
  theme_classic() +
  scale_color_manual(values = new.pal)

