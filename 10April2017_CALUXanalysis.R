# R script to
# - read in CALUX data
# - explore & analyze data
# - generate figures for paper

# Carolyn Anderson May 2015

# Load necessary packages
library(ggplot2)
library(xtable) #http://blog.revolutionanalytics.com/2010/02/making-publicationready-tables-with-xtable.html
library(grid)
library(gridExtra)
#library(dplyr)
#library(agricolae)
#library(multcompView)
theme_set(theme_bw())

setwd("C:/Users/Carolyn/Desktop/Data")

# Read in the data
CALUX <- read.csv("3July2015_CALUX_data.csv", stringsAsFactors=FALSE, skip=1)[c(1:60),] #not reading in blanks/etc
CALUX.bc <- read.csv("3July2015_CALUX_data.csv", stringsAsFactors = FALSE, skip=1)
Plants <- read.csv("3July2015_PlantGrowth_data.csv")[c(0:60),]

pH <- read.csv("pH.csv", skip=3)
uptake <- read.csv("Greenhouse plant uptake data_150720_CGA.csv")

# -----------------------------------------------------------------------------
## Prepping the data
# -----------------------------------------------------------------------------
# 1. CALUX
# Setting up the data frame
# Expanding "Description" to two columns (biochar and biosolids)
tmp <- strsplit(CALUX$BC.BS, "-")
cols <- t(sapply(tmp,c))
CALUX[,c("BC","BS")] <- cols

# Plant, BC, BS as factors
CALUX$Plant <- as.factor(CALUX$Plant)
CALUX$BC <- as.factor(CALUX$BC)
CALUX$BS <- as.factor(CALUX$BS)
# Take out unnecessary column names (2, 9-10)
CALUX <- CALUX[ ,-c(2,9,10)]

# 2. Plant growth
# Plant, BC, BS as factors
Plants$Plant <- as.factor(Plants$Plant)
Plants$BC <- as.factor(Plants$BC)
Plants$BS <- as.factor(Plants$BS)
# Take out unnecessary column names
Plants <- Plants[ ,-c(12:21)]

# 3. pH
pH$treatment <- as.character(pH$treatment)
tmp2 <- strsplit(pH$treatment, "-")
cols <- t(sapply(tmp2,c))
pH[,c("BC","BS")] <- cols
# Get averages for each treatment
pH.av <- aggregate(pH ~ BS + BC, pH, mean)

# 4. uptake
#remove replicated columns (5-11,18)
uptake <- uptake[ ,-c(5:11,18)]

# -----------------------------------------------------------------------------
# Merge CALUX, plant, pH data, and uptake
total <- merge(CALUX, Plants, by=c("Plant", "BC", "BS", "Rep"))
total <- merge(total, pH.av, by=c("BC", "BS"))
total <- merge(total, uptake, by=c("Plant", "BC", "BS", "Rep"))

Lettuce <- subset(total, Plant=="Lettuce")
Carrot <- subset(total, Plant=="Carrot")
Biosolids <- subset(total, BS==10)

# -----------------------------------------------------------------------------
# Experimental design
# - Factor: two levels of (spiked) biosolids (0 and 20 t/ha); 0 biosolids received equivalent N in mineral fertilizer
# - Treatment: three levels of biochar (0, 10, 100 t/ha)
# - Two different plants (lettuce and carrot)
# - 5 reps for each plant*treatment combination (independent replicates: these were separate pots)

# Data
# - CALUX bioassay
# - LC/MS/MS analysis (recovery efficiency of CALUX extraction)
# - plant growth (roots/shootS)
# - plant uptake (in triplicate)
# - pH (in triplicate)

# Analyses
# - CALUX: focus on AhR
# - plant growth
# - LC/MS/MS recoveries
# - pH
# - plant uptake

# Statistics
# https://people.richland.edu/james/lecture/m170/ch13-2wy.html
# http://www.stat.columbia.edu/~martin/W2024/R8.pdf
# http://www.r-bloggers.com/two-way-analysis-of-variance-anova/
# - Run ANOVAs (continuous dependent measure, categorical predictors)
# - Two-way independent (two factors, each observations are independent) - 6 treatment combinations
#   - Main effect: Only looking at e.g. 0 BS and different levels of BC, or 10 BS and different levels of BC (NO interactions)
#   - Interaction effect: The effect that one factor has on the other factor (this can answer whether BC "mitigates" BS)
#   - Within variation: sum of squares within each treatment group; n-1; 

# Boxplot info:
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# -----------------------------------------------------------------------------
# FIGURES: CALUX
# -----------------------------------------------------------------------------
# Boxplots
# AhR vs. different biosolids/biochar treatments
ggplot(total, aes(x=BC, y=AhR)) +
  geom_boxplot() +
  xlab("Biochar") + ylab("Relative response to AhR standard")

###FINAL: AhR in different BS/BC treatments (combined lettuce & carrot soils)
ggplot(total, aes(x=BS, y=AhR, fill=BC)) +
  geom_boxplot() +
  xlab(expression(Biosolids~(t~ha^{-1}))) + ylab("AhR-CALUX response (percent of TCDD)") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_grey(start=0, end=0.9, name=expression(Biochar~(t~ha^{-1}))) # greyscale

# Regressing AhR vs. plant growth: No significant effect of AhR activity on root/shoot mass.
plot3 <- ggplot(subset(total)) +
  aes(x=AhR, y=Shoot_lyoph) +
  geom_point() +
#  geom_smooth() +
  xlab("") + ylab("Shoot biomass (g)") + #("Relative response to AhR standard")
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=18)) +
  facet_wrap(~Plant, scales="free")

summary(lm(AhR ~ Shoot_lyoph, data=total[total$Plant=="Lettuce",]))
summary(lm(AhR ~ Shoot_lyoph, data=total[total$Plant=="Carrot",]))


plot4 <- ggplot(subset(total)) +
  aes(x=AhR, y=Root_lyoph) +
  geom_point() +
#  geom_smooth() +
  xlab("AhR-CALUX response (percent of TCDD)") + ylab("Root biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=18)) +
  facet_wrap(~Plant, scales="free")

summary(lm(AhR ~ Root_lyoph, data=total[total$Plant=="Lettuce",]))
summary(lm(AhR ~ Root_lyoph, data=total[total$Plant=="Carrot",]))


grid.newpage()
grid.draw(rbind(ggplotGrob(plot3), ggplotGrob(plot4), size = "last"))





# Plant growth (root/shoot) vs. BS/BC treatments
##FINAL Lettuce:
ggplot(subset(total, Plant %in% "Lettuce")) +
  aes(x=BS, y=Root_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids (t/ha)") + ylab("Lettuce root biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_discrete(name="Biochar (t/ha)")

ggplot(subset(total, Plant %in% "Lettuce")) +
  aes(x=BS, y=Shoot_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids (t/ha)") + ylab("Lettuce shoot biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_discrete(name="Biochar (t/ha)")

#FINAL Carrot:
ggplot(subset(total, Plant %in% "Carrot")) +
  aes(x=BS, y=Root_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids (t/ha)") + ylab("Carrot root biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_discrete(name="Biochar (t/ha)")

ggplot(subset(total, Plant %in% "Carrot")) +
  aes(x=BS, y=Shoot_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids (t/ha)") + ylab("Carrot shoot biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_discrete(name="Biochar (t/ha)")

# Shoots, by plant
# Change order for facet_wrap (lettuce, then carrot -- this is the order in the ms)
total$Plant <- factor(total$Plant, levels = c("Lettuce","Carrot"))

plot1 <- ggplot(subset(total)) +
  aes(x=BS, y=Shoot_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("") + ylab("Shoot biomass (g)") + #xlab(expression(Biosolids~(t~ha^{-1})))
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=18)) +
  scale_fill_grey(start=0, end=0.9, name=expression(Biochar~(t~ha^{-1}))) + # greyscale
  facet_wrap(~Plant, scales="free")


# Roots, by plant
plot2 <- ggplot(subset(total)) +
  aes(x=BS, y=Root_lyoph, fill=BC) +
  geom_boxplot() +
  xlab(expression(Biosolids~(t~ha^{-1}))) + ylab("Root biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=18)) +
  scale_fill_grey(start=0, end=0.9, name=expression(Biochar~(t~ha^{-1}))) + # greyscale
  facet_wrap(~Plant, scales="free")

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

# -----------------------------------------------------------------------------
# STATISTICS: CALUX
# -----------------------------------------------------------------------------
# Summary tables of AhR
ag.ahr <- do.call(data.frame, aggregate(AhR ~ BS + BC, total, function(x) c(mean=mean(x), sd = sd(x))))
ag.ahr.bs <- do.call(data.frame, aggregate(AhR ~ BS, total, function(x) c(mean=mean(x), sd = sd(x))))
ag.ahr.bc <- do.call(data.frame, aggregate(AhR ~ BC, total, function(x) c(mean=mean(x), sd = sd(x))))

# ANOVAS on CALUX

# Two-way ANOVAs, both plants
lm1 <- lm(AhR ~ BS + BC, total)
anova(lm1)
print(xtable(anova(lm1)), type="html", file="lm1.html")
layout(matrix(c(1,2,3,4),2,2,))
plot(lm1)
tukey1 <- TukeyHSD(aov(AhR ~ BS + BC, total), conf.level=0.95)
plot(tukey1)
summary(lm1)


# Both plants combined: Found that interaction is not significant, so it will not be included in the model

# Summary stats for AhR activity (both plants combined)
AhRsum <- aggregate(AhR ~ BS + BC, total, mean)
aggregate(AhR ~ BS, total, mean)
aggregate(AhR ~ BC, total, mean)

## Two-way ANOVA, lettuce treatments
# No interaction
lm3 <- lm(AhR ~ BS + BC, Lettuce)
anova(lm3)
layout(matrix(c(1,2,3,4),2,2,))
plot(lm3)
tukey3 <- TukeyHSD(aov(AhR ~ BS + BC, Lettuce), conf.level=0.95)
plot(tukey3)

# With interaction
lm4 <- lm(AhR ~ BS * BC, Lettuce)
anova(lm4) # interaction is significant!
layout(matrix(c(1,2,3,4),2,2,))
plot(lm4)
tukey4 <- TukeyHSD(aov(AhR ~ BS * BC, Lettuce), conf.level=0.95)
plot(tukey4)


# Summary stats for AhR activity (only lettuce)
AhRsum.lettuce <- aggregate(AhR ~ BS + BC, Lettuce, mean)
aggregate(AhR ~ BS, Lettuce, mean)
aggregate(AhR ~ BC, Lettuce, mean)



## Two-way ANOVA, carrot treatments
# No interaction
lm5 <- lm(AhR ~ BS + BC, Carrot)
anova(lm5)
layout(matrix(c(1,2,3,4),2,2,))
plot(lm5)
tukey5 <- TukeyHSD(aov(AhR ~ BS + BC, Carrot), conf.level=0.95)
plot(tukey5)

# With interaction
lm6 <- lm(AhR ~ BS * BC, CALUX_Car)
anova(lm6) # interaction is not significant!
layout(matrix(c(1,2,3,4),2,2,))
plot(lm6)
tukey6 <- TukeyHSD(aov(AhR ~ BS * BC, Carrot), conf.level=0.95)
plot(tukey6)


# -----------------------------------------------------------------------------
## Pairwise t-tests for CALUX
#What significant differences are present amongst the biosolids treatment means?
#Shows that biosolids means are significantly different, p=0.024
pairwise.t.test(total$AhR, total$BS, p.adj="none")

#What significant differences are present amongs the biochar treatment means?
#Shows that significant differences between 0-100 and 10-100 (but not 0-10)
pairwise.t.test(total$AhR, total$BC, p.adj="none")


# -----------------------------------------------------------------------------
# FIGURES: Plant growth
# -----------------------------------------------------------------------------
# Boxplots
# Plant growth vs. different biosolids/biochar treatments
# Could also switch these so biochar is on the x-axis... play around and see if you want to.
# Could also present bar charts (see link at top of script)

# Lettuce
ggplot(subset(total, Plant %in% "Lettuce")) +
  aes(x=BS, y=Root_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids level (t/ha)") + ylab("Root biomass (g)") +
  ggtitle("Root growth in lettuce treatment soils")

ggplot(subset(total, Plant %in% "Lettuce")) +
  aes(x=BS, y=Shoot_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids level (t/ha)") + ylab("Shoot biomass (g)") +
  ggtitle("Shoot growth in lettuce treatment soils")

# Carrot
ggplot(subset(total, Plant %in% "Carrot")) +
  aes(x=BS, y=Root_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids level (t/ha)") + ylab("Root biomass (g)") +
  ggtitle("Root growth in carrot treatment soils")

ggplot(subset(total, Plant %in% "Carrot")) +
  aes(x=BS, y=Shoot_lyoph, fill=BC) +
  geom_boxplot() +
  xlab("Biosolids level (t/ha)") + ylab("Shoot biomass (g)") +
  ggtitle("Shoot growth in carrot treatment soils")

ggplot(total) +
  aes(x=BS, y=Root_lyoph, fill=BC) +
  geom_boxplot() +
  facet_wrap(~Plant, scales="free")

# -----------------------------------------------------------------------------
# STATISTICS: Plant growth
# -----------------------------------------------------------------------------
# - I should test the assumptions of normality, etc. on my data. Look at the R files from class, also online.
# - I should also separate these by plants, perhaps?

# Summary tables of Plant growth
ag.lettuce.root <- do.call(data.frame, aggregate(Root_lyoph ~ BS + BC, Lettuce, function(x) c(mean=mean(x), sd = sd(x))))
ag.lettuce.root.bs <- do.call(data.frame, aggregate(Root_lyoph ~ BS, Lettuce, function(x) c(mean=mean(x), sd = sd(x))))
ag.lettuce.root.bc <- do.call(data.frame, aggregate(Root_lyoph ~ BC, Lettuce, function(x) c(mean=mean(x), sd = sd(x))))

ag.lettuce.shoot <- do.call(data.frame, aggregate(Shoot_lyoph ~ BS + BC, Lettuce, function(x) c(mean=mean(x), sd = sd(x))))
ag.lettuce.shoot.bs <- do.call(data.frame, aggregate(Shoot_lyoph ~ BS, Lettuce, function(x) c(mean=mean(x), sd = sd(x))))
ag.lettuce.shoot.bc <- do.call(data.frame, aggregate(Shoot_lyoph ~ BC, Lettuce, function(x) c(mean=mean(x), sd = sd(x))))

ag.carrot.root <- do.call(data.frame, aggregate(Root_lyoph ~ BS + BC, Carrot, function(x) c(mean=mean(x), sd = sd(x))))
ag.carrot.root.bs <- do.call(data.frame, aggregate(Root_lyoph ~ BS, Carrot, function(x) c(mean=mean(x), sd = sd(x))))
ag.carrot.root.bc <- do.call(data.frame, aggregate(Root_lyoph ~ BC, Carrot, function(x) c(mean=mean(x), sd = sd(x))))

ag.carrot.shoot <- do.call(data.frame, aggregate(Shoot_lyoph ~ BS + BC, Carrot, function(x) c(mean=mean(x), sd = sd(x))))
ag.carrot.shoot.bs <- do.call(data.frame, aggregate(Shoot_lyoph ~ BS, Carrot, function(x) c(mean=mean(x), sd = sd(x))))
ag.carrot.shoot.bc <- do.call(data.frame, aggregate(Shoot_lyoph ~ BC, Carrot, function(x) c(mean=mean(x), sd = sd(x))))



## Two-way ANOVA, lettuce treatments
# No interaction
lm7 <- lm(Root_lyoph ~ BS + BC, Lettuce) #interaction not significant
anova(lm7)
lm7.tukey <- TukeyHSD(aov(Root_lyoph ~ BS + BC, Lettuce), conf.level=0.95)
lm7.tukey

lm8 <- lm(Shoot_lyoph ~ BS + BC, Lettuce) #interaction not significant
anova(lm8)
lm8.tukey <- TukeyHSD(aov(Shoot_lyoph ~ BS + BC, Lettuce), conf.level=0.95)
lm8.tukey

## Two-way ANOVA, carrot treatments
# No interaction
lm9 <- lm(Root_lyoph ~ BS + BC, Carrot) #interaction not significant
anova(lm9)
lm9.tukey <- TukeyHSD(aov(Root_lyoph ~ BS + BC, Carrot), conf.level=0.95)
lm9.tukey

lm10 <- lm(Shoot_lyoph ~ BS + BC, Carrot) #interaction not significant
anova(lm10)
lm10.tukey <- TukeyHSD(aov(Shoot_lyoph ~ BS + BC, Carrot), conf.level=0.95)
lm10.tukey
