# #############################
# Developed by: André Dorigueto Canal
# Using R to make statistical inference on the data, from boxplots to ANOVAs

# #####################################################################################################################
# ############## [EXP 1] STATISTICS
# ############################# Loading
library(AICcmodavg)

rm(list=ls())
setwd("C:\\Users\\André\\Documents\\00_ITA\\00_Mestrado\\20_Data_Preparation")
dados = read.csv("Exp1_Stat.csv", header = TRUE)

ap = factor(dados$ap)
f = factor(dados$f)
vc = factor(dados$vc)

Ra = dados$Ra
Rsk = dados$Rsk
Rku = dados$Rku
RSm = dados$RSm
Rt = dados$Rt

# ############################# Test normality
library("ggpubr")
ggdensity(dados$Ra, xlab = "Ra")
ggdensity(dados$F, xlab = "F")
ggqqplot(dados$Ra)
ggqqplot(dados$F)

shapiro.test(dados$Ra)
shapiro.test(dados$F)

# ############################# Visualizing the data
head(dados)
length(dados$ap)

interaction.plot(ap, f, Ra)
plot (Ra, F)
plot(Ra~f)
plot(Ra~ap)
plot(Ra~vc)
boxplot(Ra~f)
boxplot(Ra~ap)
boxplot(Ra~vc)

boxplot(F~f)
boxplot(F~ap)
boxplot(F~vc)

plot(Block, ap)
boxplot(Ra~Block)
boxplot(Ra~SBlock)

boxplot(F~Block)
boxplot(F~SBlock)

hist(Ra, breaks = 25)

# ############################# Statistics to Ra
# Building FACTORIAL ANOVA
Exp1.Model.Ra.1 = aov(Ra~ap+f+vc)
summary(Exp1.Model.Ra.1)

Exp1.Model.Ra.2 = aov(Ra~ap*f+vc)
summary(Exp1.Model.Ra.2)

Exp1.Model.Ra.3 = aov(Ra~ap*f*vc)
summary(Exp1.Model.Ra.3)

plot(Ra~ap*f*vc)

# Using AIC test to decide which model is the best
library(AICcmodavg)   # Import library

model.set.ra <- list(Exp1.Model.Ra.1, Exp1.Model.Ra.2, Exp1.Model.Ra.3)        # Create data frame with the models
model.names.ra <- c("Exp1.ANOVA.Ra.1", "Exp1.ANOVA.Ra.2", "Exp1.ANOVA.Ra.3")   # Give column names

Exp1.Model.Ra.AIC.table = aictab(model.set.ra, modnames = model.names.ra)                 # Do AIC test
Exp1.Model.Ra.AIC.table
# write.csv(Exp1.Model.Ra.AIC.table, file = "Exp1_Model_Ra_AIC_table.csv")

# Check for homoscedasticity
png("Diagnosis plot Ra.png", width = 2200, height = 1300, res = 300)
par(mfrow=c(2,2), family = "serif")
plot(Exp1.Model.Ra.3)
dev.off()
par(mfrow=c(1,1))

# ############################# Statistics to F
# Building FACTORIAL ANOVA
rm(list=ls())
setwd("C:\\Users\\André\\Documents\\00_ITA\\00_Mestrado\\20_Data_Preparation")
dados = read.csv("Exp1_54_samples.csv", header = TRUE)

ap = factor(dados$ap)
f = factor(dados$f)
vc = factor(dados$vc)
ChipA = dados$ap * dados$f

F = dados$F
Fx = dados$Fx
Fy = dados$Fy
Fz = dados$Fz

# F
Exp1.ANOVA.F.1 = aov(F~ap+f+vc)
summary(Exp1.ANOVA.F.1)
Exp1.ANOVA.F.2 = aov(F~ap*f+vc)
summary(Exp1.ANOVA.F.2)
Exp1.ANOVA.F.3 = aov(F~ap*f*vc)
summary(Exp1.ANOVA.F.3)
Exp1.ANOVA.F.4 = aov(F~ap+f*vc)
summary(Exp1.ANOVA.F.4)
Exp1.ANOVA.F.5 = aov(F~ChipA)
summary(Exp1.ANOVA.F.5)

# Using AIC test to decide which model is the best
model.set.force <- list(Exp1.ANOVA.F.1, Exp1.ANOVA.F.2, Exp1.ANOVA.F.3, Exp1.ANOVA.F.4, Exp1.ANOVA.F.5)
model.names.force <- c("Exp1.ANOVA.F.1", "Exp1.ANOVA.F.2", "Exp1.ANOVA.F.3", "Exp1.ANOVA.F.4", "Exp1.ANOVA.F.5")

aictab(model.set.force, modnames = model.names.force)

# Model diagnosis
png("Diagnosis plot F.png", width = 2200, height = 1300, res = 300)
par(mfrow=c(2,2), family = "serif")
plot(Exp1.ANOVA.F.2)
dev.off()
par(mfrow=c(1,1))

# Fx
Exp1.ANOVA.Fx.1 = aov(Fx~ap+f+vc)
summary(Exp1.ANOVA.Fx.1)
Exp1.ANOVA.Fx.2 = aov(Fx~ap*f+vc)
summary(Exp1.ANOVA.Fx.2)
Exp1.ANOVA.Fx.3 = aov(Fx~ap*f*vc)
summary(Exp1.ANOVA.Fx.3)
Exp1.ANOVA.Fx.4 = aov(Fx~ap+f*vc)
summary(Exp1.ANOVA.Fx.4)
Exp1.ANOVA.Fx.5 = aov(Fx~ChipA)
summary(Exp1.ANOVA.Fx.5)

library(AICcmodavg)
model.set.force <- list(Exp1.ANOVA.Fx.1, Exp1.ANOVA.Fx.2, Exp1.ANOVA.Fx.3, Exp1.ANOVA.Fx.4, Exp1.ANOVA.Fx.5)
model.names.force <- c("Exp1.ANOVA.Fx.1", "Exp1.ANOVA.Fx.2", "Exp1.ANOVA.Fx.3", "Exp1.ANOVA.Fx.4", "Exp1.ANOVA.Fx.5")

aictab(model.set.force, modnames = model.names.force)

# Fy
Exp1.ANOVA.Fy.1 = aov(Fy~ap+f+vc)
summary(Exp1.ANOVA.Fy.1)
Exp1.ANOVA.Fy.2 = aov(Fy~ap*f+vc)
summary(Exp1.ANOVA.Fy.2)
Exp1.ANOVA.Fy.3 = aov(Fy~ap*f*vc)
summary(Exp1.ANOVA.Fy.3)
Exp1.ANOVA.Fy.4 = aov(Fy~ap+f*vc)
summary(Exp1.ANOVA.Fy.4)
Exp1.ANOVA.Fy.5 = aov(Fy~ChipA)
summary(Exp1.ANOVA.Fy.5)

library(AICcmodavg)
model.set.force <- list(Exp1.ANOVA.Fy.1, Exp1.ANOVA.Fy.2, Exp1.ANOVA.Fy.3, Exp1.ANOVA.Fy.4, Exp1.ANOVA.Fy.5)
model.names.force <- c("Exp1.ANOVA.Fy.1", "Exp1.ANOVA.Fy.2", "Exp1.ANOVA.Fy.3", "Exp1.ANOVA.Fy.4", "Exp1.ANOVA.Fy.5")

aictab(model.set.force, modnames = model.names.force)

# Fz
Exp1.ANOVA.Fz.1 = aov(Fz~ap+f+vc)
summary(Exp1.ANOVA.Fz.1)
Exp1.ANOVA.Fz.2 = aov(Fz~ap*f+vc)
summary(Exp1.ANOVA.Fz.2)
Exp1.ANOVA.Fz.3 = aov(Fz~ap*f*vc)
summary(Exp1.ANOVA.Fz.3)
Exp1.ANOVA.Fz.4 = aov(Fz~ap+f*vc)
summary(Exp1.ANOVA.Fz.4)
Exp1.ANOVA.Fz.5 = aov(Fz~ChipA)
summary(Exp1.ANOVA.Fz.5)

library(AICcmodavg)
model.set.force <- list(Exp1.ANOVA.Fz.1, Exp1.ANOVA.Fz.2, Exp1.ANOVA.Fz.3, Exp1.ANOVA.Fz.4, Exp1.ANOVA.Fz.5)
model.names.force <- c("Exp1.ANOVA.Fz.1", "Exp1.ANOVA.Fz.2", "Exp1.ANOVA.Fz.3", "Exp1.ANOVA.Fz.4", "Exp1.ANOVA.Fz.5")

aictab(model.set.force, modnames = model.names.force)
# ############################# Statistics to other roguhness parameters

# Rsk
Exp1.ANOVA.Rsk.1 = aov(Rsk~ap+f+vc)
summary(Exp1.ANOVA.Rsk.1)
Exp1.ANOVA.Rsk.2 = aov(Rsk~ap*f+vc)
summary(Exp1.ANOVA.Rsk.2)
Exp1.ANOVA.Rsk.3 = aov(Rsk~ap*f*vc)
summary(Exp1.ANOVA.Rsk.3)

library(AICcmodavg)   # Import library

model.set.rsk <- list(Exp1.ANOVA.Rsk.1, Exp1.ANOVA.Rsk.2, Exp1.ANOVA.Rsk.3)        # Create data frame with the models
model.names.rsk <- c("Exp1.ANOVA.Rsk.1", "Exp1.ANOVA.Rsk.2", "Exp1.ANOVA.Rsk.3")   # Give column names

Exp1.Model.Rsk.AIC.table = aictab(model.set.rsk, modnames = model.names.rsk)                 # Do AIC test
Exp1.Model.Rsk.AIC.table

# Rku
Exp1.ANOVA.Rku.1 = aov(Rku~ap+f+vc)
summary(Exp1.ANOVA.Rku.1)
Exp1.ANOVA.Rku.2 = aov(Rku~ap*f+vc)
summary(Exp1.ANOVA.Rku.2)
Exp1.ANOVA.Rku.3 = aov(Rku~ap*f*vc)
summary(Exp1.ANOVA.Rku.3)

model.set.rku <- list(Exp1.ANOVA.Rku.1, Exp1.ANOVA.Rku.2, Exp1.ANOVA.Rku.3)        # Create data frame with the models
model.names.rku <- c("Exp1.ANOVA.Rku.1", "Exp1.ANOVA.Rku.2", "Exp1.ANOVA.Rku.3")   # Give column names

Exp1.Model.Rku.AIC.table = aictab(model.set.rku, modnames = model.names.rku)                 # Do AIC test
Exp1.Model.Rku.AIC.table

# RSm
Exp1.ANOVA.RSm.1 = aov(RSm~ap+f+vc)
summary(Exp1.ANOVA.RSm.1)
Exp1.ANOVA.RSm.2 = aov(RSm~ap*f+vc)
summary(Exp1.ANOVA.RSm.2)
Exp1.ANOVA.RSm.3 = aov(RSm~ap*f*vc)
summary(Exp1.ANOVA.RSm.3)

model.set.rsm <- list(Exp1.ANOVA.RSm.1, Exp1.ANOVA.RSm.2, Exp1.ANOVA.RSm.3)        # Create data frame with the models
model.names.rsm <- c("Exp1.ANOVA.RSm.1", "Exp1.ANOVA.RSm.2", "Exp1.ANOVA.RSm.3")   # Give column names

Exp1.Model.RSm.AIC.table = aictab(model.set.rsm, modnames = model.names.rsm)                 # Do AIC test
Exp1.Model.RSm.AIC.table

# Rt
Exp1.ANOVA.Rt.1 = aov(Rt~ap+f+vc)
summary(Exp1.ANOVA.Rt.1)
Exp1.ANOVA.Rt.2 = aov(Rt~ap*f+vc)
summary(Exp1.ANOVA.Rt.2)
Exp1.ANOVA.Rt.3 = aov(Rt~ap*f*vc)
summary(Exp1.ANOVA.Rt.3)

model.set.rt <- list(Exp1.ANOVA.Rt.1, Exp1.ANOVA.Rt.2, Exp1.ANOVA.Rt.3)        # Create data frame with the models
model.names.rt <- c("Exp1.ANOVA.Rt.1", "Exp1.ANOVA.Rt.2", "Exp1.ANOVA.Rt.3")   # Give column names

Exp1.Model.Rt.AIC.table = aictab(model.set.rt, modnames = model.names.rt)                 # Do AIC test
Exp1.Model.Rt.AIC.table
