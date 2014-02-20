# Steps to run analyses: Run these first commands to create basic files and variables. Then
# Additional analyses can be run separately

disData <- merge(ls, dis, by=c(idName, "wave"))
disData <- merge(disData, extent, by=c(idName, "wave"), all.x=TRUE)

# Create Indicator
disData$disIndicator <- NA
disData[which(disData$dis==2),"disIndicator"] <- 0
disData[which(disData$dis==1),"disIndicator"] <- 1

# Aggregate disability status to find percentage of years disabled
disDataAgg <- aggregate(disData$disIndicator, by=list(disData$persnr), FUN=mean, na.rm=TRUE)
names(disDataAgg) <- c("persnr", "percentDis")
disData <- merge(disData, disDataAgg, by="persnr")
disData$disIndicator.C <- disData$disIndicator-disData$percentDis


#--------------------------------------------------------------------------------------------#
# First analysis. Simple model with all participants examining effect of disability #
#--------------------------------------------------------------------------------------------#


# Baseline Model
baseline <- lmer(ls ~ 1 + (1 | persnr), data=disData)

# Model 1 -- Basic MLM without centering
model1a <- lmer(ls ~ disIndicator + (1 + disIndicator | persnr), data=disData)

# Model 2 -- Centered disability + level 2 percentage: Equivalent to fixed effects
model2a <- lmer(ls ~ disIndicator.C + percentDis + (1 + disIndicator.C | persnr), data=disData)

summary(model1a)
summary(model2a)

#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------#
# Replicate old model; only select those who start disabled and stay disabled; stay in study
# for at least three years of disability
#--------------------------------------------------------------------------------------------#

select <- gotDisWide[which(gotDisWide$dis.dss$ST3=="%"&gotDisWide$DUR2>=3),c("persnr", "notDisMin", "disMin", "DUR1", "DUR2")]
replication <- merge(disData, select, by="persnr")
replication <- replication[order(replication$persnr, replication$wave),]

replication$year <- as.numeric(replication$wave)
replication$linear <- 0
replication[which(replication$year>replication$disMin), "linear"] <-
    replication[which(replication$year>replication$disMin), "year"] -
    replication[which(replication$year>replication$disMin), "disMin"]

replication$yearBefore <- 0
replication[which(replication$year==replication$disMin-1),'yearBefore'] <- 1

# Baseline Model
baseline <- lmer(ls ~ 1 + (1 | persnr), data=replication)

# Model 1 -- Basic MLM without centering
model1 <- lmer(ls ~ yearBefore + disIndicator + (1 + disIndicator | persnr), data=replication)

# Model 2 -- Centered disability + level 2 percentage: Equivalent to fixed effects
model2 <- lmer(ls ~ yearBefore + disIndicator.C + percentDis + (1 + disIndicator.C | persnr), data=replication)

summary(model1)
summary(model2)



# Model3 -- Basic MLM without centering plus linear
model3 <- lmer(ls ~ yearBefore + disIndicator + linear + (1 + disIndicator + linear | persnr), data=replication)
summary(model3)

# Model4 -- MLM with centering
# Create new average linear variable
disDataAgg2 <- aggregate(replication$linear, by=list(replication$persnr), FUN=mean, na.rm=TRUE)
names(disDataAgg2) <- c("persnr", "meanLinear")
replication <- merge(replication, disDataAgg2, by="persnr")
replication$linear.C <- replication$linear-replication$meanLinear

model4 <- lmer(ls ~ yearBefore + disIndicator.C + linear.C + percentDis + meanLinear + (1 + disIndicator.C + linear.C | persnr), data=replication)
summary(model4)


# Model5 -- check for effect of general linear decline
replication$adjLS <- replication$ls-.03*(replication$year-replication$notDisMin)
model5 <- lmer(adjLS ~ disIndicator.C + linear.C + percentDis + meanLinear + (1 + disIndicator.C + linear.C | persnr), data=replication)
summary(model5)

                                        #--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
# Compare those who stayed disabled for three years (from above) to those who 
# recovered.                                        #--------------------------------------------------------------------------------------------#

select2 <- gotDisWide[which(gotDisWide$dis.dss$ST3=="2"&gotDisWide$dis.dss$ST4=="%"),c("persnr", "notDisMin", "disMin", "DUR1", "DUR2", "DUR3")]
comparison <- merge(disData, select2, by="persnr")
comparison <- comparison[order(comparison$persnr, comparison$wave),]

comparison$year <- as.numeric(comparison$wave)
comparison$linear <- 0
comparison$spell1End <- comparison$disMin+comparison$DUR2-1
comparison[which(comparison$year>comparison$disMin&comparison$year<=comparison$spell1End), "linear"] <-
    comparison[which(comparison$year>comparison$disMin&comparison$year<=comparison$spell1End), "year"] -
    comparison[which(comparison$year>comparison$disMin&comparison$year<=comparison$spell1End), "disMin"]

comparison$yearBefore <- 0
comparison[which(comparison$year==comparison$disMin-1),'yearBefore'] <- 1


# Model 1 -- Basic MLM without centering
model1c <- lmer(ls ~ yearBefore + disIndicator + (1 + disIndicator | persnr), data=comparison)
summary(model1c)

# Model 2 -- Centered disability + level 2 percentage: Equivalent to fixed effects
model2c <- lmer(ls ~ yearBefore + disIndicator.C + percentDis + (1 + disIndicator.C | persnr), data=comparison)
summary(model2c)


# Model3 -- Basic MLM without centering plus linear
model3c <- lmer(ls ~ yearBefore + disIndicator + linear + (1 + disIndicator + linear | persnr), data=comparison)
summary(model3c)

# Model4 -- MLM with centering
# Create new average linear variable
disDataAgg3 <- aggregate(comparison$linear, by=list(comparison$persnr), FUN=mean, na.rm=TRUE)
names(disDataAgg3) <- c("persnr", "meanLinear")
comparison <- merge(comparison, disDataAgg3, by="persnr")
comparison$linear.C <- comparison$linear-comparison$meanLinear

model4 <- lmer(ls ~ disIndicator.C + linear.C + percentDis + meanLinear + (1 + disIndicator.C + linear.C | persnr), data=comparison)
summary(model4)


# Model5 -- check for effect of general linear decline
replication$adjLS <- replication$ls-.03*(replication$year-replication$notDisMin)
model5 <- lmer(adjLS ~ disIndicator.C + linear.C + percentDis + meanLinear + (1 + disIndicator.C + linear.C | persnr), data=replication)
summary(model5)


#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#



disData <- merge(disData, disabilityCombined, by=idName)
disData <- disData[order(disData$persnr, disData$wave),]
