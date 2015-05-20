## These scripts assume that basic models have been tested and initial dataframes
## for those analyses have already been constructed.

##################################################################################
##
## Run basic replication models with personality as a moderator
##
##################################################################################

select <- gotDisWide[which(gotDisWide$dis.dss$ST3=="%"),c("persnr", "notDisMin", "disMin", "DUR1", "DUR2")]
replication <- merge(disData, select, by="persnr")
replication <- replication[order(replication$persnr, replication$wave),]

replication$year <- as.numeric(replication$wave)
replication$linear <- 0
replication[which(replication$year>replication$disMin), "linear"] <-
    replication[which(replication$year>replication$disMin), "year"] -
    replication[which(replication$year>replication$disMin), "disMin"]

replication$yearBefore <- 0
replication[which(replication$year==replication$disMin-1),'yearBefore'] <- 1


## Merge personality data with replication data

## First Center Personality Data
center_apply <- function(x) {
    apply(x, 2, function(y) y - mean(y, na.rm=TRUE))
}

personality[,2:11] <- center_apply(personality[,2:11])

persModerator <- merge(replication, personality, by="persnr", all.x=TRUE)

## Find personality measure that is closest to event

persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"ext"] <-
    persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"e05"]
persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"agr"] <-
    persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"a05"]
persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"con"] <-
    persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"c05"]
persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"neu"] <-
    persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"n05"]
persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"ope"] <-
    persModerator[which(persModerator$disMin>2005&persModerator$disMin<=2009),"o05"]

persModerator[which(persModerator$disMin>2009),"ext"] <-
    persModerator[which(persModerator$disMin>2009),"e09"]
persModerator[which(persModerator$disMin>2009),"agr"] <-
    persModerator[which(persModerator$disMin>2009),"a09"]
persModerator[which(persModerator$disMin>2009),"con"] <-
    persModerator[which(persModerator$disMin>2009),"c09"]
persModerator[which(persModerator$disMin>2009),"neu"] <-
    persModerator[which(persModerator$disMin>2009),"n09"]
persModerator[which(persModerator$disMin>2009),"ope"] <-
    persModerator[which(persModerator$disMin>2009),"o09"]


# Model 1 -- Basic MLM without centering
model1.E <- lmer(ls ~ yearBefore + disIndicator + ext + ext*yearBefore + ext*disIndicator +
                     (1 + disIndicator | persnr), data=persModerator)
model1.N <- lmer(ls ~ yearBefore + disIndicator + neu + neu*yearBefore + neu*disIndicator +
                     (1 + disIndicator | persnr), data=persModerator)
model1.A <- lmer(ls ~ yearBefore + disIndicator + agr + agr*yearBefore + agr*disIndicator +
                     (1 + disIndicator | persnr), data=persModerator)
model1.C <- lmer(ls ~ yearBefore + disIndicator + con + con*yearBefore + con*disIndicator +
                     (1 + disIndicator | persnr), data=persModerator)
model1.O <- lmer(ls ~ yearBefore + disIndicator + ope + ope*yearBefore + ope*disIndicator +
                     (1 + disIndicator | persnr), data=persModerator)

model1.EO <- lmer(ls ~ yearBefore + disIndicator + ope + ext + ope*yearBefore + ext*yearBefore +
                      ope*disIndicator + ext*disIndicator +
                     (1 + disIndicator | persnr), data=persModerator)


# Model3 -- Basic MLM without centering plus linear
model3 <- lmer(ls ~ yearBefore + disIndicator + linear + (1 + disIndicator + linear | persnr), data=replication)
summary(model3)

model3.E <- lmer(ls ~ yearBefore + disIndicator + linear +
                     ext + ext:yearBefore + ext:disIndicator + ext:linear +
                         (1 + disIndicator + linear | persnr), data=persModerator)

model3.N <- lmer(ls ~ yearBefore + disIndicator + linear +
                     neu + neu:yearBefore + neu:disIndicator + neu:linear +
                         (1 + disIndicator + linear | persnr), data=persModerator)
