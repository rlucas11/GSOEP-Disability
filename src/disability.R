disData <- merge(ls, dis, by=c(idName, "wave"))
disData <- merge(disData, extent, by=c(idName, "wave"), all.x=TRUE)
disData <- merge(disData, disabilityCombined, by=idName)
disData <- disData[order(disData$persnr, disData$wave),]
