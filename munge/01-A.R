# Get Variables for Analysis
lsVarNames <- c("AP6801","BP9301","CP9601","DP9801","EP89","FP108","GP109","GP6401E","HP10901",
                "IP10901","JP10901","KP10401","LP10401","MP11001","NP11701","OP12301","PP13501",
                "QP14301","RP13501","SP13501","TP14201","UP14501","VP154","WP142","XP149","YP15501",
                "ZP15701","BAP160","BBP15201")
ls <- get_variable(lsVarNames, "ls")
lsRecodeString <- "-10:-1=NA"
ls$ls <- iRecode(ls$ls, lsRecodeString)
cache('ls')

disVarNames <-c("AP5201","BP7401","CP74B01","DP0601","EP6601","FP8201",NA,NA,"HP7701","IP7801",
                NA,"KP8401","LP9101","MP7701","NP8101","OP7001","PP9701","QP9701","RP9701",
                "SP9501","TP9901","UP9201","VP10501","WP9601","XP9901","YP10901","ZP9601",
                "BAP9801","BBP10101")
dis <- get_variable(disVarNames, "dis")
disRecodeString <- "-10:-1=NA"
dis$dis <- iRecode(dis$dis, disRecodeString)
cache('dis')
disWide <- dcast(dis, persnr ~ wave)
cache('disWide')

## Note: Remove HP770 (East German) because it is a different question than in other waves.
extentVarNames <- c("AP5202","BP7402","CP74B02","DP0602","EP6602","FP8202",NA,NA,"HP7702","HP77O","IP7802",
                    NA,"KP8402","LP9102","MP7702","NP8102","OP7002","PP9702","QP9702","RP9702",
                    "SP9502","TP9902","UP9202","VP10502","WP9602","XP9902","YP10902","ZP9602",
                    "BAP9802","BBP10102")
extentVarNames <- extentVarNames[-10]
extent <- get_variable(extentVarNames, "extent")
extentRecodeString <- "-10:-1=NA; 101:hi"
extent$extent <- iRecode(extent$extent, extentRecodeString)
extent <- na.omit(extent)
cache('extent')

# -------------------- find people who became disabled -------------------- #
notDisabledYear <- firstStatus(dis, "dis", 2, "notDisMin")
disabledYear <- firstStatus(dis, "dis", 1, "disMin")
lastDisStatus <- lastStatus(dis, "dis", 2, "notDisMax")
disabilityCombined <- merge(notDisabledYear, disabledYear, by=idName)
disabilityCombined <- merge(disabilityCombined, lastDisStatus, by = idName)

# select those who got disabled
gotDisabled <- disabilityCombined[which(disabilityCombined[,"notDisMin"]<
                                        disabilityCombined[,"disMin"]),]
cache('gotDisabled')

# select those who got disabled and stayed disabled
stayedDisabled <- disabilityCombined[which(disabilityCombined[,"notDisMin"]<
                                               disabilityCombined[,"disMin"] &
                                               disabilityCombined[,"notDisMax"]<
                                               disabilityCombined[,"disMin"]),]

# -------------------- wide file for those who got disabled -------------------- #
gotDisWide <- merge(disWide, gotDisabled, by = idName)

# -------------------- set up transitions with TraMineR -------------------- #
gotDisWide$seq <- seqconc(gotDisWide[,2:27], void='')
dis.seq <- seqdef(gotDisWide, 31, labels = c("disabled","notDisabled"))
gotDisWide$dis.seq <- dis.seq
transition <- seqetm(dis.seq, method="transition")
#transition <- transition[1:2,1:2]
dis.tse <- seqformat(gotDisWide[,32], from="STS", to = "TSE", tevent=transition)
# Number of transitions
# TraMineR doesn't handle missing data well, so we have to compare two strategies to
# identify transitions that do not occur around a missing occasion
dis.dss <- seqdss(dis.seq)
gotDisWide$dis.dss <- dis.dss
gotDisWide$nTransitions <- seqlength(gotDisWide$dis.dss)-1
dis.agg <- aggregate(dis.tse$id, by=list(dis.tse$id), FUN=length)
names(dis.agg) <- c(idName, "dis.agg")
dis.agg$persnr <- as.integer(dis.agg$persnr)
gotDisWide <- merge(gotDisWide, dis.agg, by = idName, sort=TRUE)
