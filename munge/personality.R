#############################################################
#This file sets up datafiles and runs R analyses for
#Lucas & Donnellan (2010) longitudinal analysis of GSOEP data
#Start with raw item-level data from soepinfo and STATA
#############################################################
## This has been modified to extract personality data for
## tests of moderation of disability

##Start with STATA data file extraction program created by SOEPinfo
##Extract persnr, sex, gebjahr, 2005 items, 2009 items
##Then read this in to R for rest of processing
g <- read.dta("~/Documents/Datasets/GSOEP2009/gsoepBig5.dta")
g[g==c("-1")] <- NA

library(psych)

##Age
#g$age <- 2005-g$gebjahr
#g$ageCat <- as.integer(cut(g$age,breaks=seq(17,96,by=4),right=F))

#################################################################################
## Create scale scores
## Use Psych package
###########################
items1 <- g[,c(19:33)]
items2 <- g[,c(34:48)]

keys1 <- matrix(c(c(0,1,0,0,0,0,0,1,0,0,0,-1,0,0,0),
                 c(0,0,-1,0,0,1,0,0,0,0,0,0,1,0,0),
                 c(1,0,0,0,0,0,-1,0,0,0,1,0,0,0,0),
                 c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,-1),
                  c(0,0,0,1,0,0,0,0,1,0,0,0,0,1,0)),ncol=5)

keys2 <- matrix(c(c(0,1,0,0,0,0,0,1,0,0,0,-1,0,0,0),
                 c(0,0,-1,0,0,1,0,0,0,0,0,0,1,0,0),
                 c(1,0,0,0,0,0,-1,0,0,0,1,0,0,0,0),
                 c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,-1),
                 c(0,0,0,1,0,0,0,0,1,0,0,0,0,1,0)),ncol=5)
                
labels1 <- c("e05","a05","c05","n05","o05")
colnames(keys1) <- labels1
labels2 <- c("e09","a09","c09","n09","o09")
colnames(keys2) <- labels2

x1 <- scoreItems(keys1,items1,totals=FALSE,impute="none")$scores
x2 <- scoreItems(keys2,items2,totals=FALSE,impute="none")$scores
g <- cbind(g,x1,x2)




#################################################################################


##Find out which years each person participated in
temp <- sapply(g[,19:33],is.na)
missing05 <- rowMeans(temp)
g <- cbind(g,missing05)
temp2 <- sapply(g[,34:48],is.na)
missing09 <- rowMeans(temp2)
g <- cbind(g,missing09)

personality <- g[,c("persnr","e05","a05","c05","n05","o05",
                    "e09","a09","c09","n09","o09",
                    "missing05","missing09")]
cache('personality')


##Write final file to csv
#write.csv(g,"~/Documents/Datasets/GSOEP2009/gsoepBig5.csv",row.names=F)


#################################################################################
### End of Data Setup
#################################################################################
