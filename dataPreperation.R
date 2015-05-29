#load libraries
library(RSQLite)
library(car)
library(Imap)
library(plyr)
library(maps)
library(sp)
library(maptools)
library(geosphere)
library(rgdal)
library(foreign)
library(psych)
library(lme4)
library(arm)
library(gstat)
library(ggplot2)
library(Amelia)
library(survey)
library(mitools)

#set working directory
setwd("/Users/phparker/Dropbox/Databases/LSAY_DATSETS")
# Extract 2003 data
LSAY <- dbConnect(SQLite(), dbname="LSAY.sqlite")
#dbListTables(LSAY)
#Extract aspirations and postcodes
d2003 <- dbGetQuery(LSAY, "SELECT Student2003.w_fstuwt, Student2003.id, Student2003.schoolid, Student2003.laa005,
					Student2003.pv1math, Student2003.pv1read, Student2003.pv1scie, 
					Student2003.pv2math, Student2003.pv2read, Student2003.pv2scie, 
					Student2003.pv3math, Student2003.pv3read, Student2003.pv3scie, 
					Student2003.pv4math, Student2003.pv4read, Student2003.pv4scie, 
					Student2003.pv5math, Student2003.pv5read, Student2003.pv5scie, 
					Student2003.escs, Student2003.indig, Student2003.sex, 
					Student2003.LCCA16 as uniInt1,Student2003.LDCA16 as uniInt2,
					Student2003.LECA016 as uniInt3,
					Student2003.xbac2005 as uni1, Student2003.xbac2006 as uni2, Student2003.xbac2007 as uni3,
					Student2003.stateid as state, Student2003.loc,
					pc2003.PC2003
					FROM Student2003 JOIN pc2003 ON Student2003.id = pc2003.STIDSTD")

d2003$laa005 <- ifelse(d2003$laa005 == 1,1,0)
d2003$escs <- recode(d2003$escs, "999=NA")
#d2003$ach <- principal(d2003[,grep("pv1.+", names(d2003))], 1)$scores
d2003$loc <- recode(d2003$loc,recodes = "1:2=1;3:6=2;7:8=3")

call.brr <-  paste0( 'w_fstr' , 1:80, collapse = "," ) 

brr.2003 <- dbGetQuery( LSAY , paste( "select id,", call.brr, "  from" , "Student2003" ) )

#Extract 2006 data
d2006 <- dbGetQuery(LSAY, "SELECT Student2006.w_fstuwt, Student2006.studenti as id, Student2006.schoolid, 
					Student2006.st48n01 as laa005,
					Student2006.pv1math, Student2006.pv1read, Student2006.pv1scie,
					Student2006.pv2math, Student2006.pv2read, Student2006.pv2scie,
					Student2006.pv3math, Student2006.pv3read, Student2006.pv3scie,
					Student2006.pv4math, Student2006.pv4read, Student2006.pv4scie,
					Student2006.pv5math, Student2006.pv5read, Student2006.pv5scie,
					Student2006.escs, Student2006.indig, Student2006.st04q01 as sex,
					Student2006.LCCA016 as uniInt1,Student2006.LDCA016 as uniInt2,Student2006.LECA016 as uniInt3,
					Student2006.xbac2008 as uni1, Student2006.xbac2009 as uni2, Student2006.xbac2010 as uni3,
					Student2006.state, Student2006.geoloc_3 as loc,
					pc2006.PC2006 as PC2003
					FROM Student2006 JOIN pc2006 ON Student2006.studenti = pc2006.STUDENID")
d2006$laa005 <- recode(d2006$laa005, "99=NA")
d2006$laa005 <- ifelse(d2006$laa005 == 1,1,0)
d2006$escs <- recode(d2006$escs, "997=NA; 999=NA")
#d2006$ach <- principal(d2006[,grep("pv1.+", names(d2006))], 1)$scores

brr.2006 <- dbGetQuery( LSAY , paste( "select studenti as id,", call.brr, "  from" , "Student2006" ) )

#link 2003 and 2006
myData <- rbind.data.frame(d2003, d2006)
brr <- rbind.data.frame(brr.2003, brr.2006)

myData$cohort <- c(rep(-1, nrow(d2003)), rep(1,nrow(d2006)))
rm(d2003); rm(d2006)
myData$inUni <- apply(myData[,c("uni1", "uni2", "uni2")],1, function(x) any(x < 5, na.rm=FALSE))
myData$inUni <- as.numeric(myData$inUni)
myData$sex <- recode(myData$sex, "9=NA") - 1

#Construct distance Measures
distanceTable <- read.csv("~/Dropbox/Projects_Research/distancePaper/Uni_PCode_Matches_CrossTab.csv")
distanceAny <- rep(NA, nrow(myData))
distanceGo8 <- rep(NA, nrow(myData))
proximityAny <- rep(NA, nrow(myData))#set to 20km
proximityGo8 <- rep(NA, nrow(myData))#set to 20km
for (i in 1:nrow(myData)){
	if(i %% 1000 == 0)print(i)
	if(is.na(myData$PC2003[i])){
		next
		}else{
		temp <- which(gsub("X", "", names(distanceTable) ) == as.character(myData$PC2003[i]))
			if(length(temp)<1){
				next
				}else{
				distanceAny[i] <- min(distanceTable[,temp], na.rm=TRUE)
				distanceGo8[i] <- min(distanceTable[distanceTable$Go8 == 1,temp], na.rm=TRUE)
				proximityAny[i] <- sum(distanceTable[,temp] < 20000, na.rm = TRUE)
				proximityGo8[i] <- sum(distanceTable[distanceTable$Go8 == 1,temp] < 20000, na.rm = TRUE)
				}
		}
}

myData$dAny <- distanceAny
myData$dGo8 <- distanceGo8
myData$dGo8 <- recode(myData$dGo8, "Inf = NA")
myData$pAny <- proximityAny
myData$pGo8 <- proximityGo8
#add census data for 
census <- dbConnect(SQLite(), dbname="/Users/phparker/Dropbox/Databases/census/census-postcode")
SES <- dbGetQuery(census, "SELECT * FROM SES")
myData <- merge(myData, SES, by.x = "PC2003", by.y = "Postal Area",  all.x = TRUE)
myData$IEO <- as.numeric(myData$IEO)

load("/Users/phparker/Dropbox/Databases/LSAY_DATSETS/institutions.RData")
myData <- merge(myData, institution[,c("id", "go8")], by = "id")


#EGP translation

#myData <- myData[myData$state != 7,]

MI <- amelia(myData[,-c(24:29, 38:40)],m = 5, idvars = c("id","PC2003", "schoolid", "w_fstuwt",
											 "pv2math", "pv2read", "pv2scie", "pv3math",
											 "pv3read", "pv3scie", "pv4math", "pv4read",
											 "pv4scie", "pv5math", "pv5read", "pv5scie"),
			 ords = c("laa005", "pAny", "pGo8", "inUni", "go8"), noms = c("loc", "state"),
			 bounds = rbind(c(28,0,Inf),c(29,0,Inf),c(30,0,Inf),c(31,0,Inf)))
a <- MI$imputations
for ( i in 1:5){
	x <- paste0("pv",i,".+")
	a[[i]]$ach <- principal(a[[i]][,grep(x, names(a[[i]]))], 1)$scores
}
a <- imputationList(a)

dclust <- svrepdesign(ids = ~1, weights = ~w_fstuwt, repweights = brr[,-1], 
					  type = "Fay", data = a, rho = 0.5)
dclust <- subset(dclust, state != 7, all=TRUE)

save(dclust, file = "~/Dropbox/Projects_Research/distancePaper/complexData.RData")

