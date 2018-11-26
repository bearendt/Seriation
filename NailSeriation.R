# set the working directory
setwd ("P:/Reports/Joiner Shop/R Code")

# Nail Code.R

# Establish a DBI connection to DAACS PostgreSQL database and submnit SQL queries
# The first half pulls exterior quads and the second half are the interior quads
# Last update: BA  11/25/2015


#load the library
library(DBI)
require(RPostgreSQL)
library(RODBC)

# tell DBI which driver to use
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon <- dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
                  dbname ='daacs-production',
                  user ='drcquery', password='!queryacct!')

#Get New Exterior Quad Data
NailData <- dbGetQuery(DRCcon,'
                             SELECT
                             "public"."tblGenArtifact"."Quantity",
                             "public"."tblGenArtifactForm"."GenArtifactForm",
                             "public"."tblGenArtifactCompleteness"."GenArtifactCompleteness",
                             "public"."tblGenArtifactManuTech"."GenArtifactManuTech",
                             "public"."tblContextFeatureType"."FeatureType",
                             "public"."tblContext"."ContextID",
                             "public"."tblContext"."ProjectID",
                             "public"."tblContext"."Context",
                             "public"."tblContext"."DAACSStratigraphicGroup",
                             "public"."tblContext"."FeatureNumber"
                             FROM
                             "public"."tblContext"
                             INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                             INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                             INNER JOIN "public"."tblGenArtifact" ON "public"."tblGenArtifact"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                             INNER JOIN "public"."tblGenArtifactForm" ON "public"."tblGenArtifact"."GenArtifactFormID" = "public"."tblGenArtifactForm"."GenArtifactFormID"
                             INNER JOIN "public"."tblGenArtifactCompleteness" ON "public"."tblGenArtifact"."GenArtifactCompletenessID" = "public"."tblGenArtifactCompleteness"."GenArtifactCompletenessID"
                             INNER JOIN "public"."tblGenArtifactMaterial" ON "public"."tblGenArtifactMaterial"."GenerateContextArtifactID" = "public"."tblGenArtifact"."GenerateContextArtifactID"
                             INNER JOIN "public"."tblGenArtifactManuTech" ON "public"."tblGenArtifactMaterial"."GenArtifactManuTechID" = "public"."tblGenArtifactManuTech"."GenArtifactManuTechID"
                             LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID" 
                             
                             WHERE (
                                
                             ( "public"."tblContext"."ContextID" LIKE \'109-2407%\' 
                              or "public"."tblContext"."ContextID" LIKE \'109-2404%\'  
                              or "public"."tblContext"."ContextID" LIKE \'109-2405%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2413%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2422%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2328%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2327%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2402%\'
                              
                              or "public"."tblContext"."ContextID" LIKE \'109-2401%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2325%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2326%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-2415%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-1837%\'
                              or "public"."tblContext"."ContextID" LIKE \'109-1836%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-117%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-109%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-105%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-118%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-135%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-119%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-136%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-128%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-138%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-145%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-291%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-108%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-107%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-106%\'
                              ) 
                             AND
                             ("public"."tblGenArtifactForm"."GenArtifactForm" LIKE \'Nail%\')
                             AND
                             ("public"."tblGenArtifactManuTech"."GenArtifactManuTech" NOT LIKE \'Machine Made\')
                             )
                             ')



#_______________________________________Nail Seriation_____________________________________________________




#Summarize data based on manutech
require(plyr)
summary2 <- ddply(NailData, .(GenArtifactManuTech), summarise, Count=sum(Quantity))
summary2

#Fill blanks with NA 
#NailData$FeatureNumber <- sapply(NailData$FeatureNumber, function(f){is.na(f)<-which(f == '');f}) 

#Fill blanks with NA 
#NailData$DAACSStratigraphicGroup <- sapply(NailData$DAACSStratigraphicGroup, 
              #                             function(f){is.na(f)<-which(f == '');f}) 



#Get rid of Unid and Indet nail man tech and SGs associated with interior
#NailData2 <- subset(NailData2, !NailData2$Context %in% '2409UNPROV')
NailData2 <- subset(NailData, !NailData$GenArtifactManuTech %in% 'Indeterminate')
NailData2 <- subset(NailData2, !NailData2$GenArtifactManuTech %in% 'Unidentified')
NailData2 <- subset(NailData2, !NailData2$GenArtifactManuTech %in% NA)
NailData3 <- subset(NailData2, !NailData2$DAACSStratigraphicGroup %in% NA)
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% '')
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% 'SG14')
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% 'SG19')
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% 'SG20')

# lets get a data frame with contexts as rows and type as cols, with the entries as counts

NailsByUnit <- ddply(NailData3, .(DAACSStratigraphicGroup, GenArtifactManuTech), summarise, Count=sum(Quantity))


# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros

require(reshape2)
NailsByUnitTrans <- acast(NailsByUnit, DAACSStratigraphicGroup ~ GenArtifactManuTech, value.var='Count', fill=0 )



# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
NailsByUnitTotals <- rowSums(NailsByUnitTrans[,2:ncol(NailsByUnitTrans)])

# and how about a histogram?
hist(NailsByUnitTrans,  
     col='light blue',
     xlab ='Count',
     ylab='Number of Contexts',
     main = 'Nail Asssemblage Size',
     cex.axis=1.5,
     cex.lab=2)


#write.csv(NailsByUnit, file='Nails.csv')


#___Unsorted Seriation Plot___________

#do the CA on the same data
library(ca)
library(maptools)
nailCA <- ca(NailsByUnitTrans)
summary(nailCA)


#sort the assemblages  on their means

nailProps <- NailsByUnitTrans/ rowSums(NailsByUnitTrans) 

#use if you want to sort by Dim 1 scores. 
#SortedNailProps <- nailProps[order(nailCA$rowcoord[,1]),]

library(plotrix)
# seriation plot of the CA dimension 1 order
battleship.plot(nailProps,
                mar=c(1,5,5,1), cex=0.7,
                main = 'Exterior',
                xlab='Manufacutring Technique',
                ylab= 'Context',
                col='grey')

#################
# Interior quads
#Get New Quad Data
NailData <- dbGetQuery(DRCcon,'
                       SELECT
                       "public"."tblGenArtifact"."Quantity",
                       "public"."tblGenArtifactForm"."GenArtifactForm",
                       "public"."tblGenArtifactCompleteness"."GenArtifactCompleteness",
                       "public"."tblGenArtifactManuTech"."GenArtifactManuTech",
                       "public"."tblContextFeatureType"."FeatureType",
                       "public"."tblContext"."ContextID",
                       "public"."tblContext"."ProjectID",
                       "public"."tblContext"."Context",
                       "public"."tblContext"."DAACSStratigraphicGroup",
                       "public"."tblContext"."FeatureNumber"
                       FROM
                       "public"."tblContext"
                       INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                       INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                       INNER JOIN "public"."tblGenArtifact" ON "public"."tblGenArtifact"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                       INNER JOIN "public"."tblGenArtifactForm" ON "public"."tblGenArtifact"."GenArtifactFormID" = "public"."tblGenArtifactForm"."GenArtifactFormID"
                       INNER JOIN "public"."tblGenArtifactCompleteness" ON "public"."tblGenArtifact"."GenArtifactCompletenessID" = "public"."tblGenArtifactCompleteness"."GenArtifactCompletenessID"
                       INNER JOIN "public"."tblGenArtifactMaterial" ON "public"."tblGenArtifactMaterial"."GenerateContextArtifactID" = "public"."tblGenArtifact"."GenerateContextArtifactID"
                       INNER JOIN "public"."tblGenArtifactManuTech" ON "public"."tblGenArtifactMaterial"."GenArtifactManuTechID" = "public"."tblGenArtifactManuTech"."GenArtifactManuTechID"
                       LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID" 
                       
                       WHERE (
                       
                       ( "public"."tblContext"."ContextID" LIKE \'109-2399%\' 
                       or "public"."tblContext"."ContextID" LIKE \'109-2323%\'  
                       or "public"."tblContext"."ContextID" LIKE \'109-2324%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2412%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2410%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2406%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2411%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2409%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2408%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2403%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2401%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2325%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2326%\'
                       or "public"."tblContext"."ContextID" LIKE \'109-2415%\'

                       ) 
                       AND
                       ("public"."tblGenArtifactForm"."GenArtifactForm" LIKE \'Nail%\')
                       AND
                       ("public"."tblGenArtifactManuTech"."GenArtifactManuTech" NOT LIKE \'Machine Made\')
                       )
                       ')



#_______________________________________Nail Seriation_____________________________________________________




#Summarize data based on manutech
require(plyr)
summary2 <- ddply(NailData, .(GenArtifactManuTech), summarise, Count=sum(Quantity))
summary2

#Fill blanks with NA 
#NailData$FeatureNumber <- sapply(NailData$FeatureNumber, function(f){is.na(f)<-which(f == '');f}) 

#Fill blanks with NA 
#NailData$DAACSStratigraphicGroup <- sapply(NailData$DAACSStratigraphicGroup, 
#                             function(f){is.na(f)<-which(f == '');f}) 

NailData2<-NailData

#Get rid of Unid and Indet nail man tech
NailData2 <- subset(NailData2, !NailData2$Context %in% '2409UNPROV')
NailData2 <- subset(NailData2, !NailData2$GenArtifactManuTech %in% 'Indeterminate')
NailData2 <- subset(NailData2, !NailData2$GenArtifactManuTech %in% 'Unidentified')
NailData3 <- subset(NailData2, !NailData2$DAACSStratigraphicGroup %in% NA)
NailData3 <- subset(NailData3, !NailData2$DAACSStratigraphicGroup %in% '')
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% 'SG16')
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% 'SG11')
NailData3 <- subset(NailData3, !NailData3$DAACSStratigraphicGroup %in% 'SG17')

# lets get a data frame with contexts as rows and type as cols, with the entries as counts

NailsByUnit <- ddply(NailData3, .(DAACSStratigraphicGroup, GenArtifactManuTech), summarise, Count=sum(Quantity))


# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros

require(reshape2)
NailsByUnitTrans <- acast(NailsByUnit, DAACSStratigraphicGroup ~ GenArtifactManuTech, value.var='Count', fill=0 )



# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
NailsByUnitTotals <- rowSums(NailsByUnitTrans[,2:ncol(NailsByUnitTrans)])

# and how about a histogram?
hist(NailsByUnitTrans,  
     col='light blue',
     xlab ='Count',
     ylab='Number of Contexts',
     main = 'Nail Asssemblage Size',
     cex.axis=1.5,
     cex.lab=2)


#write.csv(NailsByUnit, file='Nails.csv')


#___Unsorted Seriation Plot___________

#do the CA on the same data
library(ca)
library(maptools)
nailCA <- ca(NailsByUnitTrans)
summary(nailCA)


#sort the assemblages  on their means

nailProps <- NailsByUnitTrans/ rowSums(NailsByUnitTrans) 

#use if you want to sort by Dim 1 scores. 
#SortedNailProps <- nailProps[order(nailCA$rowcoord[,1]),]

library(plotrix)
# seriation plot of the CA dimension 1 order
battleship.plot(nailProps,
                mar=c(1,5,5,1), cex=0.7,
                main = 'Interior',
                xlab='Manufacturing Technique',
                ylab= 'Context',
                col='grey')





