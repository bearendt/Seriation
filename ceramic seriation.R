# set the working directory
setwd ("P:/Reports/Joiner Shop/R Code")

# Ceramic Seriation Code.R
#First run is exterior quads and second run is interior quads only

# Establish a DBI connection to DAACS PostgreSQL database and submit SQL queries
# Last update: BA  5/24/2018


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

#Get New Quad Data
CeramicExterior <- dbGetQuery(DRCcon,'
                      SELECT
                         "public"."tblCeramic"."Quantity",
                          "public"."tblCeramicWare"."Ware",
                          "public"."tblCeramicWare"."BeginDate",
                          "public"."tblCeramicWare"."EndDate",
                          "public"."tblContextFeatureType"."FeatureType",
                          "public"."tblCeramicGenre"."CeramicGenre",
                          "public"."tblContext"."ContextID",
                          "public"."tblContext"."ProjectID",
                          "public"."tblContext"."Context",
                          "public"."tblContext"."DAACSStratigraphicGroup",
                          "public"."tblContext"."FeatureNumber"
                          FROM
                          "public"."tblContext"
                          INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                          INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                          INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                          INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                          LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID"  
                          LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
                          LEFT JOIN "public"."tblContextFeature" ON "public"."tblContext"."ProjectID" = "public"."tblContextFeature"."ProjectID" AND "public"."tblContext"."FeatureNumber" = "public"."tblContextFeature"."FeatureNumber"                         
                          
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
or "public"."tblContext"."ContextID" LIKE \'109-1835%\'
or "public"."tblContext"."ContextID" LIKE \'109-1836%\'
or "public"."tblContext"."ContextID" LIKE \'109-1837%\'
or "public"."tblContext"."ContextID" LIKE \'109-117%\'
or "public"."tblContext"."ContextID" LIKE \'109-109%\'
or "public"."tblContext"."ContextID" LIKE \'109-105%\'
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
                       )
                       ')



#Summarize data based on manutech
require(plyr)
summary2 <- ddply(CeramicExterior, .(Ware), summarise, Count=sum(Quantity))
summary2

#Fill blanks with NA 
#CeramicExterior$FeatureNumber <- sapply(CeramicExterior$FeatureNumber, function(f){is.na(f)<-which(f == '');f}) 

#Fill blanks with NA 
#CeramicExterior$DAACSStratigraphicGroup <- sapply(CeramicExterior$DAACSStratigraphicGroup, 
#                             function(f){is.na(f)<-which(f == '');f}) 



#Get rid of Unid and Indet nail man tech and SGs associated with interior
#NailData2 <- subset(NailData2, !NailData2$Context %in% '2409UNPROV')
CeramicExterior1 <- subset(CeramicExterior, !CeramicExterior$DAACSStratigraphicGroup %in% NA)
CeramicExterior2 <- subset(CeramicExterior1, !CeramicExterior1$DAACSStratigraphicGroup %in% '')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$DAACSStratigraphicGroup %in% 'SG14')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$DAACSStratigraphicGroup %in% 'SG19')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$DAACSStratigraphicGroup %in% 'SG20')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$Ware %in% 'Refined Earthenware, unidentifiable')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$Ware %in% 'Coarse Earthenware, unidentifiable')
# lets get a data frame with contexts as rows and type as cols, with the entries as counts

CeramicByUnit <- ddply(CeramicExterior2, .(DAACSStratigraphicGroup, Ware), summarise, Count=sum(Quantity))


# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros

require(reshape2)
CeramicByUnitTrans <- acast(CeramicByUnit, DAACSStratigraphicGroup ~ Ware, value.var='Count', fill=0 )



# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
CeramicsByUnitTotals <- rowSums(CeramicByUnitTrans[,2:ncol(CeramicByUnitTrans)])

# and how about a histogram?
hist(CeramicByUnitTrans,  
     col='light blue',
     xlab ='Count',
     ylab='Number of Contexts',
     main = 'Ware Asssemblage Size',
     cex.axis=1.5,
     cex.lab=2)


#write.csv(NailsByUnit, file='Nails.csv')


CeramicProp <- CeramicByUnitTrans/ rowSums(CeramicByUnitTrans) 


library(plotrix)
# seriation plot of the CA dimension 1 order
battleship.plot(CeramicProp,
                mar=c(1,5,5,1), cex=0.7,
                main = 'Exterior',
                xlab='Ware',
                ylab= 'Context',
                col='grey')

#####################
#Ceramic seriation for interior quads

#Get New inteiror Quad Data
CeramicInterior <- dbGetQuery(DRCcon,'
                              SELECT
                              "public"."tblCeramic"."Quantity",
                              "public"."tblCeramicWare"."Ware",
                              "public"."tblCeramicWare"."BeginDate",
                              "public"."tblCeramicWare"."EndDate",
                              "public"."tblContextFeatureType"."FeatureType",
                              "public"."tblCeramicGenre"."CeramicGenre",
                              "public"."tblContext"."ContextID",
                              "public"."tblContext"."ProjectID",
                              "public"."tblContext"."Context",
                              "public"."tblContext"."DAACSStratigraphicGroup",
                              "public"."tblContext"."FeatureNumber"
                              FROM
                              "public"."tblContext"
                              INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                              INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                              INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                              INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                              LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID"  
                              LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
                              LEFT JOIN "public"."tblContextFeature" ON "public"."tblContext"."ProjectID" = "public"."tblContextFeature"."ProjectID" AND "public"."tblContext"."FeatureNumber" = "public"."tblContextFeature"."FeatureNumber"                         
                              
                              WHERE 
                       
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
                            
                              ')
#Summarize data based on manutech
require(plyr)
summary2 <- ddply(CeramicInterior, .(Ware), summarise, Count=sum(Quantity))
summary2

#Fill blanks with NA 
#CeramicInterior$FeatureNumber <- sapply(CeramicInterior$FeatureNumber, function(f){is.na(f)<-which(f == '');f}) 

#Fill blanks with NA 
#CeramicInterior$DAACSStratigraphicGroup <- sapply(CeramicInterior$DAACSStratigraphicGroup, 
#                             function(f){is.na(f)<-which(f == '');f}) 



#Get rid of Unid and Indet nail man tech and SGs associated with interior
#NailData2 <- subset(NailData2, !NailData2$Context %in% '2409UNPROV')
CeramicInterior1 <- subset(CeramicInterior, !CeramicInterior$Ware %in% 'Refined Earthenware, unidentifiable')
CeramicInterior1 <- subset(CeramicInterior1, !CeramicInterior1$DAACSStratigraphicGroup %in% NA)
CeramicInterior2 <- subset(CeramicInterior1, !CeramicInterior1$DAACSStratigraphicGroup %in% '')
CeramicInterior2 <- subset(CeramicInterior2, !CeramicInterior2$DAACSStratigraphicGroup %in% 'SG16')
#CeramicInterior2 <- subset(CeramicInterior2, !CeramicInterior2$DAACSStratigraphicGroup %in% 'SG11')
CeramicInterior2 <- subset(CeramicInterior2, !CeramicInterior2$DAACSStratigraphicGroup %in% 'SG17')
#CeramicInterior2 <- subset(CeramicInterior2, !CeramicInterior2$Context %in% '2411N')
CeramicInterior2 <- subset(CeramicInterior2, !CeramicInterior2$Ware %in% 'Porcelain, unidentifiable')

# lets get a data frame with contexts as rows and type as cols, with the entries as counts

CeramicByUnit <- ddply(CeramicInterior2, .(DAACSStratigraphicGroup, Ware), summarise, Count=sum(Quantity))


# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros

require(reshape2)
CeramicByUnitTrans <- acast(CeramicByUnit, DAACSStratigraphicGroup ~ Ware, value.var='Count', fill=0 )



# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
CeramicsByUnitTotals <- rowSums(CeramicByUnitTrans[,2:ncol(CeramicByUnitTrans)])

# and how about a histogram?
hist(CeramicByUnitTrans,  
     col='light blue',
     xlab ='Count',
     ylab='Number of Contexts',
     main = 'Ware Asssemblage Size',
     cex.axis=1.5,
     cex.lab=2)


#write.csv(NailsByUnit, file='Nails.csv')


CeramicProp <- CeramicByUnitTrans/ rowSums(CeramicByUnitTrans) 


library(plotrix)
# seriation plot of the CA dimension 1 order
battleship.plot(CeramicProp,
                mar=c(1,5,5,1), cex=0.7,
                main = 'Interior',
                xlab='Ware',
                ylab= 'Context',
                col='grey')

#### Haha quads

#Get New Quad Data
CeramicExterior <- dbGetQuery(DRCcon,'
                              SELECT
                              "public"."tblCeramic"."Quantity",
                              "public"."tblCeramicWare"."Ware",
                              "public"."tblCeramicWare"."BeginDate",
                              "public"."tblCeramicWare"."EndDate",
                              "public"."tblContextFeatureType"."FeatureType",
                              "public"."tblCeramicGenre"."CeramicGenre",
                              "public"."tblContext"."ContextID",
                              "public"."tblContext"."ProjectID",
                              "public"."tblContext"."Context",
                              "public"."tblContext"."DAACSStratigraphicGroup",
                              "public"."tblContext"."FeatureNumber"
                              FROM
                              "public"."tblContext"
                              INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                              INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                              INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                              INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                              LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID"  
                              LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
                              LEFT JOIN "public"."tblContextFeature" ON "public"."tblContext"."ProjectID" = "public"."tblContextFeature"."ProjectID" AND "public"."tblContext"."FeatureNumber" = "public"."tblContextFeature"."FeatureNumber"                         
                              
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
                              )
                              ')



#Summarize data based on manutech
require(plyr)
summary2 <- ddply(CeramicExterior, .(Ware), summarise, Count=sum(Quantity))
summary2

#Fill blanks with NA 
#CeramicExterior$FeatureNumber <- sapply(CeramicExterior$FeatureNumber, function(f){is.na(f)<-which(f == '');f}) 

#Fill blanks with NA 
#CeramicExterior$DAACSStratigraphicGroup <- sapply(CeramicExterior$DAACSStratigraphicGroup, 
#                             function(f){is.na(f)<-which(f == '');f}) 



#Get rid of Unid and Indet nail man tech and SGs associated with interior
#NailData2 <- subset(NailData2, !NailData2$Context %in% '2409UNPROV')
CeramicExterior1 <- subset(CeramicExterior, !CeramicExterior$DAACSStratigraphicGroup %in% NA)
CeramicExterior2 <- subset(CeramicExterior1, !CeramicExterior1$DAACSStratigraphicGroup %in% '')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$DAACSStratigraphicGroup %in% 'SG14')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$DAACSStratigraphicGroup %in% 'SG19')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$DAACSStratigraphicGroup %in% 'SG20')
CeramicExterior2 <- subset(CeramicExterior2, !CeramicExterior2$Ware %in% 'Refined Earthenware, unidentifiable')

# lets get a data frame with contexts as rows and type as cols, with the entries as counts

CeramicByUnit <- ddply(CeramicExterior2, .(DAACSStratigraphicGroup, Ware), summarise, Count=sum(Quantity))


# now we transpose the data so that we end up with a context (rows) x type 
# (cols) data matrix; unit ~ ware formula syntax, left side = row, right side = column, to fill in
# body of table with the counts, fill rest with zeros

require(reshape2)
CeramicByUnitTrans <- acast(CeramicByUnit, DAACSStratigraphicGroup ~ Ware, value.var='Count', fill=0 )



# lets compute the totals for each context i.e. row
# Note the use of column numbers as index values to get the type counts, which are
# assumed to start iin col 2.
CeramicsByUnitTotals <- rowSums(CeramicByUnitTrans[,2:ncol(CeramicByUnitTrans)])

# and how about a histogram?
hist(CeramicByUnitTrans,  
     col='light blue',
     xlab ='Count',
     ylab='Number of Contexts',
     main = 'Ware Asssemblage Size',
     cex.axis=1.5,
     cex.lab=2)


#write.csv(NailsByUnit, file='Nails.csv')


CeramicProp <- CeramicByUnitTrans/ rowSums(CeramicByUnitTrans) 


library(plotrix)
# seriation plot of the CA dimension 1 order
battleship.plot(CeramicProp,
                mar=c(1,5,5,1), cex=0.7,
                main = '',
                xlab='Ware',
                ylab= 'Context',
                col='grey')
