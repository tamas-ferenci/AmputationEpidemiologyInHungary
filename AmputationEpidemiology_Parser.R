library(data.table)

RawData <- fread("input.csv")

RawData <- RawData[!is.na(SEX)&!is.na(TAJ)&TYPE!="", ]
RawData <- RawData[!is.na(HOMECODE)&!is.na(DONECODE)]
for(i in grep("DATE", names(RawData))) RawData[[i]] <- as.Date(RawData[[i]], format = "%Y.%m.%d")
RawData$TYPE <- as.factor(RawData$TYPE)
setkey(RawData, TAJ, TYPE, PROCDATE, SERVICEENTRYDATE, SERVICEEXITDATE)

MergeSameICD <- function( SDin ) {
  SD <- copy( SDin )
  setkey( SD, SERVICEENTRYDATE, SERVICEEXITDATE )
  SD$Diff <- as.numeric( shift( SD$SERVICEENTRYDATE, type = "lead" )-SD$SERVICEEXITDATE )
  while( ( nrow( SD )>1 ) && ( min( SD[ , Diff ], na.rm = TRUE )<40 ) ) {
    m <- which.min( SD$Diff )
    sexitval <- SD$SERVICEEXITDATE[ m+1 ]
    SD <- SD[ -(m+1) ]
    SD$SERVICEEXITDATE[ m ] <- sexitval
    setkey( SD, SERVICEENTRYDATE, SERVICEEXITDATE )
    SD$Diff <- as.numeric( shift( SD$SERVICEENTRYDATE, type = "lead" )-SD$SERVICEEXITDATE )
  }
  SD$Diff <- NULL
  return( SD[] )
}

RawData <- rbind(RawData[TYPE!="AMI"], RawData[TYPE=="AMI"][ , {print(.GRP); MergeSameICD(.SD)}, .(TAJ)])

setkey(RawData, TAJ, TYPE, PROCDATE, SERVICEENTRYDATE, SERVICEEXITDATE)

RawData[ , c( "PTCAMINDATE", "CABGMINDATE" ) := lapply( c( "PTCA", "CABG" ), function( proc )
  apply( .SD, 1, function( i ) {
    if ( i[ "TYPE" ]!="AMI" )
      as.numeric( NA )
    else {
      dates <- .SD[ TYPE==proc&PROCDATE>=i[ "SERVICEENTRYDATE" ]&PROCDATE<=i[ "SERVICEEXITDATE" ],
                    PROCDATE ]
      if( length( dates )== 0 ) as.numeric( NA ) else as.numeric( min( dates ) )
    }
  } ) ), by = .( TAJ ) ][]
RawData$PTCAMINDATE <- as.Date( RawData$PTCAMINDATE, origin = "1970-01-01" )
RawData$CABGMINDATE <- as.Date( RawData$CABGMINDATE, origin = "1970-01-01" )
RawData[ TYPE=="AMI", PROCDATE:=ifelse( is.na( PTCAMINDATE )&!is.na( CABGMINDATE ), CABGMINDATE,
                                        ifelse( !is.na( PTCAMINDATE )&is.na( CABGMINDATE ),
                                                PTCAMINDATE,
                                                ifelse( !is.na( PTCAMINDATE )&!is.na( CABGMINDATE ),
                                                        pmin( PTCAMINDATE, CABGMINDATE ),
                                                        EXITDATE ) ) ) ]
RawData[ TYPE=="AMI", TYPE:=ifelse( is.na( PTCAMINDATE )&!is.na( CABGMINDATE ), "AMICABG",
                                    ifelse( !is.na( PTCAMINDATE )&is.na( CABGMINDATE ), "AMIPTCA",
                                            ifelse( !is.na( PTCAMINDATE )&!is.na( CABGMINDATE ),
                                                    "AMIPTCACABG", "AMI" ) ) ) ]
RawData[ , c( "PTCAMINDATE", "CABGMINDATE" ):=NULL ]
setkey(RawData, TAJ, TYPE, PROCDATE, SERVICEENTRYDATE, SERVICEEXITDATE)

IrszHnk <- as.data.table(readRDS("IrszHnk.rds"))
IrszHnk$JarasKod <- substring(IrszHnk$Járáskódja, 1, 3)
IrszHnk <- IrszHnk[IRSZ!=1121|JarasKod!="021"]

DistributeToLAU1 <- unique(IrszHnk, by = c("IRSZ", "JarasKod"))[
  ,.(JarasKod, Lakó.népesség/sum(Lakó.népesség)) , .(IRSZ)][,.(sum(V2)) ,.(IRSZ, JarasKod)]
DistributeToLAU1$JarasKod <- as.integer(DistributeToLAU1$JarasKod)

sapply(unique(DistributeToLAU1[V1<1]$IRSZ), function(x) length(unique(IrszHnk[IRSZ==x]$JarasKod)))
table(sapply(setdiff(unique(IrszHnk$IRSZ), unique(DistributeToLAU1[V1<1]$IRSZ)),
             function(x) length(unique(IrszHnk[IRSZ==x]$JarasKod))))

sum(unique(RawData$HOMECODE)%in%DistributeToLAU1[V1<1]$IRSZ)
sum(unique(RawData$DONECODE)%in%DistributeToLAU1[V1<1]$IRSZ)

sum((RawData$HOMECODE)%in%DistributeToLAU1[V1<1]$IRSZ)
sum((RawData$DONECODE)%in%DistributeToLAU1[V1<1]$IRSZ)

set.seed(1)
codelist <- merge(RawData, DistributeToLAU1[V1==1, .(HOMECODE = IRSZ, JarasKod)],
                  by = "HOMECODE", all.x = TRUE, sort = FALSE)$JarasKod
for(i in which(RawData$HOMECODE%in%DistributeToLAU1[V1<1]$IRSZ))
  codelist[i] <- sample(DistributeToLAU1[IRSZ==RawData$HOMECODE[i]]$JarasKod, 1,
                        prob = DistributeToLAU1[IRSZ==RawData$HOMECODE[i]]$V1)
RawData$HOMECODE <- codelist
codelist <- merge(RawData, DistributeToLAU1[V1==1, .(DONECODE = IRSZ, JarasKod)],
                  by = "DONECODE", all.x = TRUE, sort = FALSE)$JarasKod
for(i in which(RawData$DONECODE%in%DistributeToLAU1[V1<1]$IRSZ))
  codelist[i] <- sample(DistributeToLAU1[IRSZ==RawData$DONECODE[i]]$JarasKod, 1,
                        prob = DistributeToLAU1[IRSZ==RawData$DONECODE[i]]$V1)
RawData$DONECODE <- codelist

RawData <- RawData[!is.na(HOMECODE)&!is.na(DONECODE)]

LAU1NUTS3NUTS2NUTS1 <- fread("LAU1NUTS3NUTS2NUTS1.csv")

RawData <- merge(RawData, LAU1NUTS3NUTS2NUTS1[, .(HOMENUTS3 = NUTS3, HOMELAU1 = LAU1,
                                                  HOMECODE = LAU1CODE)], by = "HOMECODE")
RawData <- merge(RawData, LAU1NUTS3NUTS2NUTS1[, .(DONENUTS3 = NUTS3, DONELAU1 = LAU1,
                                                  DONECODE = LAU1CODE)], by = "DONECODE")

Elixhauser <- read.csv2("Elixhauser.csv")

RawData <- merge(RawData, readRDS("ElixProc.rds"), by = "TAJ")

RawData$DM <- pmin(RawData$DMCOMPL, RawData$DMNOCOMPL, na.rm = TRUE)
RawData$HTossz <- pmin(RawData$HTCOMPL, RawData$HT, na.rm = TRUE)

temp <- RawData[TYPE%in%c("VASCOPER", "VASCINTER", "VASCOPERINTER", "VASCEMBOL")]
temp$TYPE <- "VASCPROCWIDE"
RawData <- rbind(RawData, temp)

temp <- RawData[TYPE%in%c("CRURALAMP", "FEMORAMP")]
temp$TYPE <- "MAJORAMP"
RawData <- rbind(RawData, temp)

temp$PrimerAmpSeged <- sapply(1:nrow(temp), function(i)
  nrow(RawData[TAJ==temp$TAJ[i]][PROCDATE<temp$PROCDATE[i]&
                                   PROCDATE>=temp$PROCDATE[i]-365&TYPE=="VASCPROCWIDE"])==0)

temp <- temp[PrimerAmpSeged==TRUE]
temp$TYPE <- "PRIMARY1YRMAJORAMP"
temp$PrimerAmpSeged <- NULL
RawData <- rbind(RawData, temp)

temp <- RawData[TYPE%in%c("MINORAMP", "CRURALAMP", "FEMORAMP")]
temp$TYPE <- "AMP"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("VASCOPER", "VASCINTER", "VASCOPERINTER")]
temp$TYPE <- "VASCPROCNARROW"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("VASCOPER", "VASCEMBOL")]
temp$TYPE <- "VASCOPERWIDE"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("CAROPER", "CARINTER")]
temp$TYPE <- "CARREVASC"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("CAROPER", "CARINTER", "STROKE")]
temp$TYPE <- "CEREBROVASC"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("CABG", "PCI")]
temp$TYPE <- "CORONARYREVASC"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("CABG", "PCI", "AMI")]
temp$TYPE <- "CORONARY"
RawData <- rbind(RawData, temp)
temp <- RawData[TYPE%in%c("CAROPER", "CARINTER", "STROKE", "CABG", "PCI", "AMI")]
temp$TYPE <- "CEREBROVASCORCORONARY"
RawData <- rbind(RawData, temp)
temp <- unique(merge(RawData[TYPE=="VASCOPERWIDE"], RawData[TYPE=="VASCINTER"],
                     by = colnames(RawData)[colnames(RawData)!="TYPE"])[, -c("TYPE.x", "TYPE.y")])
temp$TYPE <- "HYBRID"
RawData <- rbind(RawData, temp)

cl <- parallel::makeCluster(parallel::detectCores()-1)
parallel::clusterExport(cl, "RawData")
parallel::clusterEvalQ(cl, library(data.table))

RawData1 <- as.data.frame(do.call(rbind, parallel::parLapply(cl, 1:nrow(RawData), function(i)
  table(RawData[TAJ==RawData$TAJ[i]][PROCDATE<RawData$PROCDATE[i]&PROCDATE>=RawData$PROCDATE[i]-365]$TYPE))))
colnames(RawData1) <- paste0("Prior1Yr_", colnames(RawData1))

RawData <- cbind(RawData, RawData1)

RawData1 <- as.data.frame(do.call(rbind,  parallel::parLapply(cl, 1:nrow(RawData), function(i)
  table(RawData[TAJ==RawData$TAJ[i]][PROCDATE<RawData$PROCDATE[i]]$TYPE))))
colnames(RawData1) <- paste0("PriorEver_", colnames(RawData1))

RawData <- cbind(RawData, RawData1)

parallel::stopCluster(cl)

saveRDS(RawData, "RawDataRegiUjElixPrior.rds")