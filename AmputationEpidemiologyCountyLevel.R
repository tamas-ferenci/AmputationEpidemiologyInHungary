library( data.table )

RawData <- readRDS( "../RawData/e_x2018_10_16b_proc.dat" )
RawData <- RawData[ PROCDATE >= as.Date( "2004-01-01" ) ]
RawData$YEAR <- lubridate::year( RawData$PROCDATE )
RawData$AGE <- cut( RawData$AGE, c( seq( 0, 85, 5 ), Inf ), right = FALSE, labels = seq( 0, 85, 5 ) )
RawData$AGE <- as.numeric( levels( RawData$AGE ) )[ RawData$AGE ]
RawData$HOMENUTS3 <- ifelse( RawData$HOMENUTS3=="főváros", "Budapest", paste0( RawData$HOMENUTS3, " megye" ) )
RawData$HOMELAU1 <- ifelse( substring( RawData$HOMELAU1, 1, 8 )=="Budapest",
                            paste0( substring( RawData$HOMELAU1, 1, 16 ), "ület" ), paste0( RawData$HOMELAU1, " járás" ) )
RawData$DONENUTS3 <- ifelse( RawData$DONENUTS3=="főváros", "Budapest", paste0( RawData$DONENUTS3, " megye" ) )
RawData$DONELAU1 <- ifelse( substring( RawData$DONELAU1, 1, 8 )=="Budapest",
                            paste0( substring( RawData$DONELAU1, 1, 16 ), "ület" ), paste0( RawData$DONELAU1, " járás" ) )
RawData[ , Primer := sapply( PROCDATE, function(x)
{ !any( PROCDATE[ TYPE%in%c( "VASCINTER", "VASCOPER", "VASCEMBOL" ) ] < x & x <= PROCDATE[ TYPE%in%c( "VASCINTER", "VASCOPER",
                                                                                                      "VASCEMBOL" ) ]+365 ) } ),
.( TAJ ) ]
temp <- RawData[ TYPE%in%c( "CRURALAMP", "FEMORAMP" ) ]
temp$TYPE <- "MAJORAMP"
RawData <- rbind( RawData, temp )
temp <- RawData[ TYPE%in%c( "VASCINTER", "VASCOPER" ) ]
temp$TYPE <- "REVASC"
RawData <- rbind( RawData, temp )
RawData$NUTS3 <- ifelse( RawData$TYPE%in%c( "MINORAMP", "CRURALAMP", "FEMORAMP", "MAJORAMP" ), RawData$HOMENUTS3,
                         RawData$DONENUTS3 )
RawData$LAU1 <- RawData$HOMELAU1
RawData <- merge( RawData, data.table( TYPE = c( "STROKE", "PTCA", "AMIPTCA", "VASCOPER", "AMI", "FEMORAMP", "CRURALAMP",
                                                 "CABG", "MINORAMP", "CARINTER", "CAROPER", "VASCINTER", "VASCEMBOL", "AMICABG",
                                                 "AMIPTCACABG", "PROSTANOID", "MAJORAMP", "REVASC" ),
                                       TYPEname = c( "Stroke", "PCI", "AMI PCI-vel", "Vaszkuláris operáció", "AMI",
                                                     "Femorális amputáció", "Cruralis amputáció", "CABG", "Minor amputáció",
                                                     "Carotis intervenció", "Carotis operáció", "Vaszkuláris intervenció",
                                                     "Vaszkuláris embolizáció", "AMI CABG-dzsel", "AMI PCI-vel és CABG-dzsel",
                                                     "Prostanoid kezelés", "Major amputáció", "Revaszkularizáció" ) ),
                  by = "TYPE" )
saveRDS( RawData, "RawData.dat" )

###

RawData <- readRDS( "RawData.dat" )
LAU1NUTS3NUTS2NUTS1 <- fread( "../../Terkepek/LAU1NUTS3NUTS2NUTS1.csv" )
PopPyramid <- rbind( KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2004:2016, AgeGroup = "FiveYear",
                                                                  GeographicArea = "Total", Type = "MidYear" ),
                     KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2017, AgeGroup = "FiveYear",
                                                                  GeographicArea = "Total", Type = "Jan1" ),
                     KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2004:2016, AgeGroup = "FiveYear",
                                                                  GeographicArea = "NUTS3", Type = "MidYear" ),
                     KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2017, AgeGroup = "FiveYear",
                                                                  GeographicArea = "NUTS3", Type = "Jan1" ),
                     KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2015:2016, AgeGroup = "FiveYear",
                                                                  GeographicArea = "LAU1", Type = "MidYear" ),
                     KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2017, AgeGroup = "FiveYear",
                                                                  GeographicArea = "LAU1", Type = "Jan1" ) )
PopPyramid$SEX <- ifelse( PopPyramid$SEX=="Male", 1, 2 )
PopPyramid$GEO[ PopPyramid$GEO=="Nyíregyházai járás" ] <- "Nyíregyházi járás"
StdPops <- readRDS( "StdPops.dat" )
StdPops$Sex <- ifelse( StdPops$Sex=="Férfi", 1, 2 )
names( StdPops )[ c( 1, 5 ) ] <- c( "AGE", "SEX" )
resNational <- cbind( SCALE = "National", as.data.table( xtabs( ~ YEAR + SEX + AGE + TYPEname, data = RawData ) ),
                      GEO = "Total" )
resNUTS3 <- cbind( SCALE = "NUTS3", as.data.table( xtabs( ~ YEAR + SEX + AGE + TYPEname + NUTS3, data = RawData ) ) )
names( resNUTS3 )[ 6 ] <- "GEO"
resLAU1 <- cbind( SCALE = "LAU1", as.data.table( xtabs( ~ YEAR + SEX + AGE + TYPEname + LAU1, data = RawData,
                                                        subset = YEAR>=2015 ) ) )
names( resLAU1 )[ 6 ] <- "GEO"
res <- rbind( resNational, resNUTS3, resLAU1 )
res$YEAR <- as.numeric( res$YEAR )
res$AGE <- as.numeric( res$AGE )
res$SEX <- as.numeric( res$SEX )
res <- merge( res, PopPyramid )
res <- merge( res, StdPops, by = c( "AGE", "SEX" ) )
res <- transform( res, Inc = N/POPULATION*1e5 )
res <- res[ order( res$TYPEname, res$YEAR, res$SEX, res$AGE ), ]
res$SEX <- ifelse( res$SEX==1, "Férfi", "Nő" )
res$GEO[ res$GEO == "Total" ] <- "Országos"
res$GEO <- relevel( as.factor( res$GEO ), ref = "Országos" )
res$GEOSEX <- interaction( res$GEO, res$SEX, sep = " - " )

RawData$Event <- !is.na( RawData$DEATHDATE )
RawData$CensorDate <- dplyr::if_else( RawData$Event, RawData$DEATHDATE, as.Date( "2018-01-31" ) )
RawData$Time <- ifelse( RawData$Event, as.numeric( difftime( RawData$DEATHDATE, RawData$PROCDATE, unit = "days" ) ),
                        as.numeric( difftime( as.Date( "2018-01-31" ), RawData$PROCDATE, unit = "days" ) ) )

RawDataStd <- res[ , as.list( epitools::ageadjust.direct( N, POPULATION, stdpop = StdESP2013 ) ), .( TYPEname, YEAR, GEO ) ]
RawDataStd$lci[ is.nan( RawDataStd$lci ) ] <- 0
RawDataStd[ , 4:7 ] <- RawDataStd[ , 4:7 ]*1e5
RawDataStd$GEO <- relevel( as.factor( RawDataStd$GEO ), ref = "Országos" )
RawDataStdLAU1 <- res[ SCALE=="LAU1" ][ , .( N = sum( N ), POPULATION = sum( POPULATION ), StdESP2013 = sum( StdESP2013 ) ),
                                        .( AGE, SEX, LAU1 = GEO, TYPEname ) ][ , as.list(
                                          epitools::ageadjust.direct( N, POPULATION, stdpop = StdESP2013 ) ),
                                          .( TYPEname, LAU1 ) ]
RawDataStdLAU1$lci[ is.nan( RawDataStdLAU1$lci ) ] <- 0
RawDataStdLAU1[ , 3:6 ] <- RawDataStdLAU1[ , 3:6 ]*1e5
RawDataStdLAU1 <- merge( RawDataStdLAU1, LAU1NUTS3NUTS2NUTS1[ , c( "LAU1", "NUTS3" ) ] )
write.csv2( RawDataStd, "Incidenciak.csv" )
write.csv2( RawDataStdLAU1, "IncidenciakLAU1.csv" )
write.csv2( data.frame( xtabs( ~ TYPEname + DONENUTS3 + HOMENUTS3, data = RawData ) ), "KuldesiAdatok.csv" )
PopPyramid$SEX <- ifelse( PopPyramid$SEX==1, "Férfi", "Nő" )
TerkepLAU1 <- rgdal::readOGR( "../../Terkepek/OSM_kozighatarok", "admin7", encoding = "UTF8",
                              use_iconv = TRUE, stringsAsFactors = FALSE )
TerkepLAU1 <- TerkepLAU1[ !duplicated( TerkepLAU1$NAME ), ]
TerkepLAU1$NAME[ TerkepLAU1$NAME=="Szentlőrinci Járás" ] <- "Szentlőrinci járás"
TerkepBP <- rgdal::readOGR( "../../Terkepek/OSM_kozighatarok", "admin9", encoding = "UTF8",
                            use_iconv = TRUE, stringsAsFactors = FALSE )
TerkepBP$NAME <- paste0( "Budapest ", sprintf( "%02d", as.numeric( as.roman(
  substring( TerkepBP$NAME, 1, as.numeric( gregexpr( ".", TerkepBP$NAME, fixed = TRUE ) )-1 ) ) ) ), ". kerület" )
TerkepLAU1 <- Reduce( rbind, list( TerkepLAU1, TerkepBP ) )
TerkepNUTS3 <- rgdal::readOGR( "../../Terkepek/OSM_kozighatarok", "admin6", encoding = "UTF8",
                               use_iconv = TRUE, stringsAsFactors = FALSE )

RawDataCruralFemoral <- rbind(
  RawData[ , c( as.list( with( binom.test( sum( TYPE=="CRURALAMP" ), sum( TYPE%in%c( "CRURALAMP", "FEMORAMP" ) ) ),
                               c( est = unname( estimate ), ci = conf.int ) ) ), SCALE = "Országos", GEO = "Országos" ),
           .( YEAR ) ],
  RawData[ , c( as.list( with( binom.test( sum( TYPE=="CRURALAMP" ), sum( TYPE%in%c( "CRURALAMP", "FEMORAMP" ) ) ),
                               c( est = unname( estimate ), ci = conf.int ) ) ), SCALE = "NUTS3" ), .( YEAR, GEO = NUTS3 ) ],
  RawData[ YEAR>=2015, c( YEAR = 0, as.list( with( binom.test( sum( TYPE=="CRURALAMP" ),
                                                               sum( TYPE%in%c( "CRURALAMP", "FEMORAMP" ) ) ),
                                                   c( est = unname( estimate ), ci = conf.int ) ) ), SCALE = "LAU1" ),
           .( GEO = LAU1 ) ][ , c( 2, 1, 3:6 )]
)[ order( YEAR ) ]
RawDataCruralFemoral$est <- RawDataCruralFemoral$est*100
RawDataCruralFemoral$ci1 <- RawDataCruralFemoral$ci1*100
RawDataCruralFemoral$ci2 <- RawDataCruralFemoral$ci2*100
RawDataCruralFemoral <- merge( RawDataCruralFemoral, LAU1NUTS3NUTS2NUTS1[ , .( GEO = LAU1, NUTS3 ) ], all.x = TRUE )

RawDataPrimer <- rbind(
  RawData[ TYPE=="MAJORAMP", c( as.list( with( binom.test( sum( Primer ), .N ), c( est = unname( estimate ), ci = conf.int ) ) ),
                                SCALE = "Országos", GEO = "Országos" ), .( YEAR ) ],
  RawData[ TYPE=="MAJORAMP", c( as.list( with( binom.test( sum( Primer ), .N ), c( est = unname( estimate ), ci = conf.int ) ) ),
                                SCALE = "NUTS3" ), .( YEAR, GEO = NUTS3 ) ],
  RawData[ TYPE=="MAJORAMP"&YEAR>=2015, c( YEAR = 0, as.list( with( binom.test( sum( Primer ), .N ), c( est = unname( estimate ),
                                                                                                        ci = conf.int ) ) ),
                                           SCALE = "LAU1" ), .( GEO = LAU1 ) ][ , c( 2, 1, 3:6 ) ]
)[ order( YEAR ) ]
RawDataPrimer$est <- RawDataPrimer$est*100
RawDataPrimer$ci1 <- RawDataPrimer$ci1*100
RawDataPrimer$ci2 <- RawDataPrimer$ci2*100
RawDataPrimer <- merge( RawDataPrimer, LAU1NUTS3NUTS2NUTS1[ , .( GEO = LAU1, NUTS3 ) ], all.x = TRUE )

RawDataBetegKuldFogad <- data.table( xtabs( ~ YEAR + TYPEname + HOMENUTS3 + DONENUTS3, data = RawData ) )
RawDataBetegKuldFogad$YEAR <- as.numeric( RawDataBetegKuldFogad$YEAR )

RawDataSurv <- rbind(
  RawData[ , with( summary( survival::survfit( survival::Surv( Time, Event ) ~ 1 ), times = 30 ),
                   list( GEO = "Országos", surv = surv, lower = lower, upper = upper ) ), .( YEAR, TYPEname ) ][ order(YEAR) ],
  RawData[ , with( summary( survival::survfit( survival::Surv( Time, Event ) ~ 1 ), times = 30 ),
                   list( surv = if(exists("surv")) surv else NA_real_, lower = if(exists("lower")) lower else NA_real_,
                         upper = if(exists("upper")) upper else NA_real_ ) ), .( YEAR, TYPEname, GEO = NUTS3 ) ][ order(YEAR) ]
)

saveRDS( PopPyramid, "PopPyramidReport.dat" )
saveRDS( RawDataStd, "RawDataStdReport.dat" )
saveRDS( RawDataCruralFemoral, "RawDataCruralFemoral.dat" )
saveRDS( RawDataPrimer, "RawDataPrimer.dat" )
saveRDS( RawDataBetegKuldFogad, "RawDataBetegKuldFogad.dat" )
saveRDS( RawDataSurv, "RawDataSurv.dat" )
saveRDS( merge( res[ , c( "AGE", "SEX", "YEAR", "GEO", "TYPEname", "N", "POPULATION", "Inc", "GEOSEX" ) ],
                LAU1NUTS3NUTS2NUTS1[ , .( GEO = LAU1, NUTS3 ) ], all.x = TRUE ), "RawDataRes.dat" )
saveRDS( TerkepLAU1, "TerkepLAU1.dat" )
saveRDS( TerkepNUTS3, "TerkepNUTS3.dat" )

###

for( county in unique( RawData$HOMENUTS3 ) ) {
  rmarkdown::render( "MegyeiHelyzetkep_1.Rmd", "pdf_document",
                     paste0( "MegyeiHelyzetkep_", gsub( "-", "", gsub( " ", "", iconv( county, to = "ASCII//TRANSLIT" ) ) ),
                             ".pdf" ), params = list( county = county ) )
}

###

types <- list( MAJORAMP = "MAJORAMP", total = c("MAJORAMP","VASCINTER","VASCOPER"), erbeav = c("VASCINTER","VASCOPER") )

prev <- do.call( rbind, lapply( c( "Total", unique( RawData$HOMENUTS3 ) ), function( x ) {
  do.call( rbind, lapply( 1:3, function( y ) {
    res <- rprev::prevalence(
      index='2017-12-31', 
      num_years_to_estimate=c(3, 5, 10, 20), 
      data=data.frame(RawData[TYPE%in%types[[y]]&Time>0&if( x=="Total" ) TRUE else HOMENUTS3==x,
                              .( time = Time, status = ifelse(Event,1,0), age = AGE,
                                 sex = as.factor( ifelse(SEX==1,"M","F") ), entrydate = PROCDATE,
                                 eventdate = CensorDate )]), 
      inc_formula = entrydate ~ sex, surv_formula = survival::Surv(time, status) ~ age + sex,  dist = "weibull",
      population_size = sum( PopPyramid[GEO==x]$POPULATION )/length(unique(PopPyramid[GEO==x]$YEAR)),
      death_column = 'eventdate')$estimates
    res <- data.frame( type = names(types[y]), county = x, do.call( rbind, lapply( res, data.frame ) ) )
    res$year <- row.names( res )
    res
  } ) )
} ) )

write.csv2( prev, "Prevalenciak.csv", row.names = FALSE, fileEncoding = "UTF-16LE" )