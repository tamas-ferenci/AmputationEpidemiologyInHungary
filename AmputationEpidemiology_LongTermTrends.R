library( data.table )
library( mgcv )

RawData <- RawData[ PROCDATE >= as.Date( "2004-01-01" ) ]
RawData$YEAR <- as.numeric( format( RawData$PROCDATE, "%Y" ) )
RawData$AGE <- cut( RawData$AGE, c( seq( 0, 85, 5 ), Inf ), right = FALSE, labels = seq( 0, 85, 5 ) )
RawData$AGE <- as.numeric( levels( RawData$AGE ) )[ RawData$AGE ]
RawData$TYPE <- as.character( RawData$TYPE )

RawData[ , Primer := sapply( PROCDATE, function( x )
{ !any( PROCDATE[ TYPE%in%c( "VASCINTER", "VASCOPER", "VASCEMBOL" ) ] < x & x <= PROCDATE[ TYPE%in%c( "VASCINTER", "VASCOPER",
                                                                                                      "VASCEMBOL" ) ]+365 ) } ),
.( TAJ ) ]

temp <- RawData[ TYPE%in%c( "CRURALAMP", "FEMORAMP" ) ]
temp$TYPE <- "MAJORAMP"
RawData <- rbind( RawData, temp )
temp <- RawData[ TYPE%in%c( "VASCINTER", "VASCOPER" ) ]
temp$TYPE <- "REVASC"
RawData <- rbind( RawData, temp )

length( unique( RawData[ TYPE%in%c( "MAJORAMP", "MINORAMP", "REVASC" ) ]$TAJ ) )

sum( RawData$TYPE%in%c("MAJORAMP", "MINORAMP", "REVASC") )
sd( RawData[ TYPE%in%c("MAJORAMP", "MINORAMP", "REVASC") ]$AGE )
prop.table( table( RawData[ TYPE%in%c("MAJORAMP", "MINORAMP", "REVASC") ]$SEX ) )

RawData[ TYPE%in%c( "CRURALAMP", "FEMORAMP" ),
         .( TYPE = "MAJORAMP", NoAmp = .N, NoPat = length( unique( TAJ ) ), NoMale = sum( SEX==1 ), PropMale = mean( SEX==1 ),
            MeanAge = mean( AGE ), SDAge = sd( AGE ), NoCrural = sum( TYPE=="CRURALAMP" ),
            PropCrural = mean( TYPE=="CRURALAMP" ), NoPrimer = sum( Primer ), PropPrim = mean( Primer ) ) ]
RawData[ TYPE=="MINORAMP",
         .( TYPE = "MINORAMP", NoAmp = .N, NoPat = length( unique( TAJ ) ), NoMale = sum( SEX==1 ), PropMale = mean( SEX==1 ),
            MeanAge = mean( AGE ), SDAge = sd( AGE ) ) ]
RawData[ TYPE%in%c( "VASCINTER", "VASCOPER" ),
         .( TYPE = "REVASC", NoRevasc = .N, NoPat = length( unique( TAJ ) ), NoMale = sum( SEX==1 ), PropMale = mean( SEX==1 ),
            MeanAge = mean( AGE ), SDAge = sd( AGE ), NoInter = sum( TYPE=="VASCINTER" ), PropInter = mean( TYPE=="VASCINTER" ),
            NoOper = sum( TYPE=="VASCOPER" ), PropInter = mean( TYPE=="VASCOPER" ) ) ]

TimeStratified <- setkey( RawData, SEX, AGE, PROCDATE, TYPE )[
  CJ( unique( SEX ), seq( 0, 85, 5 ), seq( as.Date( "2004-01-01" ), as.Date( "2017-12-31" ), by = 1 ),
      unique( TYPE ) ), .N, by = .EACHI ]

PopPyramid <- data.table( rbind( KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2004:2016, AgeGroup = "FiveYear",
                                                                              GeographicArea = "Total", Type = "MidYear" ),
                                 KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2017, AgeGroup = "FiveYear",
                                                                              GeographicArea = "Total", Type = "Jan1" ),
                                 KSHStatinfoScraper::GetPopulationPyramidKSH( Years = 2018, AgeGroup = "FiveYear",
                                                                              GeographicArea = "Total", Type = "Jan1" ) ) )
PopPyramid$SEX <- ifelse( PopPyramid$SEX=="Male", 1, 2 )

lattice::xyplot( POPULATION/1e5 ~ AGE | factor( SEX, levels = 1:2, labels = c( "Males", "Females" ) ),
                 groups = YEAR, data = PopPyramid[ YEAR%in%c( 2004, 2010, 2017 ) ],
                 type = "l", auto.key = list( columns = 3, points = FALSE, lines = TRUE ),
                 xlab = "Age", ylab = "Population [100,000]" )

PopPyramidDaily <- PopPyramid[ , with( approx( as.Date( paste0( YEAR, "-01-01" ) ), POPULATION,
                                               seq( as.Date( "2004-01-01" ),
                                                    as.Date( "2017-12-31" ), by = 1 ) ),
                                       list( PROCDATE = x, POPULATION = y ) ), .( AGE, SEX ) ]
PopPyramidDaily <- PopPyramidDaily[ , .( AGE, SEX, PROCDATE, POPULATION = POPULATION/Hmisc::yearDays( PROCDATE ) ) ]

TimeStratified <- merge( TimeStratified, PopPyramidDaily, by = c( "AGE", "SEX", "PROCDATE" ) )
TimeStratified$PROCDATEnum <- as.numeric( TimeStratified$PROCDATE )
TimeStratified$SEX <- as.factor( TimeStratified$SEX )
TimeStratified$DOY <- ( as.numeric( format(TimeStratified$PROCDATE, "%j") )-1 )/( Hmisc::yearDays( TimeStratified$PROCDATE )-1 )
TimeStratified$DOW <- relevel( as.factor( weekdays( TimeStratified$PROCDATE ) ), ref = "hétfő" )

StdPops <- readRDS( "StdPops.dat" )
StdPops$Sex <- ifelse( StdPops$Sex=="Férfi", 1, 2 )
names( StdPops )[ c( 1, 5 ) ] <- c( "AGE", "SEX" )

res <- as.data.table( xtabs( ~ YEAR + SEX + AGE + TYPE, data = RawData ) )
res$YEAR <- as.numeric( res$YEAR )
res$AGE <- as.numeric( res$AGE )
res$SEX <- as.numeric( res$SEX )
res <- merge( res, PopPyramid, by = c( "YEAR", "SEX", "AGE" ) )
res <- merge( res, StdPops, by = c( "AGE", "SEX" ) )
res <- transform( res, Inc = N/POPULATION*1e5 )
res <- res[ order( res$TYPE, res$YEAR, res$SEX, res$AGE ), ]
res$SEX <- ifelse( res$SEX==1, "Férfi", "Nő" )

res[ TYPE%in%c( "FEMORAMP", "CRURALAMP" ), .( CrudeInc = sum( N )/sum( POPULATION )*1e5 ) , .( YEAR, TYPE ) ]
prop.table( xtabs( ~ Primer + YEAR, RawData[ TYPE=="MAJORAMP" ] ), 2 )[ 2, ]*100

RawDataStd <- res[ , as.list( epitools::ageadjust.direct( N, POPULATION, stdpop = StdESP2013 ) ), .( TYPE, YEAR, GEO ) ]
RawDataStd$lci[ is.nan( RawDataStd$lci ) ] <- 0
RawDataStd[ , 4:7 ] <- RawDataStd[ , 4:7 ]*1e5
RawDataStd

RawDataStdSEX <- res[ , as.list( epitools::ageadjust.direct( N, POPULATION, stdpop = StdESP2013 ) ), .( TYPE, YEAR, GEO, SEX ) ]
RawDataStdSEX$lci[ is.nan( RawDataStdSEX$lci ) ] <- 0
RawDataStdSEX[ , 5:8 ] <- RawDataStdSEX[ , 5:8 ]*1e5

merge( merge( res[ AGE>=45, .( CrudeInc45 = sum( N )/sum( POPULATION )*1e5 ) , .( YEAR, TYPE ) ],
              res[ AGE>=60, .( CrudeInc60 = sum( N )/sum( POPULATION )*1e5 ) , .( YEAR, TYPE ) ] ),
       res[ AGE>=65, .( CrudeInc65 = sum( N )/sum( POPULATION )*1e5 ) , .( YEAR, TYPE ) ] )
res[ , as.list( xtabs( N ~ cut( AGE, c( -Inf, 45, 60, 65, Inf ) ) ) ) , .( YEAR, TYPE ) ]
res[ , .( sum( N[ AGE<45 ] )/sum( N )*100, sum( N[ AGE<60 ] )/sum( N )*100, sum( N[ AGE<65 ] )/sum( N )*100 ), .( TYPE ) ]
RawData[TYPE=="MAJORAMP",.(mean(AGE<65)*100)]
RawData[TYPE%in%c("MAJORAMP","REVASC","MINORAMP"),.(mean(AGE<45)*100,mean(AGE<60)*100,mean(AGE<65)*100)]

temp <- RawData[ TYPE%in%c( "VASCINTER", "VASCOPER" ) ]
temp2 <- temp[ , format( as.Date( intersect( PROCDATE[ TYPE=="VASCINTER" ], PROCDATE[ TYPE=="VASCOPER" ] ),
                                  origin = "1970-01-01" ), "%Y" ), .( TAJ ) ]
table( temp2$V1 )
table( temp$YEAR )
merge( data.table( table( temp2$V1 ) ), data.table( table( temp$YEAR ) ), by = "V1" )[ , .( V1, N.x/(N.y-N.x)*100 ) ]

fits <- lapply( unique( TimeStratified$TYPE ),
                function( t )
                  gam( N ~ s( PROCDATEnum, k = 50 ) + s( AGE, by = SEX, k = 15 ) + SEX + s( DOY, bs = "cc", k = 50 ) + DOW,
                       offset = log( POPULATION ), data = TimeStratified[ TYPE==t ], family = nb( link = log ),
                       knots = list( DOY = c( -(1/365.25)/2, 1+(1/365.25)/2 ) ), control = list( nthreads = 4 ) ) )
names( fits ) <- unique( TimeStratified$TYPE )

labs <- c( MAJORAMP = "Major amputations", VASCOPER = "Open vascular surgery procedures",
           VASCINTER = "Endovascular procedures", MINORAMP = "Minor amputations",
           REVASC = "All revascularizations procedures" )

for( t in names( labs ) ) {
  p <- plot( fits[[ t ]], select = 0, scale = 0, trans = exp, unconditional = TRUE,
             n = nrow( TimeStratified[ TYPE==t&AGE==0&SEX==1 ] ) )
  ran <- extendrange( c( exp( p[[ 1 ]]$fit[ , 1 ]-p[[ 1 ]]$se ), exp( p[[ 1 ]]$fit[ , 1 ]+p[[ 1 ]]$se ) ) )
  ranfit <- lm( y~x, data = data.frame( x = exp( p[[ 1 ]]$fit[ which( p[[ 1 ]]$x%in%as.Date( paste0( 2004:2017,"-06-30" ),
                                                                                             origin = "1970-01-01" ) ) ] ),
                                        y = RawDataStd[ TYPE==t&YEAR%in%2004:2017 ]$adj.rate ) )
  p1 <- Hmisc::xYplot( Hmisc::Cbind( exp( fit[ , 1 ] ), exp( fit[ , 1 ]+se ),
                                     exp( fit[ , 1 ]-se ) ) ~ as.Date( x, origin = "1970-01-01" ), data = p[[ 1 ]],
                       method = "filled bands", type = "l", col.fill = "lightgray", ylab = "Incidence rate ratio", xlab = "Date",
                       ylim = ran )
  p2 <- Hmisc::xYplot( Hmisc::Cbind( adj.rate, lci, uci ) ~ as.numeric( as.Date( paste0( YEAR, "-06-30" ) ) ),
                       data = RawDataStd[ TYPE==t ], ylab = "Standardized incidence",
                       ylim = predict( ranfit, data.frame( x = ran ) ) )
  print( latticeExtra::doubleYScale( p1, p2, add.ylab2 = TRUE ) )
  
  print( Hmisc::xYplot( Hmisc::Cbind( exp( fit ), exp( fit + se ), exp( fit - se ) ) ~ x, groups = sex,
                        data = rbind( with( p[[2]], data.frame( fit, se, x, sex = "Male" ) ),
                                      with( p[[3]], data.frame( fit = fit + coef( fits[[ t ]] )[ "SEX2" ], x, se,
                                                                sex = "Female" ) ) ),
                        method = "filled bands", type = "l",
                        col.fill = scales::alpha( lattice::trellis.par.get()$superpose.line$col, 0.1 ),
                        ylab = "Incidence rate ratio", xlab = "Age" ) )
}

plot( fits[[ "VASCINTER" ]], select = 0, scale = 0, trans = exp, unconditional = TRUE, n = 1000 )
plot( fits[[ "VASCOPER" ]], select = 0, scale = 0, trans = exp, unconditional = TRUE, n = 1000 )
predgrid <- data.table( expand.grid(
  TYPE = c( "VASCINTER", "VASCOPER" ),
  PROCDATEnum = as.numeric( seq( as.Date( "2004-01-01" ), as.Date( "2017-12-31" ), by = "days" ) ),
  AGE = seq( 45, 85, by = 5 ), SEX = 1:2, DOY = 0.5, DOW = "hétfő" ) )
temp <- predict( fits[[ "VASCINTER" ]], newdata = predgrid[ TYPE=="VASCINTER" ] )
temp2 <- predict( fits[[ "VASCOPER" ]], newdata = predgrid[ TYPE=="VASCOPER" ] )
predgrid2 <- rbind( cbind( predgrid[ TYPE=="VASCINTER" ], pred = temp ), cbind( predgrid[ TYPE=="VASCOPER" ], pred = temp2 ) )

xyplot( Inc ~ YEAR | factor( AGE ) + SEX, groups = TYPE,
        data = res[ TYPE%in%c( "VASCINTER", "VASCOPER" )&AGE>40, .( Inc = N/POPULATION*1e5 ), .( YEAR, TYPE, SEX, AGE ) ],
        auto.key = list( columns = 2, points = FALSE, lines = TRUE ), type = "b" )
xyplot( pred ~ as.Date( PROCDATEnum, origin = "1970-01-01" ) | factor( AGE ) + factor( SEX ), groups = TYPE, data = predgrid2,
        auto.key = list( columns = 2, points = FALSE, lines = TRUE ), type = "l" )
Hmisc::xYplot( Hmisc::Cbind( adj.rate, lci, uci ) ~ YEAR, groups = TYPE, ylim = c( 30, 130 ), xlab = "Year",
               data = RawDataStd[ TYPE%in%c( "VASCINTER", "VASCOPER", "REVASC" ) ], ylab = "Standardized incidence", type = "l",
               label.curves = list( labels = c( "All revascularization procedures\n\n", "Endovascular procedures",
                                                "Open vascular surgery procedures" ), cex = 1.5,
                                    xlim = c( 2005, 2016 ), offset = grid::unit( 45, "native") ),
               scales = list( x = list( at = 2004:2017 ) ) )