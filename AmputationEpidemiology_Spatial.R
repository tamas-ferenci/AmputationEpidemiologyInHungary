library( rgdal )
library( sp )
library( spdep )
library( data.table )
library( lattice )
library( epitools )
library( INLA )
library( parallel )
library( boot )
library( gridExtra )

cairo_pdf( "OSMterkepek.pdf", onefile = TRUE )
for ( i in paste0( "admin", c( 2, 5:10 ) ) ) {
  plot( readOGR( "OSM_kozighatarok", i ), main = i )
}
dev.off()

TerkepLAU1 <- readOGR( "OSM_kozighatarok", "admin7", encoding = "UTF8",
                       use_iconv = TRUE, stringsAsFactors = FALSE )
TerkepLAU1 <- TerkepLAU1[ !duplicated( TerkepLAU1$NAME ), ]
TerkepLAU1$NAME[ TerkepLAU1$NAME=="Szentlőrinci Járás" ] <- "Szentlőrinci járás"
TerkepBP <- readOGR( "OSM_kozighatarok", "admin9", encoding = "UTF8",
                     use_iconv = TRUE, stringsAsFactors = FALSE )
TerkepBP$NAME <- paste0( "Budapest ", sprintf( "%02d", as.numeric( as.roman(
  substring( TerkepBP$NAME, 1, as.numeric( gregexpr( ".", TerkepBP$NAME, fixed = TRUE ) )-1 ) ) ) ),
  ". kerület" )
TerkepLAU1 <- Reduce( rbind, list( TerkepLAU1, TerkepBP ) )
TerkepNUTS3 <- readOGR( "OSM_kozighatarok", "admin6", encoding = "UTF8",
                        use_iconv = TRUE, stringsAsFactors = FALSE )
TerkepNUTS2 <- readOGR( "OSM_kozighatarok", "admin5", encoding = "UTF8",
                        use_iconv = TRUE, stringsAsFactors = FALSE )
TerkepLAU2 <- readOGR( "OSM_kozighatarok", "admin8", encoding = "UTF8",
                       use_iconv = TRUE, stringsAsFactors = FALSE )

PopPyramid <- fread( "PopPyramid_5YR_LAU1_20152017.csv", dec = "," )
PopPyramid$GEO[ PopPyramid$GEO=="Nyíregyházai járás" ] <- "Nyíregyházi járás"
PopPyramid$SEX <- ifelse( PopPyramid$SEX=="Male", 1, 2 )
names( PopPyramid )[ 4 ] <- "LAU1"

LAU1NUTS3NUTS2NUTS1 <- fread( "LAU1NUTS3NUTS2NUTS1.csv" )

PopPyramid <- merge( PopPyramid, LAU1NUTS3NUTS2NUTS1 )

PopPyramidPopRat <- PopPyramid[ YEAR==2015, .( PopRat = POPULATION/sum( POPULATION ), LAU1 = LAU1 ),
                                by = .( NUTS3, AGE, SEX ) ]

PopPyramidNUTS3 <- fread( "PopPyramid_5YR_NUTS3_20042014.csv", dec = "," )
names( PopPyramidNUTS3 )[ 4 ] <- "NUTS3"
PopPyramidNUTS3$SEX <- ifelse( PopPyramidNUTS3$SEX=="Male", 1, 2 )

BackProjected <- merge( PopPyramidPopRat, PopPyramidNUTS3, allow.cartesian = TRUE )
BackProjected$POPULATION <- BackProjected$PopRat*BackProjected$POPULATION

PopPyramid <- rbind( PopPyramid[ , c( "YEAR", "SEX", "AGE", "LAU1", "NUTS3", "POPULATION" ) ],
                     BackProjected[ , c( "YEAR", "SEX", "AGE", "LAU1", "NUTS3", "POPULATION" ) ] )

cairo_pdf( "PopulationDistribution.pdf" )
densityplot( ~POPULATION | NAMETYPE,
             data = rbind(
               PopPyramid[ , .( POPULATION = sum( POPULATION ), NAMETYPE = "LAU1" ),
                           by = .( NAME = LAU1 ) ],
               PopPyramid[ , .( POPULATION = sum( POPULATION ), NAMETYPE = "NUTS3" ),
                           by = .( NAME = NUTS3 ) ] ),
             from = 0, scales = list( relation = "free" ) )
dev.off()

load( "output.dat" )
RawData$AGE <- cut( RawData$AGE, c( seq( 0, 85, 5 ), Inf ), right = FALSE,
                    labels = seq( 0, 85, 5 ) )
RawData$AGE <- as.numeric( levels( RawData$AGE ) )[ RawData$AGE ]

SpatialStratified <- setkey( RawData, SEX, AGE, LAU1, TYPE )[ CJ( unique( SEX ), seq( 0, 85, 5 ),
                                                                  unique( LAU1 ),
                                                                  levels( TYPE ) ), .N,
                                                              by = .EACHI ]
SpatialStratified <- rbind( SpatialStratified,
                            SpatialStratified[ TYPE%in%c( "FEMORAMP", "CRURALAMP" ),
                                               .( TYPE = "MAJORAMP", N = sum( N ) ) ,
                                               by = .( SEX, AGE, LAU1 ) ],
                            SpatialStratified[ TYPE%in%c( "VASCOPER", "VASCINTER", "VASCOPERINTER" ),
                                               .( TYPE = "VASCPROCNARROW", N = sum( N ) ) ,
                                               by = .( SEX, AGE, LAU1 ) ],
                            SpatialStratified[ TYPE%in%c( "VASCOPER", "VASCINTER", "VASCOPERINTER", "VASCEMBOL" ),
                                               .( TYPE = "VASCPROCWIDE", N = sum( N ) ) ,
                                               by = .( SEX, AGE, LAU1 ) ] )

SpatialStratified$LAU1 <- paste0( SpatialStratified$LAU1, " járás" )
SpatialStratified$LAU1 <- gsub( "ker. járás", "kerület", SpatialStratified$LAU1, fixed = TRUE )
SpatialStratified <- merge( SpatialStratified, LAU1NUTS3NUTS2NUTS1 )

SpatialStratified <- merge( SpatialStratified,
                            PopPyramid[ , .( POPULATION = sum( POPULATION ) ),
                                        .( SEX, AGE, LAU1, NUTS3 ) ],
                            by = c( "SEX", "AGE", "LAU1", "NUTS3" ) )

ESP2013 <- fread( "ESP2013.csv", dec = "," )
ESP2013[ AGE==85 ]$StdPop <- ESP2013[ AGE==85 ]$StdPop + ESP2013[ AGE==90 ]$StdPop +
  ESP2013[ AGE==95 ]$StdPop
ESP2013 <- ESP2013[ AGE <= 85 ]
SpatialStratified <- merge( SpatialStratified, ESP2013, by = "AGE" )
CountryData <- SpatialStratified[ , .( NCountry = sum( N ), PopulationCountry = sum( POPULATION ) ),
                                  by = .( SEX, AGE, TYPE ) ]
SpatialStratified <- merge( SpatialStratified, CountryData, by = c( "SEX", "AGE", "TYPE" ) )

Stds <- rbind(
  SpatialStratified[ , c( as.list( c( ageadjust.indirect( N, POPULATION, NCountry, PopulationCountry
  )$sir[ c( "observed", "exp" ) ], ageadjust.direct( N, POPULATION, stdpop = StdPop )[
    c( "crude.rate", "adj.rate" ) ] ) ),
  NAMEType = "LAU1", POPULATION = sum( POPULATION ) ), .( TYPE, NAME = LAU1 ) ],
  SpatialStratified[ , c( as.list( c( ageadjust.indirect( N, POPULATION, NCountry, PopulationCountry
  )$sir[ c( "observed", "exp" ) ], ageadjust.direct( N, POPULATION, stdpop = StdPop )[
    c( "crude.rate", "adj.rate" ) ] ) ), NAMEType = "NUTS3", POPULATION = sum( POPULATION ) ),
  .( TYPE, NAME = NUTS3 ) ],
  SpatialStratified[ , c( as.list( c( ageadjust.indirect( N, POPULATION, NCountry, PopulationCountry
  )$sir[ c( "observed", "exp" ) ], ageadjust.direct( N, POPULATION, stdpop = StdPop )[
    c( "crude.rate", "adj.rate" ) ] ) ), NAMEType = "NUTS2", POPULATION = sum( POPULATION ) ),
  .( TYPE, NAME = NUTS2 ) ],
  SpatialStratified[ , c( as.list( c( ageadjust.indirect( N, POPULATION, NCountry, PopulationCountry
  )$sir[ c( "observed", "exp" ) ], ageadjust.direct( N, POPULATION, stdpop = StdPop )[
    c( "crude.rate", "adj.rate" ) ] ) ), NAMEType = "NUTS1", POPULATION = sum( POPULATION ) ),
  .( TYPE, NAME = NUTS1 ) ] )
Stds$SIR <- Stds$observed/Stds$exp
Stds$crude.rate <- Stds$crude.rate*100000
Stds$adj.rate <- Stds$adj.rate*100000

write.csv2( Stds, "Stds.csv", row.names = FALSE )

cor( Stds[ TYPE=="MAJORAMP"&NAMEType=="LAU1", c( "crude.rate", "SIR", "adj.rate" ) ] )

cairo_pdf( "PopPyramidsCompare.pdf" )
xyplot( V1 ~ AGE, data = rbind( ESP2013[ , .( AGE, V1 = StdPop/sum( StdPop )*100, Type = "ESP2013" ) ],
                                PopPyramid[ SEX==1 ][ , .( sum( POPULATION ) ), .( SEX, AGE ) ][
                                  , .( AGE, V1 = V1/sum( V1 )*100, Type = "Hungarian - Males" ) ],
                                PopPyramid[ SEX==2 ][ , .( sum( POPULATION ) ), .( SEX, AGE ) ][
                                  , .( AGE, V1 = V1/sum( V1 )*100, Type = "Hungarian - Females" ) ] ),
        type = "b", groups = Type, auto.key = list( columns = 3, points = FALSE, lines = TRUE ),
        xlab = "Age", ylab = "Proportion [%]")
dev.off()

labs <- c( crude.rate = "Crude incidence [/100,000 person-years]",
           SIR = "Indirect standardized incidence",
           adj.rate = "Direct standardized incidence [/100,000 person-years]" )

for( t in unique( Stds$TYPE ) ) {
  cairo_pdf( paste0( "./Results/Terkep", t, ".pdf" ), onefile = TRUE )
  
  for( v in c( "crude.rate", "SIR", "adj.rate" ) ) {
    ran <- pmax( extendrange( range( Stds[ TYPE==t&NAMEType=="LAU1", v, with = FALSE ] ) ), 0 )
    p1 <- spplot( merge( TerkepLAU1, Stds[ TYPE==t&NAMEType=="LAU1" ] ), v,
                  col.regions = colorRampPalette( c( "green", "red" ) )( 1000 ),
                  main = labs[ v ], at = seq( ran[1], ran[2], length.out = 1000 ),
                  sp.layout = list( "sp.polygons", TerkepNUTS3, lwd = 2, col = "black",
                                    first = FALSE ) )
    p2 <- spplot( merge( TerkepBP, Stds[ TYPE==t&NAMEType=="LAU1" ] ), v,
                  col.regions = colorRampPalette( c( "green", "red" ) )( 1000 ),
                  at = seq( ran[1], ran[2], length.out = 1000 ) )
    grid.arrange( p1, p2 )
    
    print( densityplot( as.formula( paste( "~", v ) ), data = Stds[ TYPE==t&NAMEType=="LAU1" ],
                        xlab = labs[ v ], ylab = "" ) )
    
    ran <- pmax( extendrange( range( Stds[ TYPE==t&NAMEType=="NUTS3", v, with = FALSE ] ) ), 0 )
    print( spplot( merge( TerkepNUTS3, Stds[ TYPE==t&NAMEType=="NUTS3" ] ), v,
                   col.regions = colorRampPalette( c( "green", "red" ) )( 1000 ),
                   main = labs[ v ], at = seq( ran[1], ran[2], length.out = 1000 ) ) )
    
    print( densityplot( as.formula( paste( "~", v ) ), data = Stds[ TYPE==t&NAMEType=="NUTS3" ],
                        xlab = labs[ v ], ylab = "" ) )
    
  }
  
  print( splom( Stds[ TYPE==t&NAMEType=="LAU1", c( "crude.rate", "SIR", "adj.rate" ) ],
                varnames = gsub( "(.{1,15})(\\s|$)", "\\1\n", labs ), xlab = "",
                main = "LAU 1" ) )
  # https://stackoverflow.com/questions/2351744/insert-line-breaks-in-long-string-word-wrap
  
  print( splom( Stds[ TYPE==t&NAMEType=="NUTS3", c( "crude.rate", "SIR", "adj.rate" ) ],
                varnames = gsub( "(.{1,15})(\\s|$)", "\\1\n", labs ), xlab = "",
                main = "NUTS 3" ) )
  
  dev.off()
}

write.csv2(
  melt( Stds[ NAMEType%in%c( "LAU1", "NUTS3", "NUTS2" ) ], id.vars = c( "TYPE", "NAME", "NAMEType" ),
        measure.vars = c( "crude.rate", "SIR" ) )[ ,
                                                   as.list( do.call( c, lapply( c( geary.test, moran.test ), function( fun )
                                                     with( fun( value, nb2listw( poly2nb( eval( parse(
                                                       text = paste0( "Terkep", NAMEType ) ) ) ) ) ),
                                                       c( estimate[ 1 ], p.value = p.value ) ) ) ) ),
                                                   by = .( TYPE, variable, NAMEType ) ],
  "Autocorr.csv", row.names = FALSE )

source( "SAVAmetrics.R" )

cl <- makeCluster( detectCores()-1 )
clusterEvalQ( cl, source( "SAVAmetrics.R" ) )

write.csv2( Stds[ NAMEType!="NUTS1", SAVAwCI( adj.rate, observed, exp, POPULATION, cl = cl ),
                  by = .( TYPE, NAMEType ) ], "SmallAreaMetrics.csv" )

stopCluster( cl )