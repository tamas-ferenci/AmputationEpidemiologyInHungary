library( data.table )

RawData <- fread( "input.txt" )

RawData <- RawData[ !is.na( SEX )&!is.na( TAJ )&TYPE!=""&BIRTHDATE!="", ]
for( i in grep( "DATE", names( RawData ) ) ) {
  RawData[[ i ]] <- as.Date( RawData[[ i ]], format = "%Y.%m.%d" )
}
RawData$TYPE <- as.factor( RawData$TYPE )
setkey( RawData, TAJ, TYPE, PROCDATE, SERVICEENTRYDATE, SERVICEEXITDATE )

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

RawData <- rbind( RawData[ TYPE!="AMI" ],
                  RawData[ TYPE=="AMI" ][ , { print( .GRP ); MergeSameICD( .SD ) }, .( TAJ ) ] )

setkey( RawData, TAJ, TYPE, PROCDATE, SERVICEENTRYDATE, SERVICEEXITDATE )

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
setkey( RawData, TAJ, TYPE, PROCDATE, SERVICEENTRYDATE, SERVICEEXITDATE )

RawData$AGE <- as.numeric( RawData$PROCDATE-RawData$BIRTHDATE )/365.25

IrszHnk <- fread( "IrszHnk.csv", dec = "," )
IrszCsop <- fread( "irsz_csop.csv", dec = "," )
IrszHnk <- merge( IrszHnk, IrszCsop )
IrszHnk <- IrszHnk[ !duplicated( IrszHnk$IRSZCSOP ), ]

names( RawData )[ names( RawData )=="HOMECODE" ] <- "IRSZCSOP"
RawData <- merge( RawData, IrszHnk[ , c( "IRSZCSOP", "Járás.neve", "Megye.megnevezése." ) ],
                  by = "IRSZCSOP" )
names( RawData )[ names( RawData )=="Járás.neve" ] <- "LAU1"
names( RawData )[ names( RawData )=="Megye.megnevezése." ] <- "NUTS3"

save( RawData, file = "output.dat" )
