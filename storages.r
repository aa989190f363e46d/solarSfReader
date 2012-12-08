createStorageFile <- function(path){
  
  if (file.exists(path)){
    file.rename(from=path
                , to = paste(path
                    , "backup"
                    , sep="."))
  }
  
  return(file.create(path))
}

openConnection <- function(path){
  
  driver    <- dbDriver("SQLite")
  connect   <- dbConnect(driver
                         ,dbname <- path)
  
  return(connect)
}

initTables <- function(connection){
  
  query.for.create.PersToProbes <- "CREATE TABLE 'PersToProbes' ( 'probeId'           CHAR(64)
                                                                 ,'persId'            INTEGER
                                                                 , PRIMARY KEY (probeID
                                                                               ,persId))"
  
  ## 'probeID'        key 1 
  ## 'waveLength'     key 2
  ## 'concentration'  key 3
  ##
  ## 'I0'             peptide fluo intens. * 286
  ## 'Imp'            peptide plus pyrene fluo intens. * 286
  ## 'Imon1'          first pyrene monomers fluo intens. 
  ## 'Imon2'          second pyrene monomers fluo intens. 
  ## 'Ie'             pyrene eximeres fluo intens.
  ## 'N'              microviscosity (Imon / Ie)
  ## 'I0Imp'          I0 / Imp * 286
  ## 'I0I0Imp'        I0 / (I0 + Imp) * 286
  ## 'Polarity'       micropolarity
  query.for.create.rbcMeasurements <- "CREATE TABLE 'rbcMeasurements' (
                                                      , 'probeID'           CHAR(64)
                                                      , 'waveLength'        INTEGER
                                                      , 'concentration'     INTEGER
                                                      , 'I0'                DOUBLE(10) 
                                                      , 'Imp'               DOUBLE(10) 
                                                      , 'Imon'              DOUBLE(10)
                                                      , 'Ie'                DOUBLE(10)
                                                      , 'N'                 DOUBLE(10)
                                                      , 'I0I0Imp'           DOUBLE(10)
                                                      , 'Polarity'          DOUBLE(10)
                                                      , PRIMARY KEY (probeID
                                                                   , waveLength
                                                                   , concentration))"
  

  query.for.create.idxRbcMeasurementsProbeID <- "CREATE INDEX idxRbcMeasurementsProbeID ON rbcMeasurements(probeID)"
  
  ## 'probeID'        key 1 
  ## 'tetha'          depth of peptide diving * probe
  ## 'F1F0'           part of R1 * probe
  query.for.create.rbcTotoals <- "CREATE TABLE 'rbcTotals' (
                                                      , 'probeID'           CHAR(64)
                                                      , 'tetha'             DOUBLE(10)
                                                      , 'F1F0'              DOUBLE(10)
                                                      , PRIMARY KEY (probeID))"

  query.for.create.idxRbcTotalsProbeID <- "CREATE INDEX idxRbcTotalsProbeID ON rbcMeasurements(probeID)"
  
  query.for.create.private <- "CREATE TABLE 'private' ( 'id' INTEGER PRIMARY KEY AUTOINCREMENT
                                                      , 'name' VARCHAR(100)
                                                      , 'data_db' VARCHAR(20)
                                                      , 'old' INT
                                                      , 'sex' VARCHAR(10)
                                                      , 'height' INT
                                                      , 'weight' INT
                                                      , 'sport' VARCHAR(20)
                                                      , 'exp' INT
                                                      , 'qual' INT
                                                      ,  UNIQUE('id'))"
  
  query.for.create.omega <- "CREATE TABLE 'omega' (  'id' INTEGER PRIMARY KEY
                                                    , 'name' VARCHAR(100)
                                                    , 'Adaptation' DOUBLE(10)
                                                    , 'Training' DOUBLE(10)
                                                    , 'Energy' DOUBLE(10)
                                                    , 'Psy' DOUBLE(10)
                                                    , 'Index_vegetative_eq' DOUBLE(10)
                                                    , 'Reserv' DOUBLE(10)
                                                    , 'Energy_equip' DOUBLE(10)
                                                    , 'Energy_balans' DOUBLE(10)
                                                    , 'Cycle_RE' DOUBLE(10)
                                                    , 'Cycle_TO' DOUBLE(10)
                                                    , 'Level_selfreg' DOUBLE(10)
                                                    , 'Reserv_selfreg' DOUBLE(10)
                                                    , 'Fractal_portrait' DOUBLE(10)
                                                    , 'Bio_age' DOUBLE(10)
                                                    ,  UNIQUE('id'))"
  
  qryResult = dbSendQuery(connection
    , statement <- paste( 'BEGIN'
                         ,query.for.create.rbcMeasurements
                         ,query.for.create.idxRbcMeasurementsProbeID
                         ,query.for.create.rbcTotoals
                         ,query.for.create.idxRbcTotalsProbeID
                         ,query.for.create.private
                         ,query.for.create.omega
                         ,'COMMIT'
                         ,sep=';'))
  
  sqliteCloseResult(qryResult)
}

fillPrivate <- function(data,connection){
  
  dbWriteTable(connection
               , "private"
               , data
               , overwrite = F
               , row.names = F
               , eol = "\r\n"
               , append=T)
}

fillOmega <- function(data,connection){
  
  dbWriteTable(connection
               , "omega"
               , data
               , overwrite = F
               , row.names = F
               , eol = "\r\n"
               , append=T)
}
