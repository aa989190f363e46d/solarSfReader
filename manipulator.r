curr.sep <- .Platform[['file.sep']]

## '.' denote that path is relative
sp.root.dir <- c('.','spectres')

## regex for spectres filenames pattern
## for example, match with "01012001_15_c5_550.fss"
## also can be used for extracting parts from filename
## using regmatches function
fl.name.regex <- "([[:digit:]]{8})_([[:digit:]]{1,3})_c([[:digit:]]{1,2})_([[:digit:]]{3}).fss"

## source('./readSpc.r')

## function behavior asume that all probe files
## placed in single directory and specialy named
## to be matched by regex above
getProbeData <- function(probeID = NA,spCat = sp.root.dir){
  probeData = list()
  if (!is.na(probeID)){
    for (msmnt.fl in dir(paste(paste(spCat
                                     ,collapse=curr.sep)
                               ,probeID
                               ,sep=curr.sep)
                         ,pattern=fl.name.regex,
                         ,full.names=T)){
      
      r <- regexec(fl.name.regex,msmnt.fl)
      ## 2: date
      ## 3: probeID
      ## 4: pyrene concentration
      ## 5: excitation wavelength 
      conditions <- regmatches(msmnt.fl,r)[[1]][3:5]

      if (is.null(probeData[[conditions[2]]])){
        probeData[[conditions[2]]] <- list()
      }
      
      probeData[[conditions[2]]][[conditions[3]]] <- spcInspect(msmnt.fl)
      
    }
  }
  
  return(probeData)
}

## function XOR all individual data file hashes
getProbeIdHash <- function(dataStruct){
  prHash <- paste(as.raw(Reduce(bitXor
                                ,(Reduce(c
                                         ,sapply(names(dataStruct)
                                                 ,function(x) lapply(names(dataStruct[[x]])
                                                                     ,function(y) dataStruct[[x]]
                                                                                            [[y]]
                                                                                            [['id']]))))))
                  ,collapse='')
  
  return(prHash)
}

## spectres catalog traverse
traverse <- function(spCat = sp.root.dir){
  
  DATA = list()
  
  for (probeID in dir(paste(spCat
                           ,collapse=curr.sep
                           ,sep=curr.sep))){
    
    prData             <- getProbeData(probeID
                                      ,spCat)
    prId               <- getProbeIdHash(prData)
    
    prData[['persId']]  <- probeID
    prData[['probeId']] <- prId

    DATA[[prId]]       <- if(is.na(prData)) NA else calculate(prData)
    
  }
  
  return(DATA)
  
}

prepareDataForWrite <- function(dataSet){
  
  
  
}
