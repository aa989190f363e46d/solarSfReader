spcInspect <- function (currentFilePath) {
  
  binData       <- file(currentFilePath, "rb")
  data          <- parseFile(binData)

  ## call here because id not relevand
  ## to internal file format
  #data[['id']] <- getIdHash(binData)
  data[['id']] <- getIdHash(currentFilePath)
  
  close(binData)
  
  return(data)
  
}

# function read file from any object
# that supported in readBin function
# return list with folowing fields:
##      timestamp      double (date and time)
##      comment        string
##      mCount         integer
##      const          integer
##      waveLenMU      string
##      fluoCoeffMU    string
##      waveLen        int vector
##      fluoCoeff      numeric vector
## output example
## spc_inspect("spectres/1/30012012_1_c0_286.fss")
## $timestamp
## [1] 40938.67
## 
## $comment
## [1] "30012012_1_c0_286FL (Ex: 286,0 \xed\xec; SEx:  5,0 \xed\xec; SEm:  3,0 \xed\xec)"
## 
## $mCount
## [1] 201
## 
## $const
## [1] 1
## 
## $waveLenMU
## [1] "\xed\xec"
## 
## $fluoCoeffMU
## [1] "\xee\xf2\xed.\xe5\xe4."
## 
## $waveLen
##   [1] 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317
## ...
## [199] 498 499 500
## 
## $fluoCoeff
##   [1] 1.089067459 1.191385865 1.294287920 1.400449514 1.513059258 1.627961516
## ...
## [199] 0.008022323 0.008368201 0.008008356
## 
parseFile <- function(file){

  # skip paddind, first record starts at 0x218
  currPos = seek(file
    ,where=0x218
    ,origin='start')
  
  ## params that define *.fss file header data binary format
  ##           ┏ datetime in delphi format (float64 where integer part is day count
  ##           ┃ since 30.12.1899 and mantiss is daypart)
  ##           ┃     ┏ comment as simple text
  ##           ┃     ┃     ┏ count of measurements as 4 byte integer
  ##           ┃     ┃     ┃     ┏ smokie constant
  ##           ┃     ┃     ┃     ┃     ┏ wave length measurement units as text
  ##           ┃     ┃     ┃     ┃     ┃     ┏ fluo. ex. measurement units as text
  ##           ┃     ┃     ┃     ┃     ┃     ┃     ┏ header ending mark
  ##           ┃     ┃     ┃     ┃     ┃     ┃     ┃                                
  fMarks <- c(0x218,0x220,0x2b0,0x2b4,0x2b8,0x2c0,0x2c8)             # offset boundaries
  fNames <- c('timestamp','comment','mCount','const','waveLenMU','fluoCoeffMU')
  tSeq   <- c('double','raw','int','int','raw','raw')                # sequence of data segment types
                                                                     # character substituit by raw because
                                                                     # it's oversmart for our goals
  tLens  <- list(1,1,4,4,4,8)                                        # length in bytes of data types
  tNames <- c('raw','character','integer','int','numeric','double')  # list of used data types names
  names(tLens) <- tNames

  ## getting header fields list
  data = lapply(seq(length(tSeq))
    ,function(i) readBin(file
                         ,tSeq[i]
                         ,as.integer((fMarks[i+1] - fMarks[i])/tLens[[tSeq[i]]])))
  names(data) <- fNames
  
  ## TODO convert raw bytes to characters via rawToChar
  temp = lapply(data[which(tSeq == 'raw')]
    ,function(x) paste(readBin(x
                               ,character()
                               ,2)
                       ,sep=" "
                       ,collapse=""))
  data[names(temp)] <- temp

  data[['ExWaveLength']] <- getExWaveLengthFromComment(data$comment)

  ## TODO convert timestamp attribute to datetime type
  
  ## skip header remain, data sequence starts from 0x141c
  currPos = seek(file
    ,where=0x141c
    ,origin='start')

  data[['waveLen']]   <- readBin(file,'numeric',data$mCount,size=4)
  data[['fluoCoeff']] <- readBin(file,'numeric',data$mCount,size=4)

  return(data)
}

## Excitation wave lenght stored in comment, after "Ex: " mark
## as float with comma delimiter
getExWaveLengthFromComment <- function(comment){

  r <- regexec("Ex: ([[:digit:]]{1,3},[[:digit:]])",comment)
  return(as.numeric(sub(","
                        ,"."
                        ,regmatches(comment
                                    ,r)
                        [[1]][2]
                        )
                    )
         )
}

## function return sha256 hash for file in argument
## in future used for appending in database as primary key
## in probe table
getIdHash <- function(file){
  d <- digest(file
              ,algo=c('sha256')
              ,file=TRUE
              ,ascii=FALSE
              ,raw=TRUE)
  return(d)
}
