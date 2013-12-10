## function estimate struct with folowing form:
##           { probeId: <ProbeID>
##             <c.0> : {<λ.286> : <spdata>}
##           , <c.1> : {<λ.286> : <spdata>
##                     ,<λ.337> : <spdata>}
##           ,...
##           , <c.10>: {<λ.286> : <spdata>
##                     ,<λ.337> : <spdata>}
##           }
##
## return list 

calculate <- function(data){

  ## preparing data structure for output
  names  <- c('rbcTotals','rbcMeasurements')
  result <- lapply(seq(length(names))
                   ,function(x) return(0))
  names(result) <- names

  result$'persId' <- data$'persId'

  #concSeq <- c(1,2,4,6,8,10)
  concSeq <- c(1,2,4)
  result$'concSeq' <- concSeq

  waveLen <- c(286,337)
  result$'$waveLen' <- waveLen

  ## init table with calculated values
  tableData <- data.frame(data[['probeId']],sort(c(concSeq,concSeq))
                          ,waveLen)

  ## sort by wavelen
  tableData <- tableData[order(tableData[,3]),]
  rowCount <- nrow(tableData)
  
  ##   I.0 - интенсивность флуоресценции раствора мембран около 330 нм,
  ##     λвозб.=286 нм.
  ##     это файлик ddmmyyy_c0_286
  ##

  I.0 <- findMaxInWindow(data
                         ,286
                         ,0
                         ,c(320,340))
  
  ##   I.m.p - интенсивность флуоресценции раствора мембран с пиреном при
  ##     λрег.=330нм,
  ##     λ возб.=286 нм.
  ##     это файлик*И* ddmmyyy_c[1|2|4|6|8|10]_286
  ##
  
  I.m.p <- sapply(concSeq
                  ,function(x) findMaxInWindow(data,286,x,c(320,340)))
  
  ##   I.m - интенсивность флуоресценции раствора мембран с пиреном при
  ##     λрег.=374-376нм,
  ##     λ возб.=286 нм | λ возб.=337 нм.
  ##     это файлик*И* ddmmyyy_c[1|2|4|6|8|10]_X
  ##
  
  I.m <- sapply(waveLen
                ,function(y) sapply(concSeq
                                    ,function(x) findMaxInWindow(data,y,x,c(374,376))))
  dim(I.m) <- NULL
  ##   I.e - интенсивность флуоресценции  раствора мембран с пиреном при
  ##     λрег.=470-480нм,
  ##     λ возб.=286 нм | λ возб.=337 нм.
  ##     это файлик*И* ddmmyyy_c[1|2|4|6|8|10]_X
  ##
  
  I.e <- sapply(waveLen,
                function(y) sapply(concSeq
                                   ,function(x) findMaxInWindow(data,y,x,c(470,480))))
  dim(I.e) <- NULL
  ##   I.2.m - интенсивность флуоресценции раствора мембран с пиреном при
  ##     λрег.=390-394нм,
  ##     λ возб.=286 нм | λ возб.=337 нм.
  ##     это файлик*И* ddmmyyy_c[1|2|4|6|8|10]_X
  ##
  
  I.2.m <- sapply(waveLen
                  ,function(y) sapply(concSeq
                                      ,function(x) findMaxInWindow(data,y,x,c(392,396))))
  dim(I.2.m) <- NULL
  ## А потом рассчитываются следующие параметры:
  ##
  ##   N = I.e/I.m - (коэффициент эксимеризации, - для всех концентраций
  ##     (1-2-4-6-8-10) и для двух длин волн (286, 337)
  ##
  
  N <- I.e / I.m

  ##   I.0/(I.0 - I.m.p) - эффективность переноса - только для 286 нм, для всех
  ##     концентраций
  ##

  transEff = I.0 / (I.0 - I.m.p)

  ##   θ = ∆ (Iо/ Iо- I м п)/∆(1/С) (ну, это по методичке через дельту двух
  ##     соседних точек находили - я строил регрессию для зависимости I0(I0-Imp) от
  ##     1/C и брал коэффициент k - она как раз суперлинейная - y = kx+b) - ток для
  ##     286 нм.

  ## buils data for lm
  y <- transEff
  x <- 1 / concSeq

  coeff <- lm(y ~ x)$'coefficients'

  tetha <- coeff[2]
    
  ##
  ##   F2/F0 - количество доноров I-го рода (это те, которые могут тушится
  ##     пиреном) - численно равен величине, обратной отрезку по оси ардинад,
  ##     отсекаемому той самой регрессионной прямой - коэффициент b.
  ##

  F1F0 <- coeff[1]
  
  ##   Polarity - для двух длин волн, всех концентраций, отношение Im к I2m.
  
  plrty <- I.m / I.2.m

  ## !
  ## compose tabulated data
  ##
  result[['rbcTotals']] <- data.frame(list(data[['probeId']],tetha,F1F0))
  names(result$'rbcTotals') <- c('ProbeID','thetha','F0F1')

  ## I0            - I.0
  ## Imp1_286      - I.m.p ток без концентрации и ДВ
  ## Imon1_286     - I.m - без концентрации
  ## Imon1_337     - I.m
  ## I0I0Imp       - transEff
  ## Ie1_286       - I.e.286
  ## Ie1_337       - I.e.337
  ## N1_286        - N.286
  ## Polarity1_286 - plrtY.286
  
  result[['rbcMeasurements']] <- cbind(tableData
                                    ,rep(NA,rowCount)               ## I0
                                    ,c(I.m.p,rep(NA,rowCount/2))    ## Imp, complete rows to NA to supress repeating
                                    ,I.m                            ## Imon
                                    ,I.e                            ## Ie
                                    ,N                              ## N
                                    ,c(transEff,rep(NA,rowCount/2)) ## I0I0Imp
                                    ,plrty)                         ## Polarity
  
  result[['rbcMeasurements']] <- rbind(result[['rbcMeasurements']]
                                       ,c(data[['probeId']]
                                          ,0
                                          ,286
                                          ,I.0
                                          ,rep(NA
                                               ,ncol(result[['rbcMeasurements']])-3)))
  
  names(result$'rbcMeasurements') <- c('ProbeID','concentration','waveLength','I0'
                                       ,'Imp','Imon','Ie','N'
                                       ,'I0I0Imp','Polarity')

  return(result)
}

## window is two-element vector c(<min>,<max>)
findMaxInWindow <- function(data,exWaveLen,conc,window){
  max(data
      [[as.character(conc)]]
      [[as.character(exWaveLen)]]
      $'fluoCoeff'[data
                   [[as.character(conc)]]
                   [[as.character(exWaveLen)]]
                   $'waveLen' %in% seq(window[1]
                                       ,window[2])])
}
