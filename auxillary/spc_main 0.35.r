library(RSQLite);
## library(Hmisc);

if (file.exists("E:/Work/Aspirantura/DataBase"))
  { 
    homedirectorypath <- "E:/Work/Aspirantura/DataBase"
    setwd(homedirectorypath)
  } else {
    homedirectorypath <- "/home/angel/Desktop/Database/"
    setwd(homedirectorypath)
    Sys.setlocale('LC_ALL','C') # it necassary for good working in english locale of linux PC
  }



      

spc_inspect <- function (current_file_path) {
  
	# extract file title
	bin.data       <- file(current_file_path, "rb")
  size           <- file.info(current_file_path)$size
  arr.data       <- readBin(bin.data
                        , double()
                        , n     <- size
                        , size  <- 4)

  I.data         <- arr.data[1489:1689]
  close(bin.data)

	# open file for binary reading
	bin.data       <- file(current_file_path
                      , "rb")
  greasy_title   <- readBin(bin.data
                          , character()
                          , n <- 533)
	close(bin.data)
  
  # deleting a garbage symbols
  title.data <- unlist(strsplit(greasy_title[532], "@"))
  if (length(title.data) == 3) {title.data[2]=title.data[3]}
  if (length(unlist(strsplit(title.data[2],"_"))) > 3) {	  
	  title.data <- title.data[2]
  } else { 
    title.data <- unlist(strsplit(greasy_title[533], "@"))
	  title.data <- title.data[2]
  }
  
  # parsing title.data
  esse <- unlist(strsplit(title.data,"_"))
  
  return(c(esse,I.data))
  }

spc_calc = function (current_dir) {
  
  spectrum.default = vector();
  spectrum.c1_286 = vector(); spectrum.c1_337 = vector()
  spectrum.c2_286 = vector(); spectrum.c2_337 = vector()
  spectrum.c4_286 = vector(); spectrum.c4_337 = vector()
  spectrum.c6_286 = vector(); spectrum.c6_337 = vector()
  spectrum.c8_286 = vector(); spectrum.c8_337 = vector()
  spectrum.c10_286 = vector(); spectrum.c10_337 = vector()
  Intens = list()

for (i in dir(path=current_dir, pattern="*.fss")) {
  #print(sub(" ","",paste(current_dir, i)))
  data.inspect <- spc_inspect(paste(current_dir, i, sep=""))
  
  title.inspect <- data.inspect[1:4]
  
  spectrum.inspect <- as.numeric(data.inspect[5:length(data.inspect)])
    
  a = title.inspect[3]
  b = title.inspect[4]
  id_from_date_and_id = paste(title.inspect[1], title.inspect[2], sep="_")

  Intens$id = title.inspect[2]
  #Intens$id = 81 # for manual adding datasets in db-file
  Intens$probe_id = id_from_date_and_id
    
  if((a=="c0") & (b=="286")) { spectrum.default[300:500] = spectrum.inspect}
    else {if((a=="c1") & (b=="286")){#print("c1_286");
                              spectrum.c1_286[300:500] = spectrum.inspect}
    else {if((a=="c1") & (b=="337")) {#print("c1_337");
                              spectrum.c1_337[350:550] = spectrum.inspect}
        else {if((a=="c2") & (b=="286")) {#print("c2_286");
                              spectrum.c2_286[300:500] = spectrum.inspect}
        else {if((a=="c2") & (b=="337")) {#print("c2_337");
                              spectrum.c2_337[350:550] = spectrum.inspect}
            else {if((a=="c4") & (b=="286")) {#print("c4_286");
                              spectrum.c4_286[300:500] = spectrum.inspect}
            else {if((a=="c4") & (b=="337")) {#print("c4_337");
                              spectrum.c4_337[350:550] = spectrum.inspect}
                else {if((a=="c6") & (b=="286")) {#print("c6_286");
                              spectrum.c6_286[300:500] = spectrum.inspect}
                else {if((a=="c6") & (b=="337")) {#print("c6_337");
                              spectrum.c6_337[350:550] = spectrum.inspect}
                    else {if((a=="c8") & (b=="286")) {#print("c8_286");
                              spectrum.c8_286[300:500] = spectrum.inspect}
                    else {if((a=="c8") & (b=="337")) {#print("c8_337");
                              spectrum.c8_337[350:550] = spectrum.inspect}
                        else {if((a=="c10") & (b=="286")) {#print("c10_286");
                              spectrum.c10_286[300:500] = spectrum.inspect}
                        else {if((a=="c10") & (b=="337")) {#print("c10_337");
                              spectrum.c10_337[350:550] = spectrum.inspect}
                            else {
                              #cat("WARNING!!!11 T1tle not recognized!",i,"\n")
                              }
                            }}
                        }}
                    }}
                }}
            }}
        }}
  }
  
#   cummul <<- c(title.inspect[2],spectrum.default[300:500], spectrum.c1_286[300:500], spectrum.c1_337[350:550], 
#              spectrum.c2_286[300:500], spectrum.c2_337[350:550], spectrum.c4_286[300:500], 
#              spectrum.c4_337[350:550], spectrum.c6_286[300:500], spectrum.c6_337[350:550], 
#              spectrum.c8_286[300:500], spectrum.c8_337[350:550], spectrum.c10_286[300:500],
#              spectrum.c10_337[350:550])
#   
#   cummul <<- c(title.inspect[2], spectrum.default[300:500], spectrum.c10_337[350:550])
#   smbvar = ""
#  
#   for(x in 1:length(cummul))
#     {
#       smbvar = paste(smbvar, cummul[x], sep=";")
#     }
#   cummul = smbvar
#   write(cummul, "test.csv", append = T)


  # I0 - ????????????? ??????? ??? ?????? ? ????????? 330 ?? ??? lambda=286
  # Imp - ????????????? ??????? ? ??????????? ?????? ? ????????? 330 ?? ??? lambda=286
  # Imon - ????????????? ????????? ?????? ? ????????? 374-376 ??, lambda=286
  # Iex - ????????????? ????????? ?????? ? ????????? 470-480 ??, lambda=337
  # N - ??????????? ????????????? ??????
  # I0Imp - ????????? ????????????? ????????????? ????????????? ??? ?????? ? ? ??????????? ????????
  # I0I0Imp - ????????? ????????????? ????????????? ????????????? ??? ?????? ? ???????? ??????????????
  # tetha - ?????????????? ??????? ?????????? ????? ? ????????
  # F1F0 - ???? ??????? ??????? ??????? ????
  # Polarity - ?????????? ?????????????? ??????
  
  # ?????????? ???????? ???????? ????????????? ????????? ??? ??????, c0_286

  interval_def = seq(320,350,1)
  
Intens$I0 = max(spectrum.default[interval_def])
max_def = which(spectrum.default[interval_def] == Intens$I0)+319

  # ?????????? ???????? ???????? ????????????? ???????? ? ???????, 286 nm

  interval_mp = seq(320,340,1)

Intens$Imp1 = max(spectrum.c1_286[interval_mp])
max_mp1 = which(spectrum.c1_286[interval_mp] == Intens$Imp1)+319

Intens$Imp2 = max(spectrum.c2_286[interval_mp])
max_mp2 = which(spectrum.c2_286[interval_mp] == Intens$Imp2)+319

Intens$Imp4 = max(spectrum.c4_286[interval_mp])
max_mp4 = which(spectrum.c4_286[interval_mp] == Intens$Imp4)+319
  
Intens$Imp6 = max(spectrum.c6_286[interval_mp])
max_mp6 = which(spectrum.c6_286[interval_mp] == Intens$Imp6)+319
  
Intens$Imp8 = max(spectrum.c8_286[interval_mp])
max_mp8 = which(spectrum.c8_286[interval_mp] == Intens$Imp8)+319
  
Intens$Imp10 = max(spectrum.c10_286[interval_mp])
max_mp10 = which(spectrum.c10_286[interval_mp] == Intens$Imp10)+319

  # ?????????? ???????? ???????? ????????? ??????, 286nm

  interval_mon_286 = seq(370,380,1)

Intens$Imon1_286 = max(spectrum.c1_286[interval_mon_286])
max_mon1_286 = which(spectrum.c1_286[interval_mon_286] == Intens$Imon1_286)+369

Intens$Imon2_286 = max(spectrum.c2_286[interval_mon_286])
max_mon2_286 = which(spectrum.c2_286[interval_mon_286] == Intens$Imon2_286)+369
  
Intens$Imon4_286 = max(spectrum.c4_286[interval_mon_286])
max_mon4_286 = which(spectrum.c4_286[interval_mon_286] == Intens$Imon4_286)+369

Intens$Imon6_286 = max(spectrum.c6_286[interval_mon_286])
max_mon6_286 = which(spectrum.c6_286[interval_mon_286] == Intens$Imon6_286)+369

Intens$Imon8_286 = max(spectrum.c8_286[interval_mon_286])
max_mon8_286 = which(spectrum.c8_286[interval_mon_286] == Intens$Imon8_286)+369

Intens$Imon10_286 = max(spectrum.c10_286[interval_mon_286])
max_mon10_286 = which(spectrum.c10_286[interval_mon_286] == Intens$Imon10_286)+369

  # ?????????? ???????? ???????? ????????? ??????, 337nm
  interval_mon_337 = seq(370,380,1)

Intens$Imon1_337 = max(spectrum.c1_337[interval_mon_337])
max_mon1_337 = which(spectrum.c1_337[interval_mon_337] == Intens$Imon1_337)+369

Intens$Imon2_337 = max(spectrum.c2_337[interval_mon_337])
max_mon2_337 = which(spectrum.c2_337[interval_mon_337] == Intens$Imon2_337)+369
  
Intens$Imon4_337 = max(spectrum.c4_337[interval_mon_337])
max_mon4_337 = which(spectrum.c4_337[interval_mon_337] == Intens$Imon4_337)+369

Intens$Imon6_337 = max(spectrum.c6_337[interval_mon_337])
max_mon6_337 = which(spectrum.c6_337[interval_mon_337] == Intens$Imon6_337)+369

Intens$Imon8_337 = max(spectrum.c8_337[interval_mon_337])
max_mon8_337 = which(spectrum.c8_337[interval_mon_337] == Intens$Imon8_337)+369

Intens$Imon10_337 = max(spectrum.c10_337[interval_mon_337])
max_mon10_337 = which(spectrum.c10_337[interval_mon_337] == Intens$Imon10_337)+369

  # ?????????? ???????? ???????? ????????? ??????, 286nm
  interval_e_286 = seq(470,480,1)

Intens$Ie1_286 = max(spectrum.c1_286[interval_e_286])
max_e1_286 = which(spectrum.c1_286[interval_e_286] == Intens$Ie1_286)+469

Intens$Ie2_286 = max(spectrum.c2_286[interval_e_286])
max_e2_286 = which(spectrum.c2_286[interval_e_286] == Intens$Ie2_286)+469

Intens$Ie4_286 = max(spectrum.c4_286[interval_e_286])
max_e4_286 = which(spectrum.c4_286[interval_e_286] == Intens$Ie4_286)+469

Intens$Ie6_286 = max(spectrum.c6_286[interval_e_286])
max_e6_286 = which(spectrum.c6_286[interval_e_286] == Intens$Ie6_286)+469
 
Intens$Ie8_286 = max(spectrum.c8_286[interval_e_286])
max_e8_286 = which(spectrum.c8_286[interval_e_286] == Intens$Ie8_286)+469

Intens$Ie10_286 = max(spectrum.c10_286[interval_e_286])
max_e10_286 = which(spectrum.c10_286[interval_e_286] == Intens$Ie10_286)+469
 
    # ?????????? ???????? ???????? ????????? ??????, 337nm
    
  interval_e_337 = seq(470,480,1)

Intens$Ie1_337 = max(spectrum.c1_337[interval_e_337])
max_e1_337 = which(spectrum.c1_337[interval_e_337] == Intens$Ie1_337)+469

Intens$Ie2_337 = max(spectrum.c2_337[interval_e_337])
max_e2_337 = which(spectrum.c2_337[interval_e_337] == Intens$Ie2_337)+469
  
Intens$Ie4_337 = max(spectrum.c4_337[interval_e_337])
max_e4_337 = which(spectrum.c4_337[interval_e_337] == Intens$Ie4_337)+469

Intens$Ie6_337 = max(spectrum.c6_337[interval_e_337])
max_e6_337 = which(spectrum.c6_337[interval_e_337] == Intens$Ie6_337)+469

Intens$Ie8_337 = max(spectrum.c8_337[interval_e_337])
max_e8_337 = which(spectrum.c8_337[interval_e_337] == Intens$Ie8_337)+469

Intens$Ie10_337 = max(spectrum.c10_337[interval_e_337])
max_e10_337 = which(spectrum.c10_337[interval_e_337] == Intens$Ie10_337)+469

  # ????????? ??????????? ????????????? ?????? ? ?????????? ???????
  Intens$N1_286 = (Intens$Ie1_286 / Intens$Imon1_286)
  Intens$N2_286 = (Intens$Ie2_286 / Intens$Imon2_286)
  Intens$N4_286 = Intens$Ie4_286 / Intens$Imon4_286
  Intens$N6_286 = Intens$Ie6_286 / Intens$Imon6_286
  Intens$N8_286 = Intens$Ie8_286 / Intens$Imon8_286
  Intens$N10_286 = Intens$Ie10_286 / Intens$Imon10_286
  # ????????? ??????????? ????????????? ?????? ? ????? ???????? ?????
  Intens$N1_337 = Intens$Ie1_337 / Intens$Imon1_337
  Intens$N2_337 = Intens$Ie2_337 / Intens$Imon2_337
  Intens$N4_337 = Intens$Ie4_337 / Intens$Imon4_337
  Intens$N6_337 = Intens$Ie6_337 / Intens$Imon6_337
  Intens$N8_337 = Intens$Ie8_337 / Intens$Imon8_337
  Intens$N10_337 = Intens$Ie10_337 / Intens$Imon10_337
  # ?????????? ??????????? F0/F
  Intens$I0Imp1 = Intens$I0 / Intens$Imp1
  Intens$I0Imp2 = Intens$I0 / Intens$Imp2
  Intens$I0Imp4 = Intens$I0 / Intens$Imp4
  Intens$I0Imp6 = Intens$I0 / Intens$Imp6
  Intens$I0Imp8 = Intens$I0 / Intens$Imp8
  Intens$I0Imp10 = Intens$I0 / Intens$Imp10
  # ????????? ????????? I0 / (I0 - Im+p)
  Intens$I0I0Imp1 = Intens$I0 / (Intens$I0 - Intens$Imp1)
  Intens$I0I0Imp2 = Intens$I0 / (Intens$I0 - Intens$Imp2)
  Intens$I0I0Imp4 = Intens$I0 / (Intens$I0 - Intens$Imp4)
  Intens$I0I0Imp6 = Intens$I0 / (Intens$I0 - Intens$Imp6)
  Intens$I0I0Imp8 = Intens$I0 / (Intens$I0 - Intens$Imp8)
  Intens$I0I0Imp10 = Intens$I0 / (Intens$I0 - Intens$Imp10)
  # ?????? theta ? F1/F0 (??????? ??????? ??????? ????)
  c.revers <- 1/c(1,2,4,6,8,10)
  effective.energy.transfer = c(Intens$I0I0Imp1, Intens$I0I0Imp2, Intens$I0I0Imp4, Intens$I0I0Imp6, Intens$I0I0Imp8, Intens$I0I0Imp10)
  Intens$tetha = lm(effective.energy.transfer ~ c.revers)$coefficient[2]
  Intens$F1F0 = 1/lm(effective.energy.transfer ~ c.revers)$coefficient[1]

  # ?????????? ?????????????? ??????, 286 nm
  
   interval_polarity_286 = seq(380,400,1)
   
   Intens$Polarity1_286 = Intens$Ie1_286 / max(spectrum.c1_286[interval_polarity_286])
   Intens$Polarity2_286 = Intens$Ie2_286 / max(spectrum.c2_286[interval_polarity_286])
   Intens$Polarity4_286 = Intens$Ie4_286 / max(spectrum.c4_286[interval_polarity_286])
   Intens$Polarity6_286 = Intens$Ie6_286 / max(spectrum.c6_286[interval_polarity_286])
   Intens$Polarity8_286 = Intens$Ie8_286 / max(spectrum.c8_286[interval_polarity_286])
   Intens$Polarity10_286 = Intens$Ie10_286 / max(spectrum.c10_286[interval_polarity_286])
  
  # ?????????? ?????????????? ??????, 337 nm
  
  interval_polarity_337 = seq(380,400,1)

  Intens$Polarity1_337 = Intens$Ie1_337 / max(spectrum.c1_337[interval_polarity_337])
  Intens$Polarity2_337 = Intens$Ie2_337 / max(spectrum.c2_337[interval_polarity_337])
  Intens$Polarity4_337 = Intens$Ie4_337 / max(spectrum.c4_337[interval_polarity_337])
  Intens$Polarity6_337 = Intens$Ie6_337 / max(spectrum.c6_337[interval_polarity_337])
  Intens$Polarity8_337 = Intens$Ie8_337 / max(spectrum.c8_337[interval_polarity_337])
  Intens$Polarity10_337 = Intens$Ie10_337 / max(spectrum.c10_337[interval_polarity_337])
  
  
  #Intens <<- Intens # ??????? ?????????? ? ?????????? ???????????? ????
  
  spc_write(Intens) # ?????????? ?????????? ???????? ? ??
}

  spc_write = function(value_for_insert, target_db = "rbc") {
      # this function work with DB self-made instruments only
      # it not use SQL query and write data directly
      if(file.exists("spc.db"))
        {print("DB already exist!")}
      else {spc_create_db_structure()}
      
        driver<-dbDriver("SQLite")
        connect<-dbConnect(driver, dbname = "spc.db")
        
        dbWriteTable(connect, "rbc", as.data.frame(value_for_insert), overwrite = F, row.names = F, eol = "\r\n", append=T ) 
          
        sqliteCloseConnection(connect); sqliteCloseDriver(driver)
    }


  spc_view = function(sql_query= "select * from rbc") {
      
      driver<-dbDriver("SQLite")
      connect<-dbConnect(driver, dbname = paste(homedirectorypath, "/spc.db", sep = ""))
      query01 <- dbSendQuery(connect, statement =sql_query)

      dataframe <- fetch(query01);
      sqliteCloseResult(query01); sqliteCloseConnection(connect); sqliteCloseDriver(driver)
      return(dataframe)
    }

  spc_illustrate = function(query_for_spc_view = "SELECT * FROM rbc") {
  
      # formation control and experimental groups from DB  
      sport_id <<- as.numeric(unlist(
        spc_view("SELECT id FROM private WHERE sport <> 'control'")))# AND old >= 18")))
      control_id <<- as.numeric(unlist(
        spc_view("SELECT id FROM private WHERE sport = 'control'")))# AND old >= 18")))

      dataframe <- spc_view(query_for_spc_view)
            
      concentration = c(1,2,4,6,8,10)
      
      # calculating data for microviscosity plot
      sport_annular = 1/mean(dataframe[sport_id, 34:39])
      sport_general = 1/mean(dataframe[sport_id, 40:45])
      control_annular = 1/mean(dataframe[control_id,34:39])
      control_general = 1/mean(dataframe[control_id,40:45])
      
      # drawing plot
      plot(concentration, sport_annular, type = "l", col = "red", lty = "dashed", main = "RED: sport, GREEN: control \n DASHED: annular, NORMAL: general",
           ylab = "Microviscosity", xlab="Concentration"); points(concentration, sport_annular, col = "red")
      
      lines(concentration, sport_general, col = "red"); points(concentration, sport_general, col = "red")
      lines(concentration, control_general, col = "green"); points(concentration, control_general, col = "green")
      lines(concentration, control_annular, col = "green", lty = "dashed"); points(concentration, control_annular, col = "green")

      # calculating data for correlating with theoretical Shtern-Folmer equation
      F1 = mean(dataframe$I0/dataframe$Imp1)
      F2 = mean(dataframe$I0/dataframe$Imp2)
      F4 = mean(dataframe$I0/dataframe$Imp4)
      F6 = mean(dataframe$I0/dataframe$Imp6)
      F8 = mean(dataframe$I0/dataframe$Imp8)
      F10 = mean(dataframe$I0/dataframe$Imp10)

      print(paste("Correlation practical data with theoretical predicted values:", cor(c(F1, F2, F4, F6, F8, F10), concentration)))
      
      # calculating effectivity energy transfer
      Z1 = mean(dataframe$I0I0Imp1[sport_id])
      Z2 = mean(dataframe$I0I0Imp2[sport_id])
      Z4 = mean(dataframe$I0I0Imp4[sport_id])
      Z6 = mean(dataframe$I0I0Imp6[sport_id])
      Z8 = mean(dataframe$I0I0Imp8[sport_id])
      Z10 = mean(dataframe$I0I0Imp10[sport_id])
      
      Z <- c(Z1, Z2, Z4, Z6, Z8, Z10)
      c.revers <- 1/concentration
      # creating linear model Z ~ 1/c
      l.sport <- lm(Z ~ c.revers)
      
      
      Z1C = mean(dataframe$I0I0Imp1[control_id])
      Z2C = mean(dataframe$I0I0Imp2[control_id])
      Z4C = mean(dataframe$I0I0Imp4[control_id])
      Z6C = mean(dataframe$I0I0Imp6[control_id])
      Z8C = mean(dataframe$I0I0Imp8[control_id])
      Z10C = mean(dataframe$I0I0Imp10[control_id])
      
      ZC <- c(Z1C, Z2C, Z4C, Z6C, Z8C, Z10C)
      l.control <- lm(ZC ~ c.revers)
      
      plot(c.revers, Z, type = "l", col="red", main = "RED: sport, GREEN: control", ylab = "I0/(I0-I)", xlab = "Concentration")
      points(c.revers, Z, col = "red")
      lines(c.revers, ZC, col = "green"); points(c.revers, ZC, col = "green")

      curve(l.control$coefficients[2]*x+l.control$coefficients[1], add = T, lty = "dashed", col = "green")
      curve(l.sport$coefficients[2]*x+l.sport$coefficients[1], add = T, lty = "dashed", col = "red")
      
      # tetha and F1/F0 values
      boxplot(dataframe$tetha[sport_id], dataframe$tetha[control_id], main = "Tetha values for sport and control group")
      boxplot(dataframe$F1F0[sport_id], dataframe$F1F0[control_id], main = "F1/F0 values for sport and control group") 
      
      # calculating values of polarity microenvironment pyrene
      sport_polarity286 = mean(dataframe[sport_id, 60:65])
      sport_polarity337 = mean(dataframe[sport_id, 66:71])
      control_polarity286 = mean(dataframe[control_id,60:65])
      control_polarity337 = mean(dataframe[control_id,66:71])
      
      plot(concentration, sport_polarity337, type = "l", col = "red", main = "RED: sport, GREEN: control \n DASHED: annular, NORMAL: general",
           ylab = "Polarity", xlab = "Concentration"); points(concentration, sport_polarity337, col = "red")
      
      lines(concentration, sport_polarity286, col = "red", lty = "dashed"); points(concentration, sport_polarity286, col = "red")
      lines(concentration, control_polarity337, col = "green"); points(concentration, control_polarity337, col = "green")
      lines(concentration, control_polarity286, col = "green", lty = "dashed"); points(concentration, control_polarity286, col = "green")
      
      #differencies of microviscosity in control and experimental groups 
  print("p-values of Wilcoxon test for microviscosity annular lipid found")
  cat(wilcox.test(dataframe$N1_286[control_id], dataframe$N1_286[sport_id])$p.value, wilcox.test(dataframe$N2_286[control_id], dataframe$N2_286[sport_id])$p.value,
      wilcox.test(dataframe$N4_286[control_id], dataframe$N4_286[sport_id])$p.value, wilcox.test(dataframe$N6_286[control_id], dataframe$N6_286[sport_id])$p.value,
      wilcox.test(dataframe$N8_286[control_id], dataframe$N8_286[sport_id])$p.value, wilcox.test(dataframe$N10_286[control_id], dataframe$N10_286[sport_id])$p.value,"\n")
  print("p-values of Wilcoxon test for microviscosity general lipid found")
  cat(wilcox.test(dataframe$N1_337[control_id], dataframe$N1_337[sport_id])$p.value, wilcox.test(dataframe$N2_337[control_id], dataframe$N2_337[sport_id])$p.value,
      wilcox.test(dataframe$N4_337[control_id], dataframe$N4_337[sport_id])$p.value, wilcox.test(dataframe$N6_337[control_id], dataframe$N6_337[sport_id])$p.value,
      wilcox.test(dataframe$N8_337[control_id], dataframe$N8_337[sport_id])$p.value, wilcox.test(dataframe$N10_337[control_id], dataframe$N10_337[sport_id])$p.value,"\n")

      # difference in effectivity energy transfer
      print("p-values of Wilcox test for I0/(I0-I)")
      cat(wilcox.test(dataframe$I0Imp1[control_id], dataframe$I0Imp1[sport_id])$p.value, wilcox.test(dataframe$I0Imp2[control_id], dataframe$I0Imp2[sport_id])$p.value,
          wilcox.test(dataframe$I0Imp4[control_id], dataframe$I0Imp4[sport_id])$p.value, wilcox.test(dataframe$I0Imp6[control_id], dataframe$I0Imp6[sport_id])$p.value,
          wilcox.test(dataframe$I0Imp8[control_id], dataframe$I0Imp8[sport_id])$p.value, wilcox.test(dataframe$I0Imp10[control_id], dataframe$I0Imp10[sport_id])$p.value,"\n")
      
      # difference in tetha values
      print("p-values of Wilcox test for tetha")
      cat(wilcox.test(dataframe$tetha[control_id], dataframe$tetha[sport_id])$p.value,"\n")
      

      # difference in F1/F0
      print("p-values of Wilcox test for F1/F0")
      cat(wilcox.test(dataframe$F1F0[control_id], dataframe$F1F0[sport_id])$p.value,"\n")
      
      # difference in polarity of microenvironment
      print("p-values of Wilcoxon test for polarity of pyren's microenvironment in annular lipid found")
      cat(wilcox.test(dataframe$Polarity1_286[control_id], dataframe$Polarity1_286[sport_id])$p.value, wilcox.test(dataframe$Polarity2_286[control_id], dataframe$Polarity2_286[sport_id])$p.value,
          wilcox.test(dataframe$Polarity4_286[control_id], dataframe$Polarity4_286[sport_id])$p.value, wilcox.test(dataframe$Polarity6_286[control_id], dataframe$Polarity6_286[sport_id])$p.value,
          wilcox.test(dataframe$Polarity8_286[control_id], dataframe$Polarity8_286[sport_id])$p.value, wilcox.test(dataframe$Polarity10_286[control_id], dataframe$Polarity10_286[sport_id])$p.value,"\n")
      print("p-values of Wilcoxon test for polarity of pyren's microenvironment in general lipid found")
      cat(wilcox.test(dataframe$Polarity1_337[control_id], dataframe$Polarity1_337[sport_id])$p.value, wilcox.test(dataframe$Polarity2_337[control_id], dataframe$Polarity2_337[sport_id])$p.value,
          wilcox.test(dataframe$Polarity4_337[control_id], dataframe$Polarity4_337[sport_id])$p.value, wilcox.test(dataframe$Polarity6_337[control_id], dataframe$Polarity6_337[sport_id])$p.value,
          wilcox.test(dataframe$Polarity8_337[control_id], dataframe$Polarity8_337[sport_id])$p.value, wilcox.test(dataframe$Polarity10_337[control_id], dataframe$Polarity10_337[sport_id])$p.value,"\n")
      
    
      # cluster analysis
      data.dist <- dist(scale(dataframe[c(control_id, sport_id), 34:45]), method="minkowski");
      data.h <<- hclust(data.dist, method="ward");
      plot(data.h)
      #length(which((sport_id == c(2,4))>0))
      }
  
    spc_compare = function(target_id = 1) {
      dataframe = spc_view()
      
      sport_id <- as.numeric(unlist(spc_view("SELECT id FROM private WHERE sport <> 'control' AND old >18")))
      control_id <- as.numeric(unlist(spc_view("SELECT id FROM private WHERE sport = 'control' AND old > 18")))

      # annular microviscosity
      mean_sa = sum(dataframe[sport_id, 34:39])/length(sport_id)
      mean_ca = sum(dataframe[control_id, 34:39])/length(control_id)
      # general microviscosity
      mean_sg = sum(dataframe[sport_id, 40:45])/length(sport_id)
      mean_cg = sum(dataframe[control_id, 40:45])/length(control_id)
      
      # target id
      
      mean_ta = sum(dataframe[target_id, 34:39])
      mean_tg = sum(dataframe[target_id, 40:45])
      
      # cat(mean_as, mean_ac, mean_sg, mean_cg, "\n")
      q = paste("SELECT name, data_db, sport, height, weight, qual FROM private WHERE id = ", target_id, sep = "")
      print(spc_view(q))
      cat("Differencies in annular lipid found: ", mean_ta/mean_ca, "\n")
      cat("Differencies in annular general found: ", mean_ta/mean_cg, "\n", "\n")
      }

spc_mlr = function(){
  dat1 = spc_view("SELECT * FROM private WHERE (id <>38 AND id<>45 AND id<>46 AND id<>47)")
  dat0 = spc_view("SELECT * FROM rbc WHERE (id <>38 AND id<>45 AND id<>46 AND id<>47)")
#  lm(dat1$qual ~ dat0$I0 + dat0$Imp1+dat0$Imp2+dat0$Imp4+dat0$Imp6+dat0$Imp8+dat0$Imp10+dat0$Imon1_286+dat0$Imon2_286+dat0$Imon4_286+dat0$Imon6_286+dat0$Imon8_286+dat0$Imon10_286+dat0$Imon1_337+dat0$Imon2_337+dat0$Imon4_337+dat0$Imon6_337+dat0$Imon8_337+dat0$Imon10_337+dat0$Ie1_286+dat0$Ie2_286+dat0$Ie4_286+dat0$Ie6_286+dat0$Ie8_286+dat0$Ie10_286+dat0$Ie1_337+dat0$Ie2_337+dat0$Ie4_337+dat0$Ie6_337+dat0$Ie8_337+dat0$Ie10_337+dat0$N1_286+dat0$N2_286+dat0$N4_286+dat0$N6_286+dat0$N8_286+dat0$N10_286+dat0$N1_337+dat0$N2_337+dat0$N4_337+dat0$N6_337+dat0$N8_337+dat0$N10_337+dat0$I0Imp1+dat0$I0Imp2+dat0$I0Imp4+dat0$I0Imp6+dat0$I0Imp8+dat0$I0Imp10+dat0$I0I0Imp1+dat0$I0I0Imp2+dat0$I0I0Imp4+dat0$I0I0Imp4+dat0$I0I0Imp8+dat0$I0I0Imp10+dat0$tetha+dat0$F1F0+dat0$Polarity1_286+dat0$Polarity2_286+dat0$Polarity4_286+dat0$Polarity6_286+dat0$Polarity8_286+dat0$Polarity10_286+dat0$Polarity1_337+dat0$Polarity2_337+dat0$Polarity4_337+dat0$Polarity6_337+dat0$Polarity8_337+dat0$Polarity10_337+dat1$old+dat1$sex+dat1$height+dat1$weight+dat1$exp)->llmm; summary(llmm)
   lm(dat1$qual ~ dat0$I0 + dat0$Imp1+dat0$Imp2+dat0$Imp4+dat0$Imp6+dat0$Imp8+dat0$Imp10+dat0$Imon1_286+dat0$Imon2_286+dat0$Imon4_286+dat0$Imon6_286+dat0$Imon8_286+dat0$Imon10_286+dat0$Imon1_337+dat0$Imon2_337+dat0$Imon4_337+dat0$Imon6_337+dat0$Imon8_337+dat0$Imon10_337+dat0$Ie1_286+dat0$Ie2_286+dat0$Ie4_286+dat0$Ie6_286+dat0$Ie8_286+dat0$Ie10_286+dat0$Ie1_337+dat0$Ie2_337+dat0$Ie4_337+dat0$Ie6_337+dat0$Ie8_337+dat0$Ie10_337+dat0$N1_286+dat0$N2_286+dat0$N4_286+dat0$N6_286+dat0$N8_286+dat0$N10_286+dat0$N1_337+dat0$N2_337+dat0$N4_337+dat0$N6_337+dat0$N8_337+dat0$N10_337+dat0$I0Imp1+dat0$I0Imp2+dat0$I0Imp4+dat0$I0Imp6+dat0$I0Imp8+dat0$I0Imp10+dat0$I0I0Imp1+dat0$I0I0Imp2+dat0$I0I0Imp4+dat0$I0I0Imp4+dat0$I0I0Imp8+dat0$I0I0Imp10+dat0$tetha+dat0$F1F0+dat0$Polarity1_286+dat0$Polarity2_286+dat0$Polarity4_286+dat0$Polarity6_286+dat0$Polarity8_286+dat0$Polarity10_286+dat0$Polarity1_337+dat0$Polarity2_337+dat0$Polarity4_337+dat0$Polarity6_337+dat0$Polarity8_337+dat0$Polarity10_337+dat1$old+dat1$sex+dat1$height+dat1$weight+dat1$exp)->llmm; summary(llmm)
  
  #plot(llmm)
  }






