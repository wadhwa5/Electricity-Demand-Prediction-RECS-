#---------------------------------Functions-------------------------------------
# i have made some fucntions which are used again and again in this program
# First function is RMSE function in which we have 3 input parameter
# i.e. model type(obj), dataset , and our response variable

RMSE_calc <- function(obj,newData, response){
        #rmse for regression
        if(class(newData[,response])!="factor"){
                predicted_val <- predict(obj, newData[,-which(names(newData) %in% response)])
                RMSE <-mean((newData[,response] - predicted_val)^2)^0.5
        }else{ #misclassification error for classification
                predicted_val <- predict(obj, newData[,-which(names(newData) %in% response)],type = "class")
                table_test <- table(predicted_val, newData[,response])
                #in this case RMSE is the misclassification error
                RMSE <- 1 - sum(diag(table_test))/sum(table_test)
        }
        return(RMSE)
}

# Second function is R-sq function in which input parameter are dataset, model and response
R2 <- function(dataset,response,model) {
        pred=predict(model,newdata = dataset)
        SSE<-sum((pred-dataset[,response])^2)
        SST<-sum((dataset[,response]-rep(mean(dataset[,response]),nrow(dataset)))^2)
        Rsq=1-(SSE/SST)
        return(Rsq)
}

# third is final function which return model parameters using above 2 functions
# Model Stat return us RMSE of train, RMSE of test and R-sq value 
Model_stat<- function(obj,response,modelname,traindata=train,testdata=test){
        RMSE_train<-RMSE_calc(obj,traindata,response)
        RMSE_test<-RMSE_calc(obj,testdata,response)
        Rsq<-R2(traindata,response,model = obj)
        out<-c(modelname,RMSE_train,RMSE_test,Rsq)
        return(out)
}





#--------------------------------- Library used --------------------------------
# first i am checking if the library is installed on the computer on which we
# are running this program, if not it will first install and later it will call that 
# library.
if (!("ggplot" %in% names(installed.packages()[,"Package"]))) {install.packages("ggplot")}
suppressMessages(library(ggplot, quietly = TRUE))
if (!("Amelia" %in% names(installed.packages()[,"Package"]))) {install.packages("Amelia")}
suppressMessages(library(Amelia, quietly = TRUE))
if (!("caTools" %in% names(installed.packages()[,"Package"]))) {install.packages("caTools")}
suppressMessages(library(caTools, quietly = TRUE))
if (!("caret" %in% names(installed.packages()[,"Package"]))) {install.packages("caret")}
suppressMessages(library(caret, quietly = TRUE))
if (!("dplyr" %in% names(installed.packages()[,"Package"]))) {install.packages("dplyr")}
suppressMessages(library(dplyr, quietly = TRUE))
if (!("corrplot" %in% names(installed.packages()[,"Package"]))) {install.packages("corrplot")}
suppressMessages(library(corrplot, quietly = TRUE))
if (!("glmnet" %in% names(installed.packages()[,"Package"]))) {install.packages("glmnet")}
suppressMessages(library(glmnet, quietly = TRUE))
if (!("mgcv" %in% names(installed.packages()[,"Package"]))) {install.packages("mgcv")}
suppressMessages(library(mgcv, quietly = TRUE))
if (!("partykit" %in% names(installed.packages()[,"Package"]))) {install.packages("partykit")}
suppressMessages(library(partykit, quietly = TRUE))
if (!("randomForest" %in% names(installed.packages()[,"Package"]))) {install.packages("randomForest")}
suppressMessages(library(randomForest, quietly = TRUE))
if (!("rpart" %in% names(installed.packages()[,"Package"]))) {install.packages("rpart")}
suppressMessages(library(rpart, quietly = TRUE))
if (!("ipred" %in% names(installed.packages()[,"Package"]))) {install.packages("ipred")}
suppressMessages(library(ipred, quietly = TRUE))
if (!("party" %in% names(installed.packages()[,"Package"]))) {install.packages("party")}
suppressMessages(library(party, quietly = TRUE))
if (!("cvTools" %in% names(installed.packages()[,"Package"]))) {install.packages("cvTools")}
suppressMessages(library(cvTools, quietly = TRUE))
if (!("earth" %in% names(installed.packages()[,"Package"]))) {install.packages("earth")}
suppressMessages(library(earth, quietly = TRUE))

library(bartMachine, quietly = TRUE)
library(e1071, quietly = TRUE)
library(ggRandomForests, quietly = TRUE)

#------------------------- Main Program ----------------------------------------
#-----------------------EDAand Feature Engineering------------------------------
# Setting working Directory
setwd("~/Desktop/Spring17/590-risk/Final") 
list.files()


# we are reading the csv file which I have downloaded from https://www.eia.gov/consumption/residential/data/2005/
full2005<-read.csv("RECS05alldata.csv")

# Subsetting the data for Northeast (Regionc = 1) in 2005 data 
mydata<-subset(full2005, REGIONC == "1" )
mydata$AGEHHMEM1 [mydata$AGEHHMEM1==99]<-0
mydata$AGEHHMEM2 [mydata$AGEHHMEM2==99]<-0
mydata$AGEHHMEM3 [mydata$AGEHHMEM3==99]<-0
mydata$AGEHHMEM4 [mydata$AGEHHMEM4==99]<-0
mydata$AGEHHMEM5 [mydata$AGEHHMEM5==99]<-0
mydata$AGEHHMEM6 [mydata$AGEHHMEM6==99]<-0
mydata$AGEHHMEM7 [mydata$AGEHHMEM7==99]<-0
mydata$AGEHHMEM8 [mydata$AGEHHMEM8==99]<-0
mydata$AGEHHMEM9 [mydata$AGEHHMEM9==99]<-0
mydata$AGEHHMEM10 [mydata$AGEHHMEM10==99]<-0
mydata$AGEHHMEM11 [mydata$AGEHHMEM11==99]<-0
mydata$AGEHHMEM12 [mydata$AGEHHMEM12==99]<-0
mydata$AGEHHMEMY [mydata$AGEHHMEMY==99]<-0
mydata$NUMPC [mydata$NUMPC==99]<-0
mydata$AMTMICRO [mydata$AMTMICRO==9]<-0
mydata$DISHWASH [mydata$DISHWASH==99]<-0
mydata$WASHLOAD [mydata$WASHLOAD==9]<-0
mydata$DRYRUSE [mydata$DRYRUSE==9]<-0
mydata$SWIMPOOL [mydata$SWIMPOOL==9]<-0
mydata$EQUIPM [mydata$SWIMPOOL==99]<-NA
mydata$EQUIPM [mydata$SWIMPOOL==21]<-NA

mydata$CHILDC<-0
mydata$ADULTC<-0
mydata$TEENC<-0
mydata$OLDC<-0
for (i in 1:length(mydata[,1])){
        #counting number of children
        if(mydata$AGEHHMEM1[i]>0 & mydata$AGEHHMEM1[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM2[i]>0 & mydata$AGEHHMEM2[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM3[i]>0 & mydata$AGEHHMEM3[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM4[i]>0 & mydata$AGEHHMEM4[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM5[i]>0 & mydata$AGEHHMEM5[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM6[i]>0 & mydata$AGEHHMEM6[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM7[i]>0 & mydata$AGEHHMEM7[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        if(mydata$AGEHHMEM8[i]>0 & mydata$AGEHHMEM8[i]<13){ mydata$CHILDC[i]<-mydata$CHILDC[i]+1}
        # counting number of teen
        if(mydata$AGEHHMEM1[i]>12 & mydata$AGEHHMEM1[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM2[i]>12 & mydata$AGEHHMEM2[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM3[i]>12 & mydata$AGEHHMEM3[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM4[i]>12 & mydata$AGEHHMEM4[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM5[i]>12 & mydata$AGEHHMEM5[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM6[i]>12 & mydata$AGEHHMEM6[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM7[i]>12 & mydata$AGEHHMEM7[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        if(mydata$AGEHHMEM8[i]>12 & mydata$AGEHHMEM8[i]<20){ mydata$TEENC[i]<-mydata$TEENC[i]+1}
        #counting number of adult
        if(mydata$AGEHHMEM1[i]>19 & mydata$AGEHHMEM1[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM2[i]>19 & mydata$AGEHHMEM2[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM3[i]>19 & mydata$AGEHHMEM3[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM4[i]>19 & mydata$AGEHHMEM4[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM5[i]>19 & mydata$AGEHHMEM5[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM6[i]>19 & mydata$AGEHHMEM6[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM7[i]>19 & mydata$AGEHHMEM7[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        if(mydata$AGEHHMEM8[i]>19 & mydata$AGEHHMEM8[i]<66){ mydata$ADULTC[i]<-mydata$ADULTC[i]+1}
        #counting old person (above 65)
        if(mydata$AGEHHMEM1[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM2[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM3[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM4[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM5[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM6[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM7[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
        if(mydata$AGEHHMEM8[i]>65 ){ mydata$OLDC[i]<-mydata$OLDC[i]+1}
}


#checking missing data
x<-sapply(mydata, function(x) {sum(is.na(x))}) 
# checking which predictor has missing value and how many.
for(i in 1:length(x)){
        if (x[i]!=0){
                print(paste(colnames(mydata)[i],x[i]))
        }
}



#we found 3 columns have missing value ORIGELS 13, KAVALNG 3 and ORIGNGQ 3.
# on reading about these value I found that KAVALNG are those who do not have Natural Gas connection
# Now we started variable selection
# After Reading the research paper and having background knowledge about how electricity is consumed
# and which factors can effect its consumption we step by step reduced the predictors in 3 itterations
# I also kept checking the varImp plot so that our top predictors stays in our model.

trimdata<-mydata %>% select(DIVISION,WALLTYPE,TOTROOMS,NCOMBATH,NHAFBATH,CELLAR,BASEHEAT,BASECOOL,ATTCHEAT,ATTCCOOL,GARGHEAT,GARGCOOL,KOWNRENT,HUPROJ,RENTHELP,KOWNCOND,YEARMADE,OCCUPYY,URBRUR,UGASHERE,WASHLOAD,DRYRFUEL,DRYRUSE,WATERBED,NOWTBDHT,WTBEDUSE,NUMCFAN,USECFAN,SWIMPOOL,POOL,FUELPOOL,RECBATH,FUELTUB,H2OLAWNSYS,USEH2OLAWNSYS,TVCOLOR,TVONWD,TVONWE,USENOTMOIST,USEMOISTURE,WELLPUMP,SWAMPCOL,AQUARIUM,DIPSTICK,NUMPC,TIMEON1,TIMEON2,TIMEON3,HOWPCUSED,TELLDAYS,COPIER,DNTHEAT,EQUIPAGE,HEATOTH,EQUIPAUX,REVERSE,WARMAIR,STEAMR,PERMELEC,PIPELESS,ROOMHEAT,WOODKILN,CARRYEL,CARRYKER,CHIMNEY,RANGE,DIFEQUIP,DKEQUIP,ELECAUX,UGASAUX,LPGAUX,FOILAUX,KEROAUX,WOODAUX,SOLARAUX,OTHERAUX,DKAUX,FURNFUEL,RADFUEL,DIFFUEL,PIPEFUEL,RMHTFUEL,HSFUEL,FPFUEL,NGFPFLUE,USENGFP,RNGFUEL,EQMAMT,THERMAIN,NUMTHERM,PROTHERM,AUTOHEATNITE,AUTOHEATDAY,TEMPHOME,TEMPGONE,TEMPNITE,HEATOFF,HEATROOM,HEATNOT,OTHNOHT,FUELH2O,FUELH2O2,USEEL,USENG,USELP,USEFO,USEKERO,USEWOOD,USESOLAR,USEOTH,ELWARM,ELFOOD,ELWATER,ELCOOL,ELOTHER,UGWARM,UGWATER,UGCOOK,UGOTH,LPWARM,LPWATER,LPCOOK,LPGRILL,LPOTHER,FOWARM,FOWATER,FOOTHER,KRWARM,KRWATER,KROTHER,WDWARM,WDWATER,WDOTHER,SOLWARM,SOLWATER,SOLPOOL,SOLOTHER,HOWPAID,HOWPAYEL,HOWPAYNG,HOWPAYLP,HOWPAYFO,HOWPAYKR,LPGDELV,QUANTFO,SCALEH,SCALEI,ENERGYAID,AIDADDRESS,NOPYELAC,NOPYELACREST,SOMEPY,NDAYSWOEL,SQFTEST,MEASURE,TOTSQFT,TOTBASESQFT,TOTATTCSQFT,TOTGARGSQFT,TOTRHMSQFT,TOTHSQFT,TOTUSQFT,BASHSQFT,BASUSQFT,GARHSQFT,GARUSQFT,ATTHSQFT,ATTUSQFT,RHMHSQFT,RHMUSQFT,TOTCSQFT,TOTUCSQFT,BASCSQFT,BASUCSQFT,GARCSQFT,GARUCSQFT,ATTCSQFT,ATTUCSQFT,RHMCSQFT,RHMUCSQFT,KAVALNG,KAVALFO,KAVALLPG,KAVALKER,BTUEL,BTUNG,BTUFO,BTULP,BTUKER,BTUELCDR,BTUELDWH,BTUELFZZ,BTUELRFG,BTUELCOL,BTUELWTH,BTUELSPH,BTUELAPL,BTUNGAPL,BTUNGWTH,BTUNGSPH,BTUFOAPL,BTUFOSPH,BTUFOWTH,BTULPAPL,BTULPSPH,BTULPWTH,BTUWOOD)

trimdata2<-mydata %>% select(TYPEHUQ,HD65,CD65,WALLTYPE,TOTROOMS,YEARMADE,TSTRFRQ,TSTRUSE,COFPOTON,NUMFRIG,NUMFREEZ,WASHLOAD,NOWTBDHT,WTBEDUSE,SWIMPOOL,FUELPOOL,RECBATH,FUELTUB,OVENUSE,TVCOLOR,BIGTV,PLASMANUM,VCR,DVD,PLYSTAHRS,TVONWD,TVONWE,USEMOISTURE,WELLPUMP,SWAMPCOL,AQUARIUM,BATTOOLS,NUMPC,NUMTHERM,AUTOHEATNITE,AUTOHEATDAY,TEMPHOME,TEMPGONE,TEMPNITE,EQUIPM,WHEATSIZ,COOLTYPE,ACROOMS,CENACHP,THERMAINAC,AUTOCOOLNITE,AUTOCOOLDAY,TEMPHOMEAC,TEMPGONEAC,TEMPNITEAC,LGT12,LGT12EE,LGT4,LGT4EE,LGT1,LGT1EE,NOUTLGTNT,NGASLIGHT,WINDOWS,TYPEGLASS,HOWPAYEL,KFUELOT,FARM,TENANT,BUSINESS,OTHERUSE,BILLELP,ATHOME,HHINTRO,HHAGE,EMPLOYHH,SPOUSE,NHSLDMEM,AGEHHMEM1,AGEHHMEMY,MONEYPY,HHINCOME,LIHEAP,ENERGYAID,AIDADDRESS,NOPYFIX,NOPYFIXREST,NDAYSWOEL,SQFTEST,MEASURE,TOTSQFT,TOTHSQFT,TOTCSQFT,KAVALEL,KWH)
# Finally we reduced our model to 66 predictor and one response variable i.e. KWH
#trimdata3<-mydata %>% select(TYPEHUQ,HD65,CD65,WALLTYPE,TOTROOMS,YEARMADE,TSTRFRQ,TSTRUSE,COFPOTON,NUMFRIG,NUMFREEZ,WASHLOAD,NOWTBDHT,WTBEDUSE,SWIMPOOL,FUELPOOL,RECBATH,FUELTUB,TVCOLOR,TVCOLOR,VCR,DVD,TVONWD,TVONWE,USEMOISTURE,NUMPC,TEMPHOME,TEMPGONE,TEMPNITE,EQUIPM,WHEATSIZ,COOLTYPE,ACROOMS,AUTOCOOLNITE,AUTOCOOLDAY,TEMPHOMEAC,TEMPGONEAC,TEMPNITEAC,LGT12,LGT12EE,LGT4,LGT4EE,LGT1,LGT1EE,NOUTLGTNT,NGASLIGHT,WINDOWS,TYPEGLASS,HOWPAYEL,KFUELOT,HHAGE,EMPLOYHH,SPOUSE,NHSLDMEM,AGEHHMEM1,AGEHHMEMY,HHINCOME,LIHEAP,ENERGYAID,AIDADDRESS,NOPYFIX,NOPYFIXREST,TOTSQFT,TOTHSQFT,TOTCSQFT,KAVALEL,OVENUSE,KWH)

trimdata3<-mydata %>% select(NHSLDMEM,CHILDC,TEENC,ADULTC,OLDC,AGEHHMEM1,EMPLOYHH,KOWNRENT,HHINCOME,HOWPAYEL,AGEHHMEMY,NUMPC,TVCOLOR,TVONWD,TVONWE,VCR,DVD,NUMCFAN,USECFAN,USENOTMOIST,CARRYEL,OVENUSE,AMTMICRO,TSTRFRQ,NUMFRIG,NUMFREEZ,DISHWASH,DWASHUSE,CWASHER,WASHLOAD,DRYER,DRYRUSE,SWIMPOOL,POOL,FUELPOOL,RECBATH,FUELTUB,TYPEHUQ,YEARMADE,TOTROOMS,BEDROOMS,NAPTFLRS,TOTSQFT,EQUIPM,LGT12,LGT12EE,LGT4,LGT4EE,LGT1,LGT1EE,NOUTLGTNT,NGASLIGHT,TOTHSQFT,TOTCSQFT,URBRUR,HD65,CD65,TEMPHOME,TEMPGONE,TEMPNITE,WINDOWS,TYPEGLASS,SPOUSE,ENERGYAID,AIDADDRESS,NOPYFIX,NOPYFIXREST,WALLTYPE,HHAGE,KWH)
# changing numeric data to factor according to code book
for(i in c(7,8,10,14,15,20,22,23,25,26,27,28,29,30,31,32,33,38,39,44,55,61,62,63,68)){
        
                trimdata3[,i]<-as.factor(trimdata3[,i])    
        }

trimdata3<-trimdata3[,-c(19,21,24,34,35,36,37,42,51,52,64,65,66,67)]
trimdata3$OVENUSE[trimdata3$OVENUSE==9]<-NA

trimdata3$TOTHSQFT<-(trimdata3$TOTHSQFT-mean(trimdata3$TOTHSQFT))/sd(trimdata3$TOTHSQFT)
trimdata3$TOTCSQFT<-(trimdata3$TOTCSQFT-mean(trimdata3$TOTCSQFT))/sd(trimdata3$TOTCSQFT)
trimdata3$TOTSQFT<-(trimdata3$TOTSQFT-mean(trimdata3$TOTSQFT))/sd(trimdata3$TOTSQFT)

tri<-sapply(trimdata3, function(x) {sum(is.na(x))}) 
# checking which predictor has missing value and how many.
for(i in 1:length(tri)){
        if (tri[i]!=0){
                print(paste(colnames(trimdata3)[i],tri[i]))
        }
}

trimdata3<-trimdata3[complete.cases(trimdata3),]

#----------------EDA------------------------------------------------------------
yearmade<-as.data.frame(table(mydata$YEARMADE))
rownames(yearmade)<-(c("BEFORE 1940","1940 - 1949","1950 - 1959","1960 - 1969","1970 - 1979","1980-1984","1985-1989","1990-1994","1995- 1999","2000-2002","2003","2004","2005"))
yearmade$perhousekwh<-0
for (i in 1:13){
        yearmade$perhousekwh[i]<-(sum(mydata$KWH[mydata$YEARMADE==i])/yearmade$Freq[i])
}

op <- par(mar=c(11,4,4,2))
bp<-barplot(yearmade$Freq,yearmade$perhousekwh, main="House distribution by year",las=2, col="skyblue",
            names.arg = c("BEFORE 1940","1940 - 1949","1950 - 1959","1960 - 1969","1970 - 1979","1980-1984","1985-1989","1990-1994","1995- 1999","2000-2002","2003","2004","2005"))
text(bp, 0, round(yearmade$Freq, 1),cex=1,pos=3)
bp2<-barplot(yearmade$perhousekwh, main="Avg Electricity Usage vs built year of houses",las=2, col="skyblue",ylab="Electricity Usage (in kWHr) ",
             names.arg = c("BEFORE 1940","1940 - 1949","1950 - 1959","1960 - 1969","1970 - 1979","1980-1984","1985-1989","1990-1994","1995- 1999","2000-2002","2003","2004","2005"))
text(bp2, 0, round(yearmade$perhousekwh, 1),cex=1,pos=3)

rm(op)
par()


#Voilon plot of heating days and cooling days
plot(1, 1, xlim = c(0, 4), ylim = range(c(trimdata3$HD65, trimdata3$CD65)), type = 'n', xlab = '', ylab = '', xaxt = 'n')
vioplot(trimdata3$HD65, at = 1, add = T, col = 'green')
vioplot(trimdata3$CD65, at = 3, add = T, col = 'magenta')
axis(1, at = c(1,3), labels = c('Heating Days', 'Cooling Days'))
title(main = 'Violin Plots of Heating Days and Cooling Days')


#Voilon plot of temphome,tempgone and tempnite
plot(1, 1, xlim = c(0, 4), ylim = range(c(trimdata3$TEMPHOME, trimdata3$TEMPGONE,trimdata3$TEMPNITE,trimdata3$TEMPHOMEAC,trimdata3$TEMPGONEAC,trimdata3$TEMPNITEAC)), type = 'n', xlab = '', ylab = '', xaxt = 'n')
vioplot(trimdata3$TEMPHOME, at = 1, add = T, col = 'green')
vioplot(trimdata3$TEMPGONE, at = 2, add = T, col = 'magenta')
vioplot(trimdata3$TEMPNITE, at = 3, add = T, col = 'blue')
axis(1, at = c(1,2,3), labels = c('TEMPHOME', 'TEMPGONE','TEMPNITE'))
title(main = 'Violin Plots of Temperature Setting of Home for heating')

plot(1, 1, xlim = c(0, 4), ylim = range(c(trimdata3$TEMPHOME, trimdata3$TEMPGONE,trimdata3$TEMPNITE,trimdata3$TEMPHOMEAC,trimdata3$TEMPGONEAC,trimdata3$TEMPNITEAC)), type = 'n', xlab = '', ylab = '', xaxt = 'n')
vioplot(trimdata3$TEMPHOMEAC, at = 1, add = T, col = 'yellow')
vioplot(trimdata3$TEMPGONEAC, at = 2, add = T, col = 'red')
vioplot(trimdata3$TEMPNITEAC, at = 3, add = T, col = 'orange')
axis(1, at = c(1,2,3), labels = c('TEMPHOMEAC', 'TEMPGONEAC','TEMPNITEAC'))
title(main = 'Violin Plots of Temperature Setting of Home for cooling')

# Number of TV graph
tv <- table(trimdata3$TVCOLOR)
barplot(tv, main="Number of TV in house distribution", 
        xlab="Number of TV",col = 'yellow',ylab = 'Frequency')
text(bp2, 0, round(tv, 1),cex=1,pos=3)

hist(trimdata3$HHINCOME,col = 'green',main=' Household income', xlab='Income (in $)',ylab='Frequency' )
hhi <- table(trimdata3$HHINCOME)

# distribution of house by locality
ho <- table(trimdata3$URBRUR)
barplot(ho, main="Distribution of House by Neighborhood", 
        xlab="Type of Neighbourhood",col = 'orange',ylab = 'Frequency',names.arg = c("City","Town","Suburbs","Rural"))
text(bp2, 0, round(ho, 1),cex=1,pos=3)



#Violinplot of KWH
vioplot(mydata$KWH, col='Red',names = 'Total Site Electricity Usage (in kWh)')#,main="Distribution of Total Site Electricity Usage")
d<- density(trimdata3$KWH)
plot(d, main="Kernel Density of Total Site Electric Usage ", xlab = 'Electricity Usage (in kWh)')
polygon(d, col="red", border="blue")




#---------------------------Splitting data--------------------------------------
#splitting the data in test and train in test:train::15:85 and I have removed first 3 columns
# as they are just name of county and state 
set.seed(108)
data<-trimdata3
sample = sample.split(data, SplitRatio = .80)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

#-----------------------------Regression Model----------------------------------

Y=as.character("KWH") # saving our response variable in Y

# checking the normality assumption of Y 
qqline(train$KWH ,main = "Normal Q-Q Plot of KWH ") # QQ-plot for checking the Normality


# stating with most basic model, MLR
regression_model<-lm(KWH~.,data = train)
summary(regression_model)
#saving the output parameter in varibale so that in end we can compare easily
Regression<-Model_stat(regression_model,response ="KWH",modelname = "Regression" )

#running stepwise model to get the improtatnt predictior 
reg_step <- step(regression_model, direction="both") 
anova(reg_step)
summary(regression_model2)
#saving the output parameter in varibale so that in end we can compare easily
Regression_stepwise<-Model_stat(reg_step,response ="KWH",modelname = "Regression_Stepwise" )

#--------------------------------MARS-------------------------------------------

mars_mod<-earth(KWH~.,data=train,pmethod = "cv",nfold=10)
summary(mars_mod)
mars_mod1<-earth(KWH~.,data=train,pmethod = "cv",nfold=10,degree = 2)
summary(mars_mod1)
mars_mod1<-earth(KWH~.,data=train,pmethod = "cv",nfold=10,degree = 2,nk=10)
summary(mars_mod1)

#saving the output parameter in varibale so that in end we can compare easily
MARS_model<-Model_stat(mars_mod,response = "KWH",modelname = "MARS Model")

#-----------------------CART----------------------------------------------------
cartmodel <- rpart(KWH~., data = train)
summary(cartmodel)
plotcp(cartmodel)
printcp(cartmodel)
#saving the best CP
bestcp<-cartmodel$cptable[which.min(cartmodel$cptable[,"xerror"]),"CP"] 
prune_cart_model<-rpart(KWH~., data = train,cp=bestcp)
summary(prune_cart_model)
part.plot(prune_cart_model, tp_args = list(id = FALSE), cex = 2)

#saving the output parameter in varibale so that in end we can compare easily
CART_Model<-Model_stat(prune_cart_model,response = "KWH",modelname = "CART_Model")

#--------------------------------------Bagged Tree -----------------------------
bagtreemodel <- bagging(KWH~., data = train)
summary(bagtreemodel)
bag.error<- data.frame()
cv_bag <- function(dataset,k = 10,ULB=10,LLB=1){ # ULB &LLB upper limit & lowerlimit of Bag in loop 
        for(c in LLB:ULB){
                
                folds <- cvFolds(NROW(dataset), K=k)
                dataset$pred <- rep(0,nrow(dataset))
                error <- vector()
                for(i in 1:k){
                        train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
                        validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
                        
                        new_bag <- bagging(KWH~., data = train, nbagg = c) 
                        newpred <- predict(new_bag,newdata=validation) 
                        
                        dataset[folds$subsets[folds$which == i], ]$pred <- newpred 
                        error[i] <- RMSE_calc(new_bag,train, "KWH")
                }
                bag.error[c,1] <- mean(error)
        }
        return(bag.error)
}
bagout<-cv_bag(train,k=10,ULB=100)
optbag<-which.min(bagout$V1) # optimum bag size I got is 73

prune_bag <- bagging(KWH~., data = train, nbagg = 73, coob = T)
summary(prune_bag)

#saving the output parameter in varibale so that in end we can compare easily
Bag_model<-Model_stat(prune_bag,response = "KWH",modelname = "BAG Tree")
#-------------------------------- Random Forest -----------------------------------
rf <- randomForest(KWH~., data = train, importance = T, ntree = 2000, oob.times = T)
summary(rf)
plot(rf)
rf2 <- randomForest(KWH~., data = train, importance = T, ntree = 250, oob.times = T)
plot(rf2)
rf3 <- randomForest(KWH~., data = train, importance = T, ntree = 200, oob.times = T)
plot(rf3, main=" Errro vs number to trees")
rf.error<- data.frame()
#made the cross validation function which take input from user how many folds he want 
# and from which range of tree he wants to check the models using cv 
cv_rf <- function(dataset,k = 10,ULT,LLT){
        for(c in LLT:ULT){ #upper limit and lower limits on tree
                
                folds <- cvFolds(NROW(dataset), K=k)
                dataset$pred <- rep(0,nrow(dataset))
                error <- vector()
                for(i in 1:k){
                        train <- dataset[folds$subsets[folds$which != i], ] 
                        validation <- dataset[folds$subsets[folds$which == i], ] 
                        
                        new_rf <- randomForest(KWH~., data = train, ntree = c) 
                        newpred <- predict(new_rf,newdata=validation) 
                        
                        dataset[folds$subsets[folds$which == i], ]$pred <- newpred 
                        error[i] <- RMSE_calc(new_rf,train, "KWH")
                }
                rf.error[c,1] <- mean(error)
                
        }
        return(rf.error)
}
rf.error <- cv_rf(train,10,120,150)
opttree<-which.min(rf.error$V1) #optimum tree size I got is 123

tunedrf<-tune.randomForest(KWH~., data = train, importance = T)
# We choose the size of optimum tree which we got from cross validation 

prune_rf<-randomForest(KWH~., data = train, importance = T, ntree = opttree, proximity = T)
summary(prune_rf)
varImpPlot(prune_rf)

imp<-prune_rf$importance
imp<-imp[order(-imp[,1]),]
names=rownames(imp)[1:15]
par(mfrow=c(1, 1), xpd=NA)


partialPlot(prune_rf,pred.data = train,x.var = TOTHSQFT)
partialPlot(prune_rf,pred.data = train,x.var = NUMFREEZ)
partialPlot(prune_rf,pred.data = train,x.var = TVCOLOR)
partialPlot(prune_rf,pred.data = train,x.var = EQUIPM)
partialPlot(prune_rf,pred.data = train,x.var = TOTCSQFT)
partialPlot(prune_rf,pred.data = train,x.var = TOTROOMS)
partialPlot(prune_rf,pred.data = train,x.var = LGT4)
partialPlot(prune_rf,pred.data = train,x.var = TOTSQFT)
partialPlot(prune_rf,pred.data = train,x.var = DWASHUSE)
partialPlot(prune_rf,pred.data = train,x.var = TYPEHUQ)
partialPlot(prune_rf,pred.data = train,x.var = NHSLDMEM)
partialPlot(prune_rf,pred.data = train,x.var = AGEHHMEMY)
partialPlot(prune_rf,pred.data = train,x.var = NUMPC)
partialPlot(prune_rf,pred.data = train,x.var = AGEHHMEM1)
partialPlot(prune_rf,pred.data = train,x.var = WINDOW)
partialPlot(prune_rf,pred.data = train,x.var = HHAGE)
partialPlot(prune_rf,pred.data = train,x.var = WASHLOAD)
partialPlot(prune_rf,pred.data = train,x.var = DRYER)
partialPlot(prune_rf,pred.data = train,x.var = ADULTC)
partialPlot(prune_rf,pred.data = train,x.var = HHINCOME)





plot(test$KWH,predict(prune_rf,newdata = test))
fittrain<-cbind.data.frame(train$KWH,predict(prune_rf,newdata = train))
colnames(fittrain)<-c("Actual","Fit")
predtest<-cbind.data.frame(test$KWH,predict(prune_rf,newdata = test))
colnames(predtest)<-c("Actual","Prediction")

#saving the output parameter in varibale so that in end we can compare easily
RF_model<-Model_stat(prune_rf,response = "KWH",modelname = "Random Forest")

#plotting predict vs actual & Fit vs actual

#plot Actual vs Fit
fp1 <- ggplot(fittrain, aes(x = Actual)) + geom_point(aes(x = Fit, y = Actual,col = "Fit" ))
fp2<-fp1+labs(x = "Fit", y = "Actual") + ggtitle("Actual vs Fit")+geom_abline(intercept = 0, slope = 1)
fp2+xlim(min(fittrain$Actual), max(fittrain$Actual))+ylim(min(fittrain$Actual), max(fittrain$Actual))

#plot Actual vs Predicted
g <- ggplot(predtest, aes(x = Actual)) + geom_point(aes(x = Prediction, y = Actual,col = "Prediction"))
g1<-g+labs(x = "Predicted", y = "Actual") + ggtitle("Actual vs Predicted")+geom_abline(intercept = 0, slope = 1)
g1+xlim(min(predtest$Actual), max(predtest$Actual))

#-------------BART models-------------------------------------------------------

options(java.parameters = "-Xmx6g")
set_bart_machine_num_cores(4)

bart_model_1<-k_fold_cv(train[,-56],train[,56],k_folds = 10,num_trees = 200,serialize = TRUE)
bart_machine_model_1<-bartMachine(train[,-56],train[,56],num_trees = 50,serialize = TRUE)
bart_machine_test_1<-bart_predict_for_test_data(bart_machine_model_1, test[,-56], test[,56])
BART_model<-c("BART Model",bart_machine_model_1$rmse_train,bart_machine_test_1$rmse,bart_machine_model_1$PseudoRsq)

#--------------------_SVM Model-----------------------------------------------
SVMModel1<-tune.svm(KWH~., data = train)
SVM_model<-Model_stat(SVMModel1$best.model,response = "KWH",modelname = "SVM")
#----------------------------combining and comparing models-----------------------------------


# I have combined all the models into a single dataframe so that it is easy to interpret

combmodel<-cbind(as.data.frame(Bag_model),as.data.frame(CART_Model),as.data.frame(SVM_model),as.data.frame(RF_model),as.data.frame(MARS_model),as.data.frame(BART_model))
combmodel<-t(combmodel)
combmodel<-as.data.frame(combmodel)
colnames(combmodel)<-c("Model Name","RMSE of train data","RMSE of test data", " R-square value")

#-------------------Cross validation error in one loop --------------------------
k = 10
folds <- cvFolds(NROW(data), K=k)
error_rf <- vector()
error_bag<-vector()
error_bart<-vector()
error_svm<-vector()
error_mars<-vector()
error_cart<-vector()
for(i in 1:k){
        train.cv <- data[folds$subsets[folds$which != i], ] 
        validation <- data[folds$subsets[folds$which == i], ] 
        
        #random forest
        cv.rf<-randomForest(KWH~., data = train.cv, importance = T, ntree = opttree, proximity = T)
        pred.rf<-predict(cv.rf,newdata = validation)
        error_rf[i]<-RMSE_calc(cv.rf,validation,"KWH")
        #bart machine
        cv.bart<-bartMachine(train.cv[,-56],train.cv[,56],num_trees = 50,serialize = TRUE)
        pred.bart<-bart_predict_for_test_data(cv.bart, validation[,-56], validation[,56])
        error_bart[i]<-pred.bart$rmse
        #svm
        cv.svm<-svm(KWH~.,data = train.cv, kernel="radial",cost=1,epsilon=0.1,gamma=0.0075)
        error_svm[i]<-RMSE_calc(cv.svm,validation,"KWH")
        #bag tree
        cv.bag<-bagging(KWH~., data = train.cv, nbagg = 73, coob = T)
        error_bag[i]<-RMSE_calc(cv.bag,validation,"KWH")
        #CART
        cv.cart<-rpart(KWH~., data = train.cv,cp=bestcp)
        error_cart[i]<-RMSE_calc(cv.cart,validation,"KWH")
        #MARS
        cv.mars<-earth(KWH~.,data=train.cv)
        error_mars[i]<-RMSE_calc(cv.mars,validation,"KWH")
        
        
}
errordata<-data.frame(error_bag,error_rf,error_mars,error_bart,error_cart,error_svm)




groups = factor(rep(letters[1:10], each = 6))
bartlett.test(errordata, groups)
#---------------------Variable importance plot----------------------------------
rfsrc_model<-rfsrc(KWH~.,data = train,ntree = 123,tree.err = TRUE)
plot(gg_rfsrc(rfsrc_model), alpha=.5) 
plot(gg_vimp(rfsrc_model,nvar = 24),main="Variable Importance plot",xlab="Importance by MSE")
varsel_west <- var.select(rfsrc_model)
gg_md <- gg_minimal_depth(varsel_west)
plot(gg_md)
plot(gg_minimal_vimp(gg_md))
gg_v <- gg_variable(rfsrc_model)
xvar <- gg_md$topvars
plot(gg_v, xvar=xvar)#+labs(y=Y)




#---------------- Final model --------------------------
finalmodel<-randomForest(KWH~., data = train, importance = T, ntree = opttree, proximity = T)
rm(list= ls()[!(ls() %in% c('finalmodel'))])