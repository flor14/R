#install.packages(c("gdata","lubridate","data.table"))
library("data.table")
library("lubridate")
library("gdata")

setwd("C:/Users/User/Dropbox/RUNP1/R_Hidrologic/")
Parana_SMN<-fread("Parana_SMN_datasetFINAL.txt", fill=TRUE, sep = " ")
colnames(Parana_SMN)<-c("Date", "TMAX", "TMIN", "RH", "WS10", "HELIO", "GR", "ID", "Lat", "Long", "Julian", "PM", "PM_Mes_Rad", "Hargreaves", "Month", "Year", "Yearmon", "Yearmon2", "PRECIP", "Epan" )





Date <-as.Date(Parana_SMN$Date)
v10 <-as.numeric(as.character(Parana_SMN$WS10))
Tmax <- as.numeric(as.character(Parana_SMN$TMAX))
Tmin <-as.numeric(as.character(Parana_SMN$TMIN))
PM <- as.numeric(as.character(Parana_SMN$PM))
SolR <- as.numeric(as.character(Parana_SMN$GR))
PP <- as.numeric(as.character(Parana_SMN$PRECIP))

RangoT <- data.frame(Tmax, Tmin)

#PP (lo tengo en mm, lo quiero en cm)
PPcm<- PP/10
#EVAPO ((1) lo tengo en PM, lo quiero en Epan, (2) lo tengo en mm, lo quiero en cm)
Epan <- PM/0.7
Epancm <- Epan/10
#Tmed
Tmed <- (rowMeans(RangoT,na.rm = TRUE))
#Viento (lo tengo en km/h, lo quiero en cm/s)
v10cms<- (v10*100000)/(60*60)
#SolR (la tengo en MJ/m2, la quiero en Langley)
#1MJ son 1000000 de Joules/1 caloria son 4.1868 joules/1 m2 son 10000 cm2
SolRlang <- (SolR*1000000)/(10000*4.1868)

Date<-as.character(format(Date, "%m-%d-%y"))
temp  <- strsplit(Date, "-")
tempo<-matrix(unlist(temp), ncol=3, byrow=TRUE)

#Fecha
#Date<-as.character(format(Date, "%m%d%y"))
#Genero el weather file
WF<- data.frame(tempo,PPcm,Epancm,Tmed,v10cms,SolRlang)
WF$empty<-NA
WF<-WF[c("empty","X1", "X2","X3", "PPcm","Epancm","Tmed","v10cms","SolRlang")]
WF$X3<-year(seq(as.Date("61/01/01"), by = "day", length.out = 11323))

#ATENCION! ESTO SOLO PARA PRUEBA/ saco NA
#WF<- na.omit(WF)

#completo huecos con promedio porque este es un ensayo bastante avanzado
WF$PPcm[is.na(WF$PPcm)] <- 0 
WF$Epancm[is.na(WF$Epancm)] <- mean(WF$Epancm, na.rm = TRUE) 
WF$Tmed[is.na(WF$Tmed)] <- mean(WF$Tmed, na.rm = TRUE) 
WF$v10cms[is.na(WF$v10cms)] <- mean(WF$v10cms, na.rm = TRUE) 
WF$SolRlang[is.na(WF$SolRlang)] <- mean(WF$SolRlang, na.rm = TRUE) 

WF$PPcm<-round(WF$PPcm, digit=2)
WF$Epancm<-round(WF$Epancm, digit=2)
WF$Tmed<-round(WF$Tmed, digit=1)
WF$v10cms<-round(WF$v10cms, digit=1)
WF$SolRlang<-round(WF$SolRlang, digit=1)

#1X,3I2,5F10.0

WF$X1<-as.integer(as.character(WF$X1))
WF$X2<-as.integer(as.character(WF$X2))
WF$X3<-as.integer(as.character(WF$X3))
WF$X1<-sprintf("%02d", WF$X1)#leading zeros
WF$X2<-sprintf("%02d", WF$X2)
WF$PPcm<-as.numeric(WF$PPcm)
WF$Epancm<-as.numeric(WF$Epancm)
WF$Tmed<-as.numeric(WF$Tmed)
WF$v10cms<-as.numeric(WF$v10cms)
WF$SolRlang<-as.numeric(WF$SolRlang)

write.fwf(WF,file="C:/Users/User/Desktop/p2/WF_Parana_SMN.dvf", width=c(1,2,2,2,10,10,10,10,10), sep ="" , colnames = FALSE)
