library("RColorBrewer")
##### MORRIS ######

#elijo con cual plaguicida trabajo
listg <- c("Ag", "Bg", "Jg", "Ng", "Pg")
listp <- c("A2", "B2", "J2", "N2", "P2")
list<-c("A2", "B2", "J2", "N2", "P2","Ag", "Bg", "Jg", "Ng", "Pg")

tab<-c()
for(i in listp){ 
  #Leo bases para graficar
  taba<-read.table(paste("F:/p2_resultados/Morris_final/8Morris/SIMLAB/SIMLABfinales/morris_", i,"", sep=""), skip = 3, fill=TRUE)
  taba$name<-paste(i)
  taba$num<-seq(1:nrow(taba))
  colnames(taba)<-c("var","MUporRO", "SIGMAporRO", "MUporERO", "SIGMAporERO","MUppbRO", "SIGMAppbRO","MUppbERO","SIGMAppbERO","MUave", "SIGMAave","MUpeak", "SIGMApeak","MU4", "SIGMA4","MU60", "SIGMA60", "name", "num")
  tab<-rbind(tab, taba)
  colnames(tab)<-c("var","MUporRO", "SIGMAporRO", "MUporERO", "SIGMAporERO","MUppbRO", "SIGMAppbRO","MUppbERO","SIGMAppbERO","MUave", "SIGMAave","MUpeak", "SIGMApeak","MU4", "SIGMA4","MU60", "SIGMA60", "name", "num")
}

#renombro para poder ordenar mejor en el facet-grid
tab$name[tab$name=="A2"]<-c("2,4-D Anguil")
tab$name[tab$name=="P2"]<-c("2,4-D Pergamino")
tab$name[tab$name=="N2"]<-c("2,4-D Parana")
tab$name[tab$name=="B2"]<-c("2,4-D Tres Arroyos")
tab$name[tab$name=="J2"]<-c("2,4-D Marcos Juarez")
tab$name[tab$name=="Ag"]<-c("Glyphosate Anguil")
tab$name[tab$name=="Pg"]<-c("Glyphosate Pergamino")
tab$name[tab$name=="Ng"]<-c("Glyphosate Parana")
tab$name[tab$name=="Bg"]<-c("Glyphosate Tres Arroyos")
tab$name[tab$name=="Jg"]<-c("Glyphosate Marcos Juarez")


#tab<-tab[order(tab$name)
#setwd("F:/")
#VERSION 1
#tiff("morris-peak.tif", width = 1500, height = 700)
windows(12000,10000)
ggplot(data=tab, aes(x=MU4, y=SIGMA4))+ geom_point(data=subset(tab, MU4<1 ),aes(x=MU4, y=SIGMA4), color="green")+
  geom_point(data=subset(tab, MU4>1 ),aes(x=MU4, y=SIGMA4), color="black", alpha=0.5)+ 
  geom_text_repel(data=subset(tab, MU4>1),aes(x=MU4, y=SIGMA4, label=var), point.padding = 0.1,min.segment.length=0.4,max.iter=5000, segment.color="darkgray", size=3,parse=TRUE)+  
  facet_wrap(~name, scales = "free", nrow = 2)+ theme_light()+
  labs(x= c("Mu 4-d avg"), y=("Sigma 4-d avg"))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
#dev.off()
#nudge_x=0.2,

#windows(150,150)
#ggplot(data=tab, aes(x=MU4, y=SIGMA4))+ geom_point(data=subset(tab, MU4<1 ),aes(x=MU4, y=SIGMA4), color="black")+
#  geom_point(data=subset(tab, MU4>1 ),aes(x=MU4, y=SIGMA4, color=var), size=2)+ 
  #geom_text_repel(data=subset(tab, MU4>1 & SIGMA4>1),aes(x=MU4, y=SIGMA4, label=var), point.padding = 0.20,min.segment.length=1, size=3)+ 
#  facet_wrap(~name, scales = "free", nrow = 2)+ 
#  theme_light()+
#  scale_color_manual(values=getPalette(colourCount), na.value="white")+ 
#  theme(legend.position = c(1, 0),
#        legend.justification = c(1, 0),
#        panel.grid.minor = element_blank(),
 #       panel.grid.major = element_blank())+ guides(fill=guide_legend(ncol=5))

#colourCount <- length(unique(tab$var[tab$MU4>1])) # number of levels
#getPalette <- colorRampPalette(brewer.pal(8, "Paired"))

###### INDICES SOBOL ########

setwd("F:/SIMLAB")
read.delim("F:/p2_resultados/Morris_final/8Morris/SIMLAB/SIMLABfinales/indices_sobol_A2", sep=" ", skip = 2, nrows=22, fill=TRUE)

#corrijo A2 que saque un putput extra
#library(data.table)
#elim<-fread("F:/SIMLAB/indices_sobol_A2_comp", fill=TRUE)
#elim$V9 <-NULL
#write.table(elim, "F:/SIMLAB/indices_sobol_A2", sep=" ")


listg <- c("Ag", "Bg", "Jg", "Ng", "Pg")
listp <- c("A2", "B2", "J2", "N2", "P2")

#### LECTURA DE ARCHIVOS
# INDICES de primer orden
acumulate=NULL

for(i in listg){ 
  #Leo bases para graficar
  tabo<-read.table(paste("F:/p2_resultados/Sobol_final/indices_sobol_", i,"", sep=""), skip = 2, nrows=22,fill=TRUE)
  colnames(tabo)<-c("variable","porro", "porero", "ppbro", "ppbero","simave", "peak", "four", "sixty")
  tabo$name <-paste("",i,"")
  acumulate<-rbind(acumulate, tabo)
}

#INDICE de efectos totales
library(tibble)
totales=NULL

for(i in listg){ 
  
  #Leo bases para graficar
  tabo<-read.table(paste("F:/p2_resultados/Sobol_final/indices_sobol_", i,"", sep=""), skip = 26, nrows=22, fill=TRUE)
  tabo<-rownames_to_column(tabo, var = "variable")
  colnames(tab)<-c("variable","porro", "porero", "ppbro", "ppbero","ave", "peak", "four", "sixty")
  
  tabo$name <-paste("",i,"")
  totales<-rbind(totales, tabo)
  
}

#GRAFICOS POR SEPARADO

library("ggplot2")
acumulate<-subset(acumulate, acumulate$four>0.001)
ggplot2::ggplot(data=acumulate, aes(x=variable, y=four, fill= variable))+ coord_flip()+theme_bw()+geom_bar(stat="identity")+facet_wrap(~name, scales = "fixed", nrow = 2)
#geom_text(aes(label=four), vjust=0.1, color="black", size=3.5)



library("dplyr")
library("ggplot2")
totales<-subset(totales, totales$four>0.01)
ggplot2::ggplot(data=totales, aes(x=variable, y=four, fill= variable))+ coord_flip()+theme_bw()+geom_bar(stat="identity")+facet_wrap(~name, scales = "fixed", nrow = 2)
#geom_text(aes(label=four), vjust=0.1, color="black", size=3.5)


####### Grafico combinado 1st order and total effects #########

acumulate$indice <- c("First Order")
totales$indice <- c("Total Effects")
data<-rbind(acumulate, totales)

#renombro para poder ordenar mejor en el facet-grid
data$name[data$name==" A2 "]<-c("2,4-D Anguil")
data$name[data$name==" P2 "]<-c("2,4-D Pergamino")
data$name[data$name==" N2 "]<-c("2,4-D Parana")
data$name[data$name==" B2 "]<-c("2,4-D Tres Arroyos")
data$name[data$name==" J2 "]<-c("2,4-D Marcos Juarez")
data$name[data$name==" Ag "]<-c("Glyphosate Anguil")
data$name[data$name==" Pg "]<-c("Glyphosate Pergamino")
data$name[data$name==" Ng "]<-c("Glyphosate Parana")
data$name[data$name==" Bg "]<-c("Glyphosate Tres Arroyos")
data$name[data$name==" Jg "]<-c("Glyphosate Marcos Juarez")





names(acumulate)==names(totales)
# melt the data frame for plotting
data.m <- melt(data, id.vars= c("variable","indice", "name", "four", "sixty"))



##### IMPORTANTE!
#para cambiar si es 4 davg o 60 davg cambiar aca 4 por 60
#para cambiar de plaguicida leer 
# plot everything
#png("F:/SIMLAB/sobolplotglifo4.png")
windows(6000,4000)
data.m<-subset(data.m, data.m$sixty>0.01)
data.m<-data.m %>% filter(!variable=="cnb")
ggplot(data=data.m, aes(x=variable, y=sixty)) +
  coord_flip()+ theme_light()+ 
  geom_bar(aes(fill = indice), position = "dodge", stat="identity")+ 
  facet_wrap(~name, scales = "fixed", nrow = 2)+ 
  labs(y=c("Sobol Index"),x=c("Variable"))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1, 0)) 
#dev.off()



############ GEOM TILE ########

tab.m<-melt(tab, id.vars = c( "name", "var" ))
tab.m<-subset(tab.m, tab.m$variable == "MU4" | tab.m$variable == "MU60" )
tab.m$class<-paste(tab.m$name, tab.m$variable, sep="")

ggplot(data=tab.m, aes(x=class, y=var))+ geom_tile(aes(fill=value>1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_discrete(labels=c("wcss"=expression(bold(wcss)), parse=TRUE))

#totales
tab.mo<-melt(totales, id.vars = c( "name", "variable" ))
colnames(tab.mo)<-c("name", "var", "variables", "value")
#tab.mo <-as.character(tab.mo$variable)
tab.mo<-subset(tab.mo, tab.mo$variable == "four" | tab.mo$variable == "sixty" )
tab.mo$class<-paste(tab.mo$name, tab.mo$variable, sep="")

#create a new variable from incidence
breaks<-c(-0.1,0,0.2,0.4,0.6,0.8,1.01)

tab.mo$value2 <- cut(as.numeric(tab.mo$value),
                     breaks = breaks)
#interpolopaleta
colourCount = length(breaks)
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))

ggplot(data=tab.mo, aes(x=class, y=var))+ geom_tile(aes(fill=value2))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_discrete(labels=c("wcss"=expression(bold(wcss)), parse=TRUE))+
  scale_fill_manual(values=getPalette(colourCount), na.value="white")


scale