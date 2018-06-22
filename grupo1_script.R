##########################################
# grupo 1                                #
# Lic. Santiago Morello                  #
# Lic. María Florencia D’Andrea          #
# Lic. María Cecilia Dalton              #    
##########################################




library(dendextend)
library(cluster)
library(ade4)
library(vegan)
library(ggmap)
library(MASS) 
library(psych)
library(ggplot2)
library(ellipse)
library(GGally)
library(data.table)   
##### Se leen las matrices #####

invertebrados<-read.csv("C:/Users/User/Desktop/USARESTAinverte_sinNA.csv")
algas<-read.csv("C:/Users/User/Desktop/USARESTAalgas.csv")
ambientales<-read.csv("C:/Users/User/Desktop/USARESTAenv.csv")
##### MAPA

#Tabla con longitudes y latitudes para el mapa
tabla1<-data.frame(x=c("Ushuaia-Ensenada Zaratiegui","Ushuaia-Playa Larga","Rio Grande-Estancia Viamonte","Rio Grande-Cabo Domingo"),lat=c(-54.84,-54.81,-53.99,-53.68), long=c(-68.47, -68.21, -67.3, -67.83))

#Obtengo mapa
gg<-get_googlemap(center = c(-67.5, -54), zoom = 7, color = "bw",
                  style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")
#Grafico mapa
ggmap(gg) +
  geom_point(data = tabla1, aes(x = long, y = lat), color="purple", size=1.5)+
  geom_text_repel(data = tabla1, aes(x = long, y = lat, label = x), size=3) 


##### Analisis de Cluster (Invertebrados)  #####

#por playa
#Factores: Ushuaia Urbano (U.U), Ushuaia No Urbano (U.NU), Rio Grande Urbano (RG.U), Rio Grande No Urbano (RG.NU)

#veo que factores hay
unique(invertebrados$playa)

#detecte un error de tipeo
#corrijo y genero nueva columna
invertebrados$abrev[invertebrados$playa=="Ushuaia UN"] <- "U.NU"
invertebrados$abrev[invertebrados$playa=="Ushuaia NU"] <- "U.NU"
invertebrados$abrev[invertebrados$playa=="Ushuaia UR"] <- "U.U"
invertebrados$abrev[invertebrados$playa=="Rio Grande NU"] <- "RG.NU"
invertebrados$abrev[invertebrados$playa=="Rio Grande UR"] <- "RG.U"

#Si esto es verdadero mis columnas estan completas
sum(!is.na(invertebrados$abrev))==nrow(invertebrados)

#asigno a cada fila el nombre y un valor unico para luego poder reconocerlo en el arbol
rownames(invertebrados) <- NULL

#Reduzco la matriz a la informacion indispensable
inv<-invertebrados[,-49]
info.inv<-c(1:5)
inv<-inv[,-info.inv]

#Estandarizo por el m?todo de Hellinger
EST<-decostand(inv, method="hel")

#Genero la matriz de distancia
MD<-vegdist(EST, method="euclidean", diag = TRUE, upper=FALSE)

#Elijo m?todo de agrupamiento

#Correlacion cofen?tica
methods<-c("single", "ward.D","ward.D2","complete", "average", "median", "centroid", "mcquitty")
coef=NULL
for(i in methods){
  cluster<-hclust(MD, method=i)
  single.coph <- cophenetic(cluster)
  cor<-cor(MD, single.coph)
  data<-data.frame(cor, i)
  colnames(data)<-c("CCof", "Method")
  coef=rbind(coef, data)
}

#?Cu?l es ek mejor m?todo de ligamiento?
#OJO! https://stats.stackexchange.com/questions/217519/centroid-linkage-clustering-with-hclust-yields-wrong-dendrogram

library(ggplot2)
ggplot(data = coef, aes(x= reorder(Method, -CCof), y= CCof))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=11))

#Metodo de ligamiento elegido
cluster<-hclust(MD, method="average")

#Grafico
par(cex=0.5)#Cuestiones esteticas del gr?fico
dend<- as.dendrogram(cluster)

#QUIERO UNA VARIABLE QUE HABIA ELIMINADO
inv.comp<-cbind(invertebrados[,info.inv], inv)

##### PARTE GRAFICA
#determino cada fila dentro de un factor para playa y nivel intermareal
fac.playa <- factor(inv.comp$playa)
fac.interm <- factor(inv.comp$Intertidal.level)

#saco la cantidad de factores como numero
xplaya <- length(unique(fac.playa))
xinterm<- length(unique(fac.interm))

#Obtengo una cantidad de colores de acuerdo al numero de niveles de cada factor (paleta rainbow)
cols_playa <- colorspace::rainbow_hcl(xplaya , c = 70, l  = 50)
cols_inter <- colorspace::rainbow_hcl(xinterm, c = 70, l  = 50)

#defino la serie de colores de acuerdo a la fila
col_playa <- cols_playa[fac.playa]
col_inter  <- cols_inter[fac.inter]

### Grafico cluster por playa
par(mar = c(6,4,1,1))
dend <- set(dend, "labels_cex", NA) #elimino labels
plot(dend)
colored_bars(col_playa, dend, rowLabels = NA, y_shift= -0.05)
legend("topright", legend = levels(fac.playa), fill = cols_playa, cex = 0.5)

#### Grafico cluster por intermareal 
par(mar = c(6,4,1,1))
dend <- set(dend, "labels_cex", NA) #elimino labels
plot(dend)
colored_bars(col_inter, dend, rowLabels = NA, y_shift= -0.05)
legend("topright", legend = levels(fac.inter), fill = cols_inter, cex = 0.5)
###
### LDA usamos de ejemplo la matriz de algas
algas <- read.csv ("USARESTAalgas.csv")
algas.info <-algas[, 6:9]



#DESCRIPTIVA
algas.cor = cor(algas.info)# Las variables no presetan una correlacion de interes
algas2<-as.data.frame(cbind(algas,playasA))
ggpairs(algas2,axisLabels="none") 

algas2 = as.data.frame(cbind(algas,nivelA))
ggpairs(algas, columns=c(2,6:9))

# Funciones discriminantes sobre las variables originales
algas.lda <- lda(algas$playa~., algas[6:9])

# ?Cual es la variable que m?s contribuye a la diferencia entre los grupos?
algas.lda 
# Con dos ejes explicamos mas del 80% de la separacion entre las playas.
# El primer eje explica el 60% de la separacion entre los grupos (LD1Trace: 0.5977)
# Y, el segundo eje explica el 23% de la separacion entre los grupos (LD2trace: 0.2367)

# Autovalores
algas.lda$svd^2 #[1] 5.284479 2.092417 1.464766

#LDA con variables estandarizadas

algas.sc <- as.data.frame(scale(algas.info))

algas.lda2 <- lda(algas$playa~., algas.sc)
algas.lda2
#Coefficients of linear discriminants:
##########################LD1        LD2         LD3
#Chlorophyta_cob        0.6299886  0.6223541  0.02889255
#AlgasIncrustantes_cob  0.7196750 -0.6063355 -0.42230489

# Carga a partir de la correlacion de la funcion discriminante con las variables originales
scoresFD1=predict(algas.lda, algas.info)$x[,1]
cor(scoresFD1,algas.info) 

scoresFD2=predict(algas.lda, algas.info)$x[,2]
cor(scoresFD2,algas.info)



# Verificacion de la clasificaci?n
# Posicion de los objetos en el espacio de las variables can?nicas
(Fp <- predict(algas.lda)$x)
algas.class <- predict(algas.lda)$class

# Probabilidad de los objetos de pertenecer al grupo
(algas.post <- predict(algas.lda)$posterior)

# Tabla de clasificaci?n previa y predicha por el modelo
algas.table <- table(algas$playa, algas.class)

# Proporcion de clasificaciones correctas
diag(prop.table(algas.table, 1))
# Con estos datos, solo las muestras de las Rio Grande NU (Estancia Viamonte)fueron correctamente clasificadas en un 86%.

prop = algas.lda$svd^2/sum(algas.lda$svd^2)
prop #[1] 0.5976794 0.2366542 0.1656664 #varianza entre grupos explicada por cada FD
#el primer eje explica el 60% de la varianza entre las 4 playas.


####Grafico los objectos en el espacio de las variables canonicas

#scores para ambas funciones discriminantes
algas.scores <- predict(algas.lda, algas.info)
# generamos un dataframe con todos los datos necesarios para graficar
dataalgas.lda <- data.frame(varnames=rownames(coef(algas.lda)), coef(algas.lda))

p <- qplot(data=data.frame(algas.scores$x), #guardado
           main="LDA Cobertura de Algas por Playa",
           x=LD1,
           y=LD2,
           shape=as.factor(algas$playa),color=as.factor(algas$playa)) + stat_ellipse()
p <- p + geom_hline(aes(0), size=0.2,yintercept=0) + geom_vline(aes(0), size=0.2,xintercept = 0)
p <- p + geom_text(data=dataalgas.lda,
                   aes(x=LD1, y=LD2,
                       label=varnames,
                       shape=NULL, linetype=NULL),
                   size = 3, vjust=0.5,
                   hjust=0, color="red")
p <- p + geom_segment(data=dataalgas.lda,
                      aes(x=0, y=0,
                          xend=LD1, yend=LD2,
                          shape=NULL, linetype=NULL),
                      arrow=arrow(length=unit(0.2,"cm")),
                      color="red")
print(p)

