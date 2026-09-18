#########################################################################
### COMPROBACION DE RUIDO ###############################################
#########################################################################

(AREA.FUN(A1,1)-AREA.FUN(L1,1))/1000			# resultado en cm2

(AREA.FUN(A3,1)-AREA.FUN(L3,1))/1000


#########################################################################
########################## CORRELATION ANALISIS #########################
#########################################################################


hist(CO1[1,5,1:40],main="correlación entre variables radio (X) y volumen (V)",xlab="porcentaje")
lines(density(CO1[1,5,1:40]))

hist(CO1[1,4,1:40],main="correlación entre variables radio (X) y ángulo (A)",xlab="porcentaje")
lines(density(CO1[1,4,1:40]))

hist(CO1[2,5,1:40],main="correlación entre variables altura (Y) y volumen (V)",xlab="porcentaje")
lines(density(CO1[2,5,1:40]))

hist(CO1[2,4,1:40],main="correlación entre variables altura (Y) y ángulo (A)",xlab="porcentaje")
lines(density(CO1[2,4,1:40]))



#########################################################################
##################### CLUSTER CORRELATION MATRIX ########################
#########################################################################


plot(hclust(dist(CO2[,,1])),main="Cluster",ylab="Radio")

plot(hclust(dist(CO2[,,2])),main="Cluster",ylab="Altura")

plot(hclust(dist(CO2[,,3])),main="Cluster",ylab="Longitud de contorno")

plot(hclust(dist(CO2[,,4])),main="Cluster",ylab="Ángulo")

plot(hclust(dist(CO2[,,5])),main="Cluster",ylab="Volumen")


#########################################################################
############################ REGRESION ##################################
#########################################################################




#########################################################################
### TRAMA FINAL DE DATOS ################################################
#########################################################################


TRF <- function (x,y,destination) {

	storage1	<- c()
	storage2	<- c()
	storage3	<- c()
	storage4	<- c()
	storage5	<- c()
	storage6	<- c()
	
	
	if (is.list(x)) {
	
		for (i in 1:length(x)) {
		
			storage1	<- c(storage1,mean(x[[i]][,1]))
			storage2	<- c(storage2,median(x[[i]][,2]))
			storage3	<- c(storage3,sum(x[[i]][,3]))
			storage4	<- c(storage4,sd(x[[i]][,4]))
			storage5	<- c(storage5,sum(x[[i]][,5])/1000)
			storage6	<- c(storage6,sum(y[[i]][,5]/1000)-sum(x[[i]][,5]/1000))
			
		}
	
	M <- cbind(storage1,storage2,storage3,storage4,storage5,storage6)
	
	dimnames(M)=list(names(x),c("mean radio","median eight","contour length","sd angle","volume (cm3)","p. volume"))
		
} else {
		
	
		for (i in 1:dim(x)[[3]]) {
		
			storage1	<- c(storage1,mean(x[,1,i]))
			storage2	<- c(storage2,mean(x[,2,i]))
			storage3	<- c(storage3,sum(x[,3,i]))
			storage4	<- c(storage4,mean(x[,4,i]))
			storage5	<- c(storage5,sum(x[,5,i])/1000)		
			storage6	<- c(storage6,sum(y[,5,i])/1000-sum(x[,5,i]/1000))
		
		}
	
	M <- cbind(storage1,storage2,storage3,storage4,storage5,storage6)
	
	dimnames(M)=list(dimnames(x)[[3]],c("mean radio","median height","contour length","sd angle","volume (cm3)","p. volume"))
	
	}
	
	path <- destination
	setwd(path)
	
	write.csv(M,'trama_datos.txt')
	print(M)
	
}


M <- TRF(L1,L2,destination)


#########################################################################
### CLUSTER MORFOMETRICO ################################################
#########################################################################

# plot(hclust(dist(M),method="centroid"),main="Morphometric Cluster",xlab=NULL,ylab="Distance")

M5 <- M[, 1:5]
plot(hclust(dist(M5), method = "centroid"),
     main = "Morphometric Cluster", xlab = NULL, ylab = "Distance")




#########################################################################
### CLUSTER AREA BAJO FUNCION ###########################################
#########################################################################

AUF <- function (array1,array2) {

AUF_CI <- c()
AUF_CE <- c()

for (k in 1:dim(array1)[[3]]) {
	
	AUF_CI <- rbind(AUF_CI,auc(array1[,1,k],array1[,2,k], type = 'spline',from=min(array1[,1,k]),to=array1[length(array1[,1,k]),1,k]))
	
	
	AUF_CE <- rbind(AUF_CE,auc(array2[,1,k],array2[,2,k], type = 'spline',from=min(array2[,1,k]),to=array2[length(array2[,1,k]),1,k]))
	
	}
	
a <- cbind(AUF_CI,AUF_CE)



}


A <- AUF(A3,A4)


plot(hclust(dist(A),method="centroid"),main="Cluster Morfométrico",ylab="Distance")


#########################################################################

#########################################################################
############################# PROCRUSTES ################################
#########################################################################

A <- A1[,1:2,1]
B <- A1[,1:2,2]
plotshapes(A,B,joinline=1:13)




ans <- procOPA(A1[,1:2,2],A1[,1:2,3])
plotshapes(ans$Ahat,ans$Bhat,joinline=1:13)

plot(ans$Bhat[,1],ans$Bhat[,2],type="l")
lines(ans$Ahat[,1],ans$Ahat[,2],col="red")

#########################################################################
### FOURIER SERIES ######################################################
#########################################################################

#########################################################################


