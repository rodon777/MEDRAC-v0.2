#######################################################################
### TFG Arqueología ###################################################
#######################################################################
### Universidad de Barcelona 2016 - 2017 ##############################
### Facultat de Geografía i Historia     ##############################
#######################################################################
### Autor: Rodrigo A. Donoso Rojas       ##############################
### contacto: rodrigodonoso@outlook.es   ##############################
#######################################################################
### Version >= R 4.x  #################################################	
#######################################################################
############################# I #######################################
#######################################################################

# Evitar que strings se conviertan en factors por defecto
options(stringsAsFactors = FALSE)

#######################################################################
# LIBRERÍAS ESENCIALES
#######################################################################
library(bezier)    			# curvas Bezier
library(plotrix)   			# gráficos base
library(scatterplot3d)   	# gráficos 3D interactivos
library(tcltk)     			# paneles interactivos básicos
library(stats)     			# funciones estadísticas básicas (ya viene con R)
library(pracma)
library(digest)

#######################################################################
# Rutas por defecto (puedes modificarlas)
ruta <- "/Users/rodrigodonoso/Desktop/MEDRAC 2025/Matrices/formato texto/"
destination <- "/Users/rodrigodonoso/Desktop/MEDRAC 2025/"

#######################################################################
#### IMPORT LIST FUNCTION #############################################
#######################################################################


IMP <- function (contour,ruta) {
	
	list <- c()
	
	if (contour==1) {d<-"_ci.txt"} else if(contour==2){d<-"_ce.txt"}
	
	path <- ruta

	file.names <- dir(path,d)


	setwd(path)


	for (i in 1:length(file.names)) {
	
		file <- read.table(file.names[i], header=TRUE,sep="\t",dec=",",row.names=NULL)
 	
		list <- c(list,(list(file)))
	
		}
			
	names(list) <- file.names		
	
list														# Importacion de ficheros *.txt






	if (contour > 1) {											# Condición revertir columnas contorno exterior
	
		REV <- function (x) {

			storage_rev <- c()

			for (k in 1:length(x)) {
	
 				r <- apply(x[[k]],2,rev)
 
 				storage_rev <- c(storage_rev,list(r))
 
 				}
 
 			storage_rev
 
		}

	list <- REV(list)	
	
}	
	


	
	
	storage <- c()
	
	for (i in 1:length(list)) { 
		
			print(i)
			
		M <- list[[length(1:i)]]
		
			print(M)
		
			no.ac <- print(M[1:nrow(M)-1,2]-M[2:nrow(M),2])
			
			
		M_1 <- list[[length(1:i)]]
	
				M_1[2:nrow(M_1),2] <- no.ac
				
	
			X <- ((M_1[1:nrow(M_1)-1,1])-(M_1[2:nrow(M_1),1]))^2
	
			Y <- (M_1[2:nrow(M_1),2])^2

			LC <- print(sqrt(X+Y))

			LC <- print(cumsum(c(0,LC)))				# Longitud contorno


			storage <- c(storage,list(cbind(M,LC)))		# contourenedor de resultados (X,Y,CL)
					


	}
				
	names(storage) <- file.names						# list de nombres
		
		
	FHD <-	function(x) {								# Extraccion de variables Angulo y Volumen
		
		
	M <- matrix(0,nrow(x),5)
	dimnames(M)=list(NULL,c("X","Y","CL","A","V"))
	
	
	M[,1] <- x[,1]
	M[,2] <- x[,2]
	M[,3] <- x[,3]
	
	
	M[2:nrow(M),3] = (x[2:nrow(x),3]-x[1:nrow(x)-1,3])	
		
									
	NY<-function(x) {

		Y=(x[1:nrow(x)-1,2]-x[2:nrow(x),2])

} 	; Y <- NY(x)									# Desacumular eje Y
	
	x[2:nrow(x),2] <- Y								# M = variable Y no cumulative
	
	
								
	
	F1<-function(x) {

		X=(x[1:nrow(x)-1,1]-x[2:nrow(x),1])
		H=(sqrt((x[1:nrow(x)-1,1]-x[2:nrow(x),1])^2+x[2:nrow(x),2]^2))
		R=(X/H)
		A=(acos(R)*180/pi)

} ; A <- F1(x)										# Angulo contorno

	M[1:nrow(x)-1,4] <- A		
	
	
	
	
	F2<-function (x) {
	
	Fa<-(x[2:nrow(x),1]^2*pi*x[2:nrow(x),2])
	Fb<-(x[1:nrow(x)-1,1]^2*pi*x[2:nrow(x),2])
	V<-((Fb-Fa)/2+Fa)
	sqrt(V^2)
	
} ; V <- F2(x)										# Volumen interior

	M[1:nrow(x)-1,5] <- V	
	
	M

		}  	
		
		Lst <- lapply(storage,FHD)

}


#######################################################################
################################ II ###################################
#######################################################################

#######################################################################
#### IMPORT ARRAY FUNCTION ############################################
#######################################################################
	

AR <- function (contour,number_points,ruta) {
	
	list <- c()
	
	if (contour==1) {d<-"_ci.txt"} else {d<-"_ce.txt"}
	
	path <- ruta

	file.names <- dir(path,d)


	setwd(path)


	for (i in 1:length(file.names)) {
	
		file <- read.table(file.names[i], header=TRUE,sep="\t",dec=",",row.names=NULL)
 	
			list <- c(list,(list(file)))
	
			}		
	
list






if (contour > 1) {											# Condición revertir columnas contourorno exterior
	
	REV <- function (x) {

		storage_rev <- c()

			for (k in 1:length(x)) {
	
 				r <- apply(x[[k]],2,rev)
 
 				storage_rev <- c(storage_rev,list(r))
 
 				}
 
 		storage_rev
 
	}

list <- REV(list)	
	
}	
	





	storage <- c()
	
	for (i in 1:length(list)) { 
		
			print(i)
			
		M <- list[[length(1:i)]]
		
			print(M)
		
			no.ac <- print(M[1:nrow(M)-1,2]-M[2:nrow(M),2])
			
			
		M_1 <- list[[length(1:i)]]
	
				M_1[2:nrow(M_1),2] <- no.ac
				
	
			X <- ((M_1[1:nrow(M_1)-1,1])-(M_1[2:nrow(M_1),1]))^2
	
			Y <- (M_1[2:nrow(M_1),2])^2

			LC <- (sqrt(X+Y))

			LC <- print(cumsum(c(0,LC)))		# Longitud contorno


			storage <- c(storage,list(cbind(M,LC)))
					
			
	BZ <- function (x) {

		t <- seq(0, 1, length=number_points)
		ip <- (bezier(t=t, p=x[1:nrow(x),1:3]))

		}

	}
				
	names(storage) <- file.names
		storage <- lapply(storage,BZ)	
		
		
	FHD <-	function	(x) {
		
		
	M <- matrix(0,nrow(x),5)
	dimnames(M)=list(NULL,c("X","Y","CL","A","V"))
	
	
	M[,1] <- x[,1]
	M[,2] <- x[,2]
	M[,3] <- x[,3]
	
	
	M[2:nrow(M),3] = (x[2:nrow(x),3]-x[1:nrow(x)-1,3])	
		
									
	NY<-function(x) {

		Y=(x[1:nrow(x)-1,2]-x[2:nrow(x),2])

} 	; Y <- NY(x)							# Desacumular eje Y
	
	x[2:nrow(x),2] <- Y						# M = return Y no cumulative
	
	
								
	
	F1<-function(x) {

		X=(x[1:nrow(x)-1,1]-x[2:nrow(x),1])
		H=(sqrt((x[1:nrow(x)-1,1]-x[2:nrow(x),1])^2+x[2:nrow(x),2]^2))
		R=(X/H)
		A=(acos(R)*180/pi)

} ; A <- F1(x)								# Angulo contourorno

	M[1:nrow(x)-1,4] <- A		
	
	
	
	
	F2<-function (x) {
	
	Fa<-(x[2:nrow(x),1]^2*pi*x[2:nrow(x),2])
	Fb<-(x[1:nrow(x)-1,1]^2*pi*x[2:nrow(x),2])
	V<-((Fb-Fa)/2+Fa)
	sqrt(V^2)
	
} ; V <- F2(x)								# Volumen interior

	M[1:nrow(x)-1,5] <- V	
	
	M

		}  	
		
		Lst <- lapply(storage,FHD)
	
	IN <- function (x) {
	
		array(unlist(x),dim=c(nrow(x[[1]]),ncol(x[[1]]),length(x)))
	
	}
	
	ARRAY <- IN(Lst) ; dimnames(ARRAY)=list(NULL,c("X","Y","CL","A","V"),file.names)
	
ARRAY

}



#########################################################################
############################### III #####################################
#########################################################################

#########################################################################
######################## NORMALITATION ARRAY ############################
#########################################################################


NF <-function(data) {
	
	
if (is.list(data)) {
	
	M <- (data)
	
for (i in 1:length(data)) {


M[[i]][,1] <- data[[i]][,1]/data[[i]][length(data[[i]][,1]),1]

M[[i]][,2] <- data[[i]][,2]/max(data[[i]][,2])

M[[i]][,3] <- data[[i]][,3]/sum(data[[i]][,3])

M[[i]][,4] <- data[[i]][,4]/180

M[[i]][,5] <- data[[i]][,5]/sum(data[[i]][,5])


		}

	names(M) <- names(data)

	M

} else {
	
	M <- array(NA,c(dim(data)[1],dim(data)[2],dim(data)[3]))
	
	for (i in 1:dim(data)[3]) {

M[,1,i] <- data[,1,i]/data[dim(data)[[1]],1,i]

M[,2,i] <- data[,2,i]/max(data[,2,i],na.rm=T)

M[,3,i] <- data[,3,i]/sum(data[,3,i],na.rm=T)

M[,4,i] <- data[,4,i]/180

M[,5,i] <- data[,5,i]/sum(data[,5,i],na.rm=T)


		}

	names_dim <- dimnames(data)[3] ; names_dim <- names_dim[[1]]

	dimnames(M)=list(NULL,c("X","Y","CL","A","V"),names_dim)

	M
	
	}
}


########################################################################
### ORIGIN ZERO EXTERNAL CONTOUR #######################################
########################################################################

OR_ce <- function (list) {

storage_ce <- c()
storage <- c()

	
	for (k in 1:length(list)) {
		
		storage_ce <- c(storage_ce,as.vector(sqrt(list[[k]][1,c(2)]^2)))
		
		storage <- c(storage,list(storage_ce[k]+L2[[k]][,c(2)]))

list[[k]][,c(2)] <- storage[[k]]

}

list

}


########################################################################
### NO ORIGIN ZERO INTERNAL CONTOUR ####################################
########################################################################

OR_ci <- function (list_ci,list_ce) {

storage_ce <- c()
storage <- c()
storage1 <- c()

	
	for (k in 1:length(list_ce)) {
		
		storage_ce <- c(storage_ce,as.vector(sqrt(list_ce[[k]][1,c(2)]^2)))
		
		storage <- c(storage,list(storage_ce[k]+list_ci[[k]][,c(2)]))

list_ci[[k]][,c(2)] <- sqrt(storage[[k]]^2)

}


list_ci

}



#######################################################################
############################### IV ####################################
#######################################################################

IND <- function(array,destination) {
	
	individuos <- dimnames(array)[3]
	individuos <- individuos[[1]]
	
	path <- destination
	
	setwd(path)
	
	write.csv(individuos,'individuos.txt')
	
	print(individuos)
}

#######################################################################
GT <- function(list,object) {
	gtable(list[[object]], contour=T)
	names(list[object])
} 

#######################################################################

## ind <- IND(A1,destination)

#######################################################################
#### PLOTING BOXES #### 2data2 ###########################################
#######################################################################


COMP <- function (list_ci,list_ce,object1,object2) {


obj2 <- names(list_ce[object1])
obj1 <- names(list_ce[object2])
vs <- "comparison"
	
if (is.list(c(list_ce,list_ci))) {	

	plot(list_ce[[object1]],type="l",col="blue",xlim=c(0,1.5),ylim=c(-1,1.5), cex.axis=0.8, las=1,main=c(obj2,vs,obj1), cex.main=0.8, xlab = "normalized X axis", ylab = "normalized Y axis")
	lines(list_ci[[object1]],col="blue")
	lines(list_ce[[object2]],type="l",col="red")
	lines(list_ci[[object2]],type="l",col="red")

} else {
	
	
	matrix(c(1:2),nrow=2,byrow=FALSE)				
layout(matrix(c(1:2),nrow=2,byrow=FALSE))

conf2data1=matrix(c(1:2),nrow=2,byrow=FALSE)
conf2data1=matrix(c(1:2),nrow=2,byrow=FALSE)

layout(conf2data1)
layout.show(2)
	
	plot(list_ce[,,object1],type="l",col="blue",xlim=c(0,1.5),ylim=c(-1,1.5))
	lines(list_ce[,,object2],col="red")
	plot(list_ci[,,object2],type="l",col="blue",xlim=c(0,1.5),ylim=c(-1,1.5))
	lines(list_ci[,,object1],type="l",col="red")
	
}


}



PER_1 <- function (data1,data2,a,b,c,d) {
	
	
	if (is.list(data1)) {
		
		matrix(c(1:4),nrow=2,byrow=FALSE)				
layout(matrix(c(1:4),nrow=2,byrow=FALSE))

conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)
conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)

layout(conf2data2)
layout.show(4)

nombres <- names(data1)


	plot(data1[[a]],ylab="Y",xlab="X", main=nombres[a],type="l",col="blue")
	lines(data2[[a]],type="l",col="red")

	plot(data1[[b]],ylab="Y",xlab="X", main=nombres[b],type="l",col="blue")
	lines(data2[[b]],type="l",col="red")

	plot(data1[[c]],ylab="Y",xlab="X", main=nombres[c],type="l",col="blue")
	lines(data2[[c]],type="l",col="red")

	plot(data1[[d]],ylab="Y",xlab="X", main=nombres[d],type="l",col="blue")
	lines(data2[[d]],type="l",col="red")


}else {
	
	
matrix(c(1:4),nrow=2,byrow=FALSE)				
layout(matrix(c(1:4),nrow=2,byrow=FALSE))

conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)
conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)

layout(conf2data2)
layout.show(4)

dim <- dimnames(data1[,,])

n <- dim[[3]]

	plot(data1[,,a],ylab="Y",xlab="X", main=n[a],type="l",col="blue")
	lines(data2[,,a],type="l",col="red")

	plot(data1[,,b],ylab="Y",xlab="X", main=n[b],type="l",col="blue")
	lines(data2[,,b],type="l",col="red")

	plot(data1[,,c],ylab="Y",xlab="X", main=n[c],type="l",col="blue")
	lines(data2[,,c],type="l",col="red")

	plot(data1[,,d],ylab="Y",xlab="X", main=n[d],type="l",col="blue")
	lines(data2[,,d],type="l",col="red")

	}

}



PER_2 <- function (list1,list2,array1,array2,object1,object2) {
	
	matrix(c(1:4),nrow=2,byrow=FALSE)				
	layout(matrix(c(1:4),nrow=2,byrow=FALSE))

	conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)
	conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)

	layout(conf2data2)
	layout.show(4)

dim <- dimnames(array1[,,])

n <- dim[[3]]
	
	plot(list1[[object1]],type="l",main=n[object1],col="blue")
	lines(list2[[object1]],col="red")
	
	plot(list1[[object2]],type="l",main=n[object2],col="blue")
	lines(list2[[object2]],col="red")
	
	plot(array1[,,object1],ylab="Y",xlab="X", main=n[object1],type="l",col="blue")
	lines(array2[,,object1],type="l",col="red")
	
	plot(array1[,,object2],ylab="Y",xlab="X", main=n[object2],type="l",col="blue")
	lines(array2[,,object2],type="l",col="red")

}



PER_3 <- function (list_ce,list_ci,array1,array3,a) {
	
	matrix(c(1:4),nrow=2,byrow=FALSE)				
	layout(matrix(c(1:4),nrow=2,byrow=FALSE))

	conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)
	conf2data2=matrix(c(1:4),nrow=2,byrow=FALSE)

	layout(conf2data2)
	layout.show(4)

ni <- names(list_ci)
ne <- names(list_ce)

	
	plot(list_ci[[a]],type="p",main=ni[a])
	
	
	plot(array1[,,a],ylab="Y",xlab="X", main=ni[a],type="p")
	
	
	plot(array3[,,a],ylab="Y",xlab="X", main=ni[a],type="p")
	
	
	plot(list_ce[[a]],type="l",main=c(main=ni[a],ne[a]),col="blue")
	lines(list_ci[[a]],col="red")
	

}



P3D <- function (list,individuo) {
	
	a <- as.matrix(list[[individuo]])
	
	plot3d(a[,1:2])

}


#######################################################################
#######################################################################


PO <- function (array,individuo,variable) {
	
	coplot( array[,1,individuo] ~ array[,2,individuo] | array[,variable,1],col=c("red","blue","green","orange","yellow"), panel=function(x,y,...) {
          panel.smooth(x,y,span=.8,iter=5,...)
          abline(lm(y ~ x), col="blue") } )
	
} 


#######################################################################
