#########################################################################
################################ V ######################################
#########################################################################

#########################################################################
######################## EdataPLORATORY ANALYSIS ###########################
#########################################################################

#summary(L1[[1]])

#########################################################################
### SUMMARY ###
###############


sumario <- function (data, object) {

	if (is.list(data)) {
	
		storage_1 <- c()
		storage_2 <- c()
		storage_3 <- c()
		storage_4 <- c()
		storage_5 <- c()
		storage_6 <- c()
		storage_7 <- c()
				
	
	for (i in 1:5) {
		
		for (j in 1:length(data)) {	
	
	Min <- min(data[[j]][,i])
	
	q	<- quantile(data[[j]][,i],names=F)
	st 	<- q[2]
	
	Median	<- median(data[[j]][,i])
	Mean	<- mean(data[[j]][,i])
	rd		<- q[4]
	Max 	<- max(data[[j]][,i])
	sd		<- sd(data[[j]][,i])
	
	
	storage_1 <- c(storage_1,Min)
	storage_2 <- c(storage_2,st)
	storage_3 <- c(storage_3,Median)
	storage_4 <- c(storage_4,Mean)
	storage_5 <- c(storage_5,rd)
	storage_6 <- c(storage_6,Max)
	storage_7 <- c(storage_7,sd)
	
	
		}

	}

	s <- rbind(storage_1,storage_2,storage_3,storage_4,storage_5,storage_6,storage_7)
	

	M <- array(NA,c(7,5,length(data))) ; dimnames(M)=list(c("Min","1st Qu.","Median","Mean","3rd Qu.", "Madata.","sd"),c("data","Y","CL","A","V"),names(data))


	M[,1,1:length(data)] <- s[,1:length(data)]
	M[,2,1:length(data)] <- s[,(length(data)+1):(length(data)*2)]
	M[,3,1:length(data)] <- s[,((length(data)*2)+1):(length(data)*3)]
	M[,4,1:length(data)] <- s[,((length(data)*3)+1):(length(data)*4)]
	M[,5,1:length(data)] <- s[,((length(data)*4)+1):(length(data)*5)]
	
	
}	else {
	
		storage_1 <- c()
		storage_2 <- c()
		storage_3 <- c()
		storage_4 <- c()
		storage_5 <- c()
		storage_6 <- c()
		storage_7 <- c()
				
	
	for (i in 1:5) {
		
		for (j in 1:dim(data)[3]) {	
	
	Min <- min(data[1:nrow(data),i,j])
	
	q	<- quantile(data[1:nrow(data),i,j],names=F)
	st 	<- q[2]
	
	Median	<- median(data[1:nrow(data),i,j])
	Mean	<- mean(data[1:nrow(data),i,j])
	rd		<- q[4]
	Max 	<- max(data[1:nrow(data),i,j])
	sd		<- sd(data[1:nrow(data),i,j])


	storage_1 <- c(storage_1,Min)
	storage_2 <- c(storage_2,st)
	storage_3 <- c(storage_3,Median)
	storage_4 <- c(storage_4,Mean)
	storage_5 <- c(storage_5,rd)
	storage_6 <- c(storage_6,Max)
	storage_7 <- c(storage_7,sd)
	
	
		}

	}
	

	s <- rbind(storage_1,storage_2,storage_3,storage_4,storage_5,storage_6,storage_7)
	
	names <- dimnames(data)[3] ; names <- names[[1]]

	M <- array(NA,c(7,5,dim(data)[3])) ; dimnames(M)=list(c("Min","1st Qu.","Median","Mean","3rd Qu.", "Madata.","sd"),c("data","Y","CL","A","V"),names)


	M[,1,1:dim(data)[3]] <- s[,1:dim(data)[3]]
	M[,2,1:dim(data)[3]] <- s[,(dim(data)[3]+1):(dim(data)[3]*2)]
	M[,3,1:dim(data)[3]] <- s[,((dim(data)[3]*2)+1):(dim(data)[3]*3)]
	M[,4,1:dim(data)[3]] <- s[,((dim(data)[3]*3)+1):(dim(data)[3]*4)]
	M[,5,1:dim(data)[3]] <- s[,((dim(data)[3]*4)+1):(dim(data)[3]*5)]
	
	}
	
print(names(data[object]))	
print(M[,,object])

}



#########################################################################
################################ VI #####################################
#########################################################################

#########################################################################
## ACP ##################################################################
#########################################################################

COR <- function (data, object) {
	
	if (is.list(data)) {
	
		storage <- c()
	
		for (i in 1:length(data)) {
		
			storage <- c(storage,list(cor(data[[i]])))
		
		}

	
	nombres <- names(data)
	
	storage <- array(unlist(storage),dim=c(nrow(storage[[1]]),ncol(storage[[1]]),length(storage)))
	
	dimnames(storage)=list(c("data","Y","CL","A","V"),c("data","Y","CL","A","V"),nombres)
	
	print(names(data[object]))	
	print(storage[,,object])
	
} else {

	storage <- c()
	
	for (i in 1:dim(data)[2]) {
		
		storage <- c(storage,list(cor(data[,i,1:dim(data)[3]])))
		
	}
	
	nombres <- dimnames(data)[[2]]
	
	storage <- array(unlist(storage),dim=c(nrow(storage[[1]]),ncol(storage[[1]]),length(storage)))
	
	dimnames(storage)=list(dimnames(data)[[3]],dimnames(data)[[3]],nombres)
	
	print(names(data[object]))	
	print(storage[,,object])
	
	}
}



COV <- function (data, object) {
	
		if (is.list(data)) {
	
		storage <- c()
	
		for (i in 1:length(data)) {
		
			storage <- c(storage,list(cov(data[[i]])))
		
		}

	
	nombres <- names(data)
	
	storage <- array(unlist(storage),dim=c(nrow(storage[[1]]),ncol(storage[[1]]),length(storage)))
	
	dimnames(storage)=list(c("data","Y","CL","A","V"),c("data","Y","CL","A","V"),nombres)
	
	print(names(data[object]))	
	print(storage[,,object])
	
} else {
	
	storage <- c()
	
	for (i in 1:dim(data)[2]) {
		
		storage <- c(storage,list(cov(data[,i,1:dim(data)[3]])))
		
	}
	
	nombres <- dimnames(data)[[2]]
	
	storage <- array(unlist(storage),dim=c(nrow(storage[[1]]),ncol(storage[[1]]),length(storage)))
	
	dimnames(storage)=list(dimnames(data)[[3]],NULL,nombres)
	
	print(names(data[object]))	
	print(storage[,,object])

	}	
}



PCA <- function (data,type,logico) {
	
	storage <- c()
	
	if (is.list(data)) {
		
		if (type > 1) {acp <- princomp} else {acp <- prcomp}
	
			for (i in 1:length(data)) {
	
				pca <- acp(data[[i]], scale=logico)
	
			storage <- c(storage,list(pca))
	
			}
			
		names(storage) <- names(data)

	storage

} else {
	
	if (type > 1) {acp <- princomp} else {acp <- prcomp}
	
		for (i in 1:dim(data)[2]) {
	
			pca <- acp(data[,i,1:dim(data)[3]],scale=logico)
	
		storage <- c(storage,list(pca))
	
		}

	nombres <- names(data) ; names(data) <- nombres[[1]]

	storage

	}

}

#########################################################################
#### DISTANCIA ENTRE COORDENADAS ########################################
#########################################################################
#### d(P1, P2) = √(data2 - data1)^2 + (Y2 - Y1)^2 #############################
#########################################################################


COPO <- function (data) {

nombres <- dimnames(data)[3] ; nombres <- nombres[[1]]
storage_p <- c()


for (k in 1:dim(data)[3]) {
	
		P <- function (data,m1,m2) {
	
			M <- matrix(NA,dim(data)[1],2)
	
			storage1 <- c()

				for (i in 1:dim(data)[1]) {
	
	
					dataP <- (data[i,1,m1] - data[i,1,m2])^2

					YP <- (data[i,2,m1] - data[i,2,m2])^2

					D <- sqrt(dataP + YP) 


			storage1 <- c(storage1,D)

					}

			storage1

		}

storage_p <- cbind(storage_p,P(data,1:dim(data)[3],k))	

	}

storage_p <- array(t(storage_p),c(c(dim(data)[3],dim(data)[3],dim(data)[1])))

dimnames(storage_p)=list(nombres,nombres,NULL)

storage_p

}


########################################################################
### AREA UNDER FUNCTION - sin auc() ###################################
########################################################################
AREA.FUN <- function(array, number) {
  if (!requireNamespace("pracma", quietly = TRUE)) {
    install.packages("pracma")
  }
  library(pracma)
  
  if (is.list(array)) {
    individuo <- names(array)
    plot(array[[number]][,1], array[[number]][,2], main = individuo[number])
    
    cord.data <- c(min(array[[number]][,1]), array[[number]][,1], max(array[[number]][,1]))
    cord.y <- c(0, array[[number]][,2], 0)
    polygon(cord.data, cord.y, col = "grey")
    
    area <- trapz(array[[number]][,1], array[[number]][,2])
    
  } else {
    individuo <- dimnames(array)[[3]][1]
    plot(array[,1,number], array[,2,number], main = individuo[number])
    
    cord.data <- c(min(array[,1,number]), array[,1,number], max(array[,1,number]))
    cord.y <- c(0, array[,2,number], 0)
    polygon(cord.data, cord.y, col = "grey")
    
    area <- trapz(array[,1,number], array[,2,number])
  }
  
  return(area)
}

#########################################################################
