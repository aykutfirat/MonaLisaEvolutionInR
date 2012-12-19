#Mona Lisa GA version

getDistance<-function(plotRaster){
   sum(abs(col2rgb(plotRaster)-col2rgb(targetRaster)))
}     

printPlot<-function(datapoly){
	g <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(red=red,green=green, blue=blue, alpha=alpha, fill=rgb(r=red,g=green,b=blue,max=255), group=id)) + scale_fill_identity()+coord_equal()+theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.ticks = element_blank(),axis.text.x = element_blank(),
                    axis.text.y = element_blank(),legend.position = "none", panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill="black"))
	print(g)
}

getRaster<-function(datapoly,rotationCounter){
	fName<-paste("plot",rotationCounter,".jpg",sep="")
	jpeg(fName,200,200)
	printPlot(datapoly)
	dev.off()
	img<-read.jpeg(fName)
	as.raster(img)
}

fitness<-function(dna){
	n=5
	ids <-dna[1:50] #sample(1:50,50)
	x<-dna[51:300] #sample(1:200, 50*n, replace=TRUE)
	y<-dna[301:550] #sample(1:200,50*n,replace=TRUE)
	red<-dna[551:600] #sample(1:255,50,replace=TRUE)
	green<-dna[601:650] #sample(1:255,50,replace=TRUE)
	blue<-dna[651:700] #sample(1:255,50,replace=TRUE)
	alpha<-dna[701:750] #sample(1:255,50,replace=TRUE)
	
	datapoly<-matrix(0,250,7)
	colnames(datapoly)<-c("x","y","id","red","green","blue","alpha")
	datapoly[,1]<-x
	datapoly[,2]<-y
	datapoly[,3]<-ids
	datapoly[,4]<-red
	datapoly[,5]<-green
	datapoly[,6]<-blue
	datapoly[,7]<-alpha/255
	datapoly<-data.frame(datapoly)
	rotationCounter<<-(rotationCounter %%20)+1
	getDistance(getRaster(datapoly,rotationCounter))
}


n=5
ids <-sample(1:50,50)
x<-sample(1:200, 50*n, replace=TRUE)
y<-sample(1:200,50*n,replace=TRUE)
red<-sample(1:255,50,replace=TRUE)
green<-sample(1:255,50,replace=TRUE)
blue<-sample(1:255,50,replace=TRUE)
alpha<-sample(1:255,50,replace=TRUE)
dna<-c(ids,x,y,red,green,blue,alpha)
datapoly<-matrix(0,50*n,7)
colnames(datapoly)<-c("x","y","id","red","green","blue","alpha")
datapoly[,1]<-x
datapoly[,2]<-y
datapoly[,3]<-ids
datapoly[,4]<-red
datapoly[,5]<-green
datapoly[,6]<-blue
datapoly[,7]<-alpha

datapoly<-data.frame(datapoly)
bounds<-matrix(0,750,2)
bounds[,1]<-c(rep(0,50), rep(0,500), rep(0,150), rep(30,50))
bounds[,2]<-c(rep(10000,50), rep(200,500), rep(255,150), rep(60,50))

library(rgenoud)
library(ReadImages)
library(ggplot2)
ml<-read.jpeg("ml.jpg")
targetRaster<-as.raster(ml)
rotationCounter<-0

ml<-genoud(fitness, nvars=750, pop.size=100, max.generations=100000,wait.generations=1000, starting.values=dna,print.level=1, Domains=bounds, boundary.enforcement=2, data.type.int=TRUE, max=FALSE)
