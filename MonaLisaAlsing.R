#Version based on Alsing's code with few modifications

mutate<-function(x,...) UseMethod("mutate")

mutate.default<-function(x,...) {print("should not be here") }

dnaPoint.init <-function(x, y){
  point<-list(x=x,y=y)
	class(point)<-"dnaPoint"
	point
}

dnaPoint.random<-function(){
	dnaPoint.init(sample(minX:maxX,1),sample(minY:maxY,1))
}

mutate.dnaPoint<-function(point){

	if (sample(1:maxMutationFrequency,1)==1) { 
				point$x<-sample(minX:maxX, 1) 	 
	            point$y<-sample(minY:maxY, 1) 
	}
	if (sample(1:midMutationFrequency,1)==1) { 
				point$x<-min(max(minX, point$x+sample((-1*movePointRangeMid):movePointRangeMid, 1)),maxX)
	 	        point$y<-min(max(minY, point$y+sample((-1*movePointRangeMid):movePointRangeMid, 1)),maxY) 
	}
	if (sample(1:minMutationFrequency,1)==1) { 
				point$x<-min(max(minX, point$x+sample((-1*movePointRangeMin):movePointRangeMin, 1)),maxX)
	 	        point$y<-min(max(minY, point$y+sample((-1*movePointRangeMin):movePointRangeMin, 1)),maxY) 
	}
    point
}      

repmat<-function(x,n) matrix(rep(x,n),nrow=n,byrow=T)

dnaPolygon.init <- function(n=3, brushColor=-1){
	 polygon<-list()
	 points <- vector('list',n)
	 #xlist<-c(0,100,200,200,200,0)
	 #ylist<-c(0,0,0,100,200,200)
	
	 for (i in 1:n){
	 	points[[i]]<-dnaPoint.random() 
	 	#points[[i]]<-dnaPoint.init(x=min(max(minX, origin$x+sample(-3:3,1)), maxX), y=min(max(minY, origin$y+sample(-3:3,1)), maxY))
	 	#points[[i]]<-dnaPoint.init(xlist[i],ylist[i])
	 }	
	 polygon$points<-points
	 polygon$brush<-dnaBrush.init(brushColor)
	 polygon$id<-sample(1:100000,1)
	 class(polygon)<-"dnaPolygon"
	 polygon
}
addPoint<-function(dnaPolygon, dnaDrawing){
        if (length(dnaPolygon$points)<pointsPerPolygonMax)
           if (pointCount(dnaDrawing) < pointsMax){
				    index = sample(2:length(dnaPolygon$points),1)
                    prevp = dnaPolygon$points[[index - 1]]
                    nextp = dnaPolygon$points[[index]]
                    if (runif(1)<0.5) { x=(prevp$x + nextp$x)/2; y= sample(c(prevp$y,nextp$y),1)}
                    else { y=(prevp$y + nextp$y)/2; x= sample(c(prevp$x,nextp$x),1)}
                    newPoint=dnaPoint.init(x,y)
                    newPoints<-c(dnaPolygon$points[1:(index-1)],list(newPoint),dnaPolygon$points[index:length(dnaPolygon$points)])
                    dnaPolygon$points<-newPoints
            }
            dnaPolygon
}

addPoint2<-function(dnaPolygon, dnaDrawing){
        if (length(dnaPolygon$points)<pointsPerPolygonMax)
           if (pointCount(dnaDrawing) < pointsMax){
				    index = sample(2:length(dnaPolygon$points),1)
                    prevp = dnaPolygon$points[[index - 1]]
                    nextp = dnaPolygon$points[[index]]
                    x=(prevp$x + nextp$x)/2;
                    y=(prevp$y + nextp$y)/2;
                    newPoint=dnaPoint.init(x,y)
                    newPoints<-c(dnaPolygon$points[1:(index-1)],list(newPoint),dnaPolygon$points[index:length(dnaPolygon$points)])
                    dnaPolygon$points<-newPoints
            }
            dnaPolygon
}

removePoint<-function(dnaPolygon, dnaDrawing){
        if (length(dnaPolygon$points)>pointsPerPolygonMin)
           if (pointCount(dnaDrawing) > pointsMin){
				    index = sample(1:length(dnaPolygon$points),1)
                    dnaPolygon$points[[index]]<-NULL
            }
            dnaPolygon
}


mutate.dnaPolygon<-function(dnaPolygon,dnaDrawing){ 
	  if (sample(1:addPointMutationRate,1)==1)
					dnaPolygon<-addPoint(dnaPolygon,dnaDrawing)
	  if (sample(1:removePointMutationRate,1)==1)
					dnaPolygon<-removePoint(dnaPolygon,dnaDrawing)
      for (i in 1:length(dnaPolygon$points)) { 
         dnaPolygon$points[[i]]<-mutate(dnaPolygon$points[[i]])
      }
      dnaPolygon$brush<-mutate(dnaPolygon$brush) 
      dnaPolygon   
}

dnaDrawing.init<-function(n=3, minPolygons=1, brushColor=-1){
	dnaDrawing<-list()
	class(dnaDrawing)<-"dnaDrawing"
	if (minPolygons>0)
	for (i in 1:minPolygons) dnaDrawing<-addPolygon(dnaDrawing,n,brushColor)
	dnaDrawing
}
   
pointCount<-function(drawing){
	sum(unlist(lapply(drawing,function(x) length(x$points))))
}   

addPolygon<-function(dnaDrawing,n,brushColor=-1){
	polygon<-dnaPolygon.init(n, brushColor)
	if (length(dnaDrawing) < maxPolygons) dnaDrawing[[length(dnaDrawing)+1]]<-polygon  
	dnaDrawing      
}

removePolygon<-function(dnaDrawing){
	if (length(dnaDrawing) > minPolygons)  dnaDrawing[[sample(1:length(dnaDrawing),1)]]<-NULL
	dnaDrawing      
}

movePolygon<-function(dnaDrawing){
	if (length(dnaDrawing) <= 1)  return(dnaDrawing)
	index = sample(1:length(dnaDrawing),1)
    polygon = dnaDrawing[[index]]
    dnaDrawing[[index]]<-NULL
    index2<-sample(1:length(dnaDrawing),1)
    dnaDrawing<-c(dnaDrawing[1:(index2-1)], list(polygon), dnaDrawing[index2:length(dnaDrawing)])
    class(dnaDrawing)<-"dnaDrawing"
    dnaDrawing
}
 

mutate.dnaDrawing<-function(dnaDrawing,n=6){

	if (sample(1:addPolygonMutationFrequency,1)==1){
		dnaDrawing<-addPolygon(dnaDrawing,n)
	} 
	if (length(dnaDrawing)>1 & sample(1:removePolygonMutationFrequency,1)==1){
		dnaDrawing<-removePolygon(dnaDrawing)
	} 
	if (length(dnaDrawing)>1 & sample(1:movePolygonMutationFrequency,1)==1){
		dnaDrawing<-movePolygon(dnaDrawing)
	} 
	if(length(dnaDrawing)>0)
	for (i in 1:length(dnaDrawing)){
		dnaDrawing[[i]]<-mutate(dnaDrawing[[i]],dnaDrawing)
	}
	dnaDrawing
}


dnaBrush.init<-function(brushColor=-1){

   dnaBrush<-list()
   dnaBrush$red=ifelse(brushColor>-1, brushColor, sample(0:255,1))
   dnaBrush$green=ifelse(brushColor>-1, brushColor, sample(0:255,1))
   dnaBrush$blue=ifelse(brushColor>-1, brushColor, sample(0:255,1))
   dnaBrush$alpha=ifelse(brushColor>-1, 1, sample(alphaRangeMin:alphaRangeMax,1)/255)
   class(dnaBrush)<-"dnaBrush"
   dnaBrush
}

mutate.dnaBrush<-function(dnaBrush){

    if (sample(1:redMutationFrequency,1)==1){
		dnaBrush$red<-sample(redRangeMin:redRangeMax,1)
	} 
    if (sample(1:greenMutationFrequency,1)==1){
		dnaBrush$green<-sample(greenRangeMin:greenRangeMax,1)
	}
	if (sample(1:blueMutationFrequency,1)==1){
		dnaBrush$blue<-sample(blueRangeMin:blueRangeMax,1)
	}
	if (sample(1:alphaMutationFrequency,1)==1){
		dnaBrush$alpha<-sample(alphaRangeMin:alphaRangeMax,1)/255
	}
	dnaBrush
}

drawingToDF<-function(drawing){
   df<-data.frame(do.call('rbind',lapply(drawing,function(polygon)  {m<-matrix(unlist(lapply(polygon$points,function(point) c(point$x,point$y))),ncol=2); cbind(m,repmat(c(polygon$id,polygon$brush$red,polygon$brush$green, polygon$brush$blue, polygon$brush$alpha),nrow(m)))})))
   colnames(df)<-c("x","y","id","red","green","blue","alpha")
   df
}

getFitness<-function(plotRaster,targetRaster){
	 1-sum(abs(col2rgb(plotRaster)-col2rgb(targetRaster)))/30600000
}     

printPlot<-function(datapoly){
	g <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(red=red,green=green, blue=blue, alpha=alpha, fill=rgb(r=red,g=green,b=blue,max=255), group=id)) +  xlim(0, 200)+ ylim(0, 200)+scale_fill_identity()+coord_equal()+theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.ticks = element_blank(),axis.text.x = element_blank(),
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

library(ReadImages)
library(ggplot2)
ml<-read.jpeg("ml.jpg")
targetRaster<-as.raster(ml)

minX=0
maxX=200 
minY=0
maxY=200

maxMutationFrequency=300
midMutationFrequency=1500
minMutationFrequency=1500
removePointMutationRate=100
addPointMutationRate=100
movePointRangeMid = 20
movePointRangeMin=3
pointsPerPolygonMax=10 
pointsMax=1500
pointsPerPolygonMin=3
pointsMin=0
maxPolygons=50
minPolygons=50
addPolygonMutationFrequency=700
removePolygonMutationFrequency=1500
movePolygonMutationFrequency=700
redMutationFrequency=greenMutationFrequency=blueMutationFrequency=alphaMutationFrequency=20
redRangeMax=greenRangeMax=blueRangeMax=255
redRangeMin=greenRangeMin=blueRangeMin=0
alphaRangeMin=30 
alphaRangeMax=60

drawing = dnaDrawing.init(n=6, minPolygons=50, brushColor=0)

datapoly<-drawingToDF(drawing)
printPlot(datapoly)
plotRaster<-getRaster(datapoly,1)
prevFitness<- getFitness(plotRaster, targetRaster)
rotationCounter<-generation<-1
numberOfIterations<-100000

for (i in 1:numberOfIterations){
maxMutationFrequency=1500
midMutationFrequency=1500
minMutationFrequency=1500
coin<-runif(1)
if (coin<0.33) maxMutationFrequency=30 else if (coin<0.67) midMutationFrequency=30 else minMutationFrequency=30
   newDrawing=mutate(drawing)
   datapoly<-drawingToDF(newDrawing)
   plotRaster<-getRaster(datapoly,rotationCounter)
   newFitness<-getFitness(plotRaster,targetRaster)
   rotationCounter<-(rotationCounter %% 20) +1
   if (newFitness>=prevFitness){
        print("newFitness")
   		prevFitness<-newFitness
   		drawing<-newDrawing
   		print(newFitness)
   }
   print(generation)
   generation<-generation+1
} 

  
