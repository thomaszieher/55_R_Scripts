###Saturation over time and depth
Linear interpolation between recording times and Depth  
Measurements smoothed by a defined window of time (40 min, )
```{r echo=F,message=F,warning=F}
from=2
to=20
colTDR=colorRampPalette(rev(brewer.pal(10,"RdYlGn")))(to)
plot(NA,NA,type="l",xlim=c(32,42),ylim=c(100,0),xlab="Saturation [Vol%]",ylab="Depth [m]")
for (i in from:to){
  lines(t(colMeans(dataProbes[i,seq(1,ncol(dataProbes),by=3)])),probes$depth,col=colTDR[i])
}
legend("bottomleft",legend=dataProbes[,2][from:to],col=colTDR,lty=1,cex=0.7)


##convert to spatio-temporal raster
##downscale to seconds and merge
##get start and end time
minTime=min(apply(dataProbes[grep("time",names(dataProbes))],2,min))
maxTime=max(apply(dataProbes[grep("time",names(dataProbes))],2,max))

sptempTDR=data.frame(time=seq(minTime,maxTime,by=1))
#probe="16"
for (probe in probes$nr){
  sptempTDR=merge(sptempTDR,dataProbes[c(sprintf("time_%s",probe),sprintf("vol_%s",probe))],by.x="time",by.y=sprintf("time_%s",probe),all.x=T)
}



##temporal linear interpolation
require(zoo)
##na.approx from zoo package
sptemp=na.omit(na.approx(sptempTDR))
#matplot(sptemp[,2:4],type="l")

##filter data.frame
filterWindow=2400 ##seconds!!
movingAverage=function(x,n){filter(x,rep(1/n,n),sides=1)}
sptemp.filter=na.omit(data.frame(time=sptemp[,1],apply(sptemp[,2:4],2,movingAverage,n=filterWindow)))

##linear interpolation with depth
##extrapolation ok??
transponded=data.frame(t(sptemp.filter))[-1,]
transponded["depth"]=probes$depth

##set up spatio-temporal raster
minDepth=0
maxDepth=100
increment=5
depthTDR=data.frame(depth=seq(minDepth,maxDepth,by=increment))
depthTDR=merge(depthTDR,transponded,by="depth",all.x=T)##[,33239:33339]
depthTDR=data.frame(na.approx(depthTDR,rule=1,na.rm=F))##rule=2 for extrapolation

##convert to matrix
TDRdata=as.matrix(depthTDR[,-depth])
TDRtime=data.frame(time=as.POSIXct((as.numeric(as.POSIXct(strptime(measureday,format="%Y-%m-%d %H:%M:%S")))+12*60*60)+as.data.frame(sptempTDR)$time,origin="1970-01-01"))
TDRtime["seconds"]=as.data.frame(sptempTDR)$time
##cut off filtered ends (NA)
TDRtime=TDRtime[-(1:filterWindow-1),]
TDRtime=TDRtime[-((nrow(TDRtime)-(filterWindow-1)):nrow(TDRtime)),]

##temporal subset
##set mindate and maxdate (like above)
mindate=as.POSIXct(strptime("2014-07-24 13:00:00",format="%Y-%m-%d %H:%M:%S"))
maxdate=as.POSIXct(strptime("2014-07-24 16:00:00",format="%Y-%m-%d %H:%M:%S"))
TDRdata.sub=TDRdata[,(TDRtime$time>=mindate)&(TDRtime$time<=maxdate)]

##rasterize matrix
TDRdatarast=raster(TDRdata.sub)
##set extent (time relative to measureday)
minTime.sec=TDRtime$seconds[TDRtime$time==mindate]
maxTime.sec=TDRtime$seconds[TDRtime$time==maxdate]
extent(TDRdatarast)=c(minTime.sec,maxTime.sec,minDepth-increment/2,maxDepth+increment/2)

##plot
par(xaxs="i",yaxs="i")
plot(NA,NA,xlim=c(minTime.sec,maxTime.sec),ylim=c(minDepth,maxDepth),axes=F,ylab="",xlab="",asp=(maxTime.sec-minTime.sec)/(maxDepth-minDepth))
timeTicks=seq(minTime.sec,maxTime.sec,by=1800)
timeLabels=format(TDRtime$time[TDRtime$seconds%in%timeTicks],format="%H:%M")
axis(side=1,at=timeTicks,labels=timeLabels)
mtext(side=1,line=3,"Time")
axis(side=2,at=seq(maxDepth,minDepth,by=-(increment*10)),labels=seq(minDepth/100,maxDepth/100,by=increment/10),las=1)
mtext(side=2,line=3,"Depth [m]",las=0)
mtext(side=4,line=2.5,"Water content [Vol%]",las=0)
plot(TDRdatarast,col=rev(colors),add=T,zlim=c(32,40))


##plot measurements as points
##cut off NAs caused by filtering
measurePoints=sptempTDR[-(1:filterWindow-1),]
measurePoints=measurePoints[-((nrow(measurePoints)-(filterWindow-1)):nrow(measurePoints)),]

##plot measurement times per depth
depth1=na.omit(data.frame(TDRtime,measurePoints[,2]))
points(x=depth1$seconds,y=rep(maxDepth-20,length(depth1$seconds)),pch=20)##raster built in reverserd y
depth2=na.omit(data.frame(TDRtime,measurePoints[,3]))
points(x=depth2$seconds,y=rep(maxDepth-60,length(depth2$seconds)),pch=20)##raster built in reverserd y
depth3=na.omit(data.frame(TDRtime,measurePoints[,4]))
points(x=depth3$seconds,y=rep(maxDepth-75,length(depth3$seconds)),pch=20)##raster built in reverserd y
```
