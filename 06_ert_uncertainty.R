require(spgrass6)
require(raster)
require(RColorBrewer)

##set parent directory
rootPath="F:\\55_check_ert"
grassPath=paste(rootPath,"01_DATA","grass",sep="\\")

setwd(grassPath)

##############################################################################
##settings
##############################################################################
location="Laterns"
mapset="ERT_monbonacker"
##specify irrigation plot measures
maximum.depth=20
plot.min=31
plot.max=196
##############################################################################


##FUNCTIONS
roundUp=function(x,to){
  to*(x%/%to+as.logical(x%%to))
}

roundDown=function(x,to){
  to*(x%/%to)
}

getDiff=function(rastT1,rastT2){
  diff=rastT1
  diff@data@values=rastT2@data@values-rastT1@data@values
  return(diff)
}

getRatio=function(rastT1,rastT2){
  ratio=rastT1
  ratio@data@values=rastT2@data@values/rastT1@data@values
  return(ratio)
}

as.Raster=function(rast){
  ##convert spatial data frame to raster
  ncols=rast@grid@cells.dim[1]
  nrows=rast@grid@cells.dim[2]
  offsetX=rast@grid@cellcentre.offset[1]
  offsetY=rast@grid@cellcentre.offset[2]
  proj4=rast@proj4string
  ##create coordinates for new raster
  xCor=((rep(seq(1,ncols,by=1),times=nrows)-1)*cellsize)-offsetX
  yCor=(((rep(1:nrows,each=ncols)-1)*-1)*cellsize)-offsetY
  ##fill data frame
  df=data.frame(x=xCor,
                y=yCor,
                data=rast@data
  )
  ##set coordinates
  coordinates(df)=~x+y
  ##grid it
  gridded(df)=T
  ##convert data frame to raster
  converted=raster(df)
  ##set projection
  crs(converted)=proj4
  return(converted)
}


##set GRASS environment
initGRASS(gisBase="C:\\QGIS\\apps\\grass\\grass-6.4.3",
          home=tempdir(),
          gisDbase=grassPath,
          location=location,
          mapset=mapset,
          override=T)

##get list of raster data sets
rlist=execGRASS("g.mlist",parameters=list(type="rast",pattern="ert_*"),intern=T)
timelist=data.frame(measurements=rlist)
timelist["time"]=sprintf("%s:%s",substr(timelist$measurements,5,6),substr(timelist$measurements,8,9))

##set colors
colorsERT=colorRampPalette(rev(brewer.pal(9,"PuBu")))(100)
colors=rev(colorRampPalette(brewer.pal(9,"RdBu"))(100))
#colors=rev(colorRampPalette(brewer.pal(9,"Spectral"))(10))

##read first raster
rast0=readRAST6(rlist[2])
par(las=1,xaxs="i",yaxs="i",cex=1.2)
#hist(rast0@data[,1],breaks=100,xlab="Resistivity [Ohm m]")
##get nrows, ncols and cellsize of spatial data frame
ncols=rast0@grid@cells.dim[1]
nrows=rast0@grid@cells.dim[2]
cellsize=rast0@grid@cellsize[1]

##convert to raster (format of raster package)
rast0=as.Raster(rast0)

##import position of ERT probes
probesERT=readVECT6("sond_pos")

##import irrigation plot
irr_plot=readVECT6("irrig_plot")

##import monitoring ert mask
monitoringMask=readVECT6("ert_monitoring")

##read sensor positions
sondPosMon=readVECT6("sond_pos")

##import dcpt data
dcpt.vect=readVECT6(vname="dcpt_points")
##calculate resistivity
##blow calculation (sonding settings)
m=10.0 #[kg]
A=5.0/10000.0 #[m2]
g=9.80665 #[m/s2]
h=0.50 #[m]
dcpt.vect@data["energy"]=dcpt.vect@data*m*h*g/A/1000.0

##build depth mask
depth.mask=(as.matrix(rast0))
dcpt.mask=(as.matrix(rast0))
length.mask=(as.matrix(rast0))

##summarize pixel cellsize in y-direction (depth-array)
##and create DCPT-depth mask
depthMax=0
count=1
for (col in seq(1,ncols,by=1)){
  depth=cellsize
  #count=1
  for (row in seq(1,nrows,by=1)){
    if (!is.nan(depth.mask[row,col])){
      depth.mask[row,col]=round(depth,2)
      dcpt.mask[row,col]=count
      length.mask[row,col]=col
      if (round(depth,2)%%1==0){
        count=count+1
      }
      if (depth>depthMax){
        depthMax=depth
      }
      depth=depth+cellsize
    }
  }
  count=count+1
}


###Value range of first five datasets
##First five measurements conducted before the beginning of irrigation experiment  
##Range assumed to represent the repeatability and uncertainties of the measurements  
##Generally higher uncertainties near the surface
##measurement uncertainties of material properties before irrigation (value range)
#execGRASS("r.series",input=rlist[2:6],output="before_range",method="range",flag="overwrite")
rangeERT=as.Raster(readRAST6("before_range"))
##boxplot
#boxplot(rangeERT@data@values)
##ecdf
#plot(ecdf(rangeERT@data@values))
#abline(v=c(mean(rangeERT@data@values,na.rm=T)),lty=2,col="red",lwd=2)
#hist(rangeERT@data@values,breaks=100)
par(mfrow=c(1,1),mar=c(5,5,5,5),xaxs="i",yaxs="i")
plot(NA,NA,xlim=c(0,250),ylim=c(-150,0),axes=F,xlab="",ylab="",asp=1)
axis(side=1,at=seq(0,250,by=50),labels=seq(0,250,by=50)/10)
mtext(side=1,line=2.5,"Relative distance [m]")
axis(side=2,at=seq(-150,0,by=50),labels=seq(0,150,by=50)/10,las=1)
mtext(side=2,line=2.5,"Relative elevation [m]",las=0)
mtext(side=4,"Range [Ohm m]",las=0,line=3.5)
plot(rangeERT,add=T,axes=F,box=F)
plot(monitoringMask,add=T)
##points(sondPosMon,pch=3,cex=0.7)




##material properties before irrigation (mean value)
#execGRASS("r.series",input=rlist[2:6],output="before_mean",method="average",flag="overwrite")
material.before=as.Raster(readRAST6("before_mean"))
##boxplot
#boxplot(material.before@data@values)
##ecdf
#plot(ecdf(material.before@data@values))
#mean(material.before@data@values,na.rm=T)
#abline(v=c(mean(material.before@data@values,na.rm=T)),lty=2,col="red",lwd=2)
#hist(material.before@data@values,breaks=100)
#plot(material.before,main="")
#points(dcpt.vect,pch=20,cex=0.5)
#points(probesERT,pch=3,cex=0.5)


##Relative deviation from mean  
##plot spatial uncertainties
par(mfrow=c(1,1),mar=c(5,5,5,5),oma=c(0,0,0,0),xaxs="i",yaxs="i")
plot(NA,NA,xlim=c(0,250),ylim=c(-150,0),axes=F,xlab="",ylab="",asp=1)
axis(side=1,at=seq(0,250,by=50),labels=seq(0,250,by=50)/10)
mtext(side=1,line=2.5,"Relative distance [m]")
axis(side=2,at=seq(-150,0,by=50),labels=seq(0,150,by=50)/10,las=1)
mtext(side=2,line=2.5,"Relative elevation [m]",las=0)
mtext(side=4,"Relative deviation [%]",las=0,line=3)
plot((rangeERT/material.before)*100,axes=F,box=F,add=T,asp=1,zlim=c(0,4.5))
plot(monitoringMask,add=T)
##points(sondPosMon,pch=3,cex=0.7)


##Mean deviation with depth (relative to mean of first five measurements)  
##relative proportion of range for differences
start=2
end=6
xMin=-0.5
xMax=0.5
cols.uncert=data.frame(run=start:end,col=colorRampPalette(rev(brewer.pal(5,"Set2")))(end-start+1),stringsAsFactors=F)
##plot
par(mfrow=c(1,1),xaxs="i",yaxs="i")
plot(NA,NA,type="l",col="red",ylim=c(47,0),xlim=c(xMin,xMax),axes=F,xlab="",ylab="")
axis(side=1,at=seq(xMin,xMax,by=0.25))
mtext(side=1,"Mean deviation [%]",line=2.5)
axis(side=2,at=seq(0,50,by=10),labels=seq(0,50,by=10)/10,las=1)
mtext(side=2,"Depth [m]",line=2.5,las=0)
abline(v=seq(xMin+1,xMax,by=1),lty=2,lwd=0.2,col="gray70")
abline(h=seq(5,50,by=5),lty=2,lwd=0.2,col="gray70")
for (i in seq(start,end,by=1)){
  #print(i)
  #print(rlist[i])
  rast1=as.Raster(readRAST6(rlist[i]))
  uncertaintyDiff=data.frame(uncert=((rast1/material.before)@data@values-1)*100)
  uncertaintyDiff["depth"]=as.data.frame(raster(depth.mask))
  ##calculate mean
  uncertaintyStat=aggregate(uncertaintyDiff,list(uncertaintyDiff$depth),FUN=mean,na.rm=T)
  ##plot
  lines(uncertaintyStat$uncert,uncertaintyStat$depth-0.5,col=cols.uncert$col[cols.uncert$run==i])
  #points(uncertaintyStat$uncert,uncertaintyStat$depth,pch=1,col="red")
}
legend("bottomright",legend=timelist$time[start:end],col=cols.uncert$col,lty=1)