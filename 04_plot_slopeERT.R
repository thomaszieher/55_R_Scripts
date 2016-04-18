require(spgrass6)
require(plotrix)
require(RColorBrewer)

##set parent directory
rootPath="F:\\55_check_ert"#dirname(getwd())
grassPath=paste(rootPath,"01_DATA","grass",sep="\\")
setwd(rootPath)


##FUNCTIONS
getLog=function(rast){
  logRast=rast
  logRast@data=log10(rast@data)
  return(logRast)
}

##set GRASS environment
initGRASS(gisBase="C:\\QGIS\\apps\\grass\\grass-6.4.3",
          home=tempdir(),
          gisDbase=grassPath,
          location="Laterns",
          mapset="ERT_slopebonacker",
          override=T)


##import first inversion model
slopeProfile=readRAST6("ert_11_00_00_01")

##import position of sondes
sondPos=readVECT6("sondes_slope")

##import mask
ertMask=readVECT6("mask")

##import monitoring ert mask
monitoringMask=readVECT6("ert_monitoring")


##PLOT SLOPE PROFILE
plot(NA,NA,xlim=c(-650,550),ylim=c(-300,300),axes=F,xlab="",ylab="",asp=1,pty="l")
axis(side=1,at=seq(-650,550,by=200),labels=seq(0,1200,by=200)/10,pos=-300)
mtext(side=1,line=2,"Relative distance [m]")
axis(side=2,at=seq(-300,300,by=100),labels=seq(0,600,by=100)/10,las=1,pos=-650)
mtext(side=2,line=1,"Relative elevation [m]",las=0)

##original values
#image(slopeProfile,add=T,col=terrain.colors(20),zlim=c(50,2500))
#mtext(side=4,"Resistivity [Ohm m]",las=0,line=3)

##log values
#cols=terrain.colors(20)
cols=rev(colorRampPalette(brewer.pal(9,"RdBu"))(20))
col.labels=seq(1.5,3.5,0.5)
image(getLog(slopeProfile),add=T,col=cols,zlim=c(min(col.labels),max(col.labels)))
mtext(side=4,"LOG10(Resistivity [Ohm m])",las=0,line=0)
plot(monitoringMask,add=T)
plot(ertMask,add=T)
points(sondPos,pch=3,cex=0.5)

##add legend
color.legend(600,-240,650,200,col.labels,cols,align="rb",gradient="y")




##PLOT MONITORING PROFILE
##set GRASS environment
initGRASS(gisBase="C:\\QGIS\\apps\\grass\\grass-6.4.3",
          home=tempdir(),
          gisDbase=grassPath,
          location="Laterns",
          mapset="ERT_monbonacker",
          override=T)

##import mean resistivity of first five measurements
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
                ##log10
                data=log10(rast@data)
                ##original values
                #data=rast@data
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
monProfile=as.Raster(readRAST6("before_mean"))
sondPosMon=readVECT6("sond_pos")
dcpt=readVECT6("dcpt_points")

plot(NA,NA,xlim=c(0,250),ylim=c(-150,0),axes=F,xlab="",ylab="",asp=1,pty="l")
axis(side=1,at=seq(0,250,by=50),labels=seq(0,250,by=50)/10,pos=-150)
mtext(side=1,line=2,"Relative distance [m]")
axis(side=2,at=seq(-150,0,by=50),labels=seq(0,150,by=50)/10,las=1,pos=0)
mtext(side=2,line=1,"Relative elevation [m]",las=0)

##original values
#image(monProfile,add=T,col=terrain.colors(20),zlim=c(50,2500))
#mtext(side=4,"Resistivity [Ohm m]",las=0,line=3)

##log values
col.labels=seq(1.5,3.5,0.5)
image(monProfile,add=T,col=cols,zlim=c(min(col.labels),max(col.labels)))
mtext(side=4,"LOG10(Resistivity [Ohm m])",las=0,line=0)
plot(monitoringMask,add=T)
points(sondPosMon,pch=3,cex=0.7)
points(dcpt,pch=20,cex=0.7)

##add legend
color.legend(260,-150,280,-50,col.labels,cols,align="rb",gradient="y")
