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
          mapset="ERT_validation",
          override=T)


##import first inversion model
slopeProfile=readRAST6("ert__01")

##import position of sondes
sondPos=readVECT6("sond_pos")

##import mask
ertMask=readVECT6("mask")

str(slopeProfile)

##PLOT SLOPE PROFILE
plot(NA,NA,xlim=c(0,240),ylim=c(-160,0),axes=F,xlab="",ylab="",asp=1,pty="l")
axis(side=1,at=seq(0,240,by=40),labels=seq(0,240,by=40)/10,pos=-160)
mtext(side=1,line=2,"Relative distance [m]")
axis(side=2,at=seq(-160,0,by=40),labels=seq(0,160,by=40)/10,las=1,pos=0)
mtext(side=2,line=1,"Relative elevation [m]",las=0)

##original values
#image(slopeProfile,add=T,col=terrain.colors(20),zlim=c(50,2500))
#mtext(side=4,"Resistivity [Ohm m]",las=0,line=3)

##log values
#cols=terrain.colors(20)
cols=rev(colorRampPalette(brewer.pal(9,"RdBu"))(20))
col.labels=seq(1.5,3.5,0.5)
prof=getLog(slopeProfile)
prof@data[prof@data<1.5]=1.5
image(prof,add=T,col=cols,zlim=c(min(col.labels),max(col.labels)))
mtext(side=4,"LOG10(Resistivity [Ohm m])",las=0,line=0)
plot(monitoringMask,add=T)
plot(ertMask,add=T)
points(sondPos,pch=3,cex=0.5)


##add legend
color.legend(250,-160,280,0,col.labels,cols,align="rb",gradient="y")

