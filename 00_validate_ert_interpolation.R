require(spgrass6)
require(rgdal)
require(foreign)

rootPath="F:\\55_check_ert"
##filesPath="F:\\55_check_ert\\01_DATA\\Geoelektrik_Auswertung\\Bonacker\\2014_07_24_gp_monitor30_bonacker"
filesPath="F:\\55_check_ert\\01_DATA\\Geoelektrik_Auswertung\\Bonacker\\2014_07_24_gp_lang120_bonacker"
##filesPath="F:\\55_check_ert\\01_DATA\\Geoelektrik_Auswertung\\Bonacker\\2014_07_24_gp_validierung30_bonacker"
grassPath="F:\\55_check_ert\\01_DATA\\grass"
setwd(rootPath)

##FUNCTIONS
roundUp=function(x,to){
  to*(x%/%to+as.logical(x%%to))
}
roundDown=function(x,to){
  to*(x%/%to)
}

##settings
location="Laterns"

##monitoring profile
# fileIn="24-07-2014_13-25-00.DAT"
# maskFile="bonacker_monitoring_mask.shp"
# resolution="1" ##1 dm
# mapset="ERT_bonacker_interpolation"

##monitoring profile
# fileIn="24-07-2014_13-10-00.DAT"
# maskFile="bonacker_validation_mask.shp"
# resolution="5" ##1 dm
# mapset="ERT_validation"

##slope profile
fileIn="bf2_lang_120m_2014-07-24_11-00-00_Messung1_aligned.DAT"
maskFile="bonacker_slope_mask.shp"
resolution="1"
mapset="ERT_slope_bonacker"

##set GRASS environment
initGRASS(gisBase="C:\\QGIS\\apps\\grass\\grass-6.4.3",
          home=tempdir(),
          gisDbase=grassPath,
          location=location,
          mapset=mapset,
          override=T)

##import mask shape
execGRASS("v.in.ogr",dsn=sprintf("%s\\%s",rootPath,maskFile),output="mask",flags=c("overwrite"))

count=1
##import data
execGRASS("v.in.ascii",input=sprintf("%s\\%s",filesPath,fileIn),output=sprintf("ert_%0.2i",count),fs=";",x=1,y=2,columns="x double precision, y double precision, resistiv double precision, log_resi double precision",flags=c("quiet","overwrite"))

##set region
execGRASS("g.region",vect="mask")
##round to next int
region=execGRASS("g.region",intern=T,flag="p")
north=as.character(roundUp(as.numeric(substr(region[5],start=10,stop=nchar(region[5]))),1))
south=as.character(roundDown(as.numeric(substr(region[6],start=10,stop=nchar(region[6]))),1))
west=as.character(roundDown(as.numeric(substr(region[7],start=10,stop=nchar(region[7]))),1))
east=as.character(roundUp(as.numeric(substr(region[8],start=10,stop=nchar(region[8]))),1))
execGRASS("g.region",n=north,s=south,w=west,e=east,res=resolution)
region=execGRASS("g.region",flag="p")

##set mask
execGRASS("v.to.rast",input="mask",output="MASK",use="val",flags=c("overwrite"))

##variate tension and smoothing for v.surf.rst
tensionList=seq(10,150,by=10)
smoothList=seq(0,1,by=0.1)
#tension=40
#smooth=0
param.table=data.frame()
for (tension in tensionList){
  for (smooth in smoothList){
      print(sprintf("t%i_s%0.2f",tension,smooth))
      execGRASS("v.surf.rst",input=sprintf("ert_%0.2i",count),zcolumn="resistiv",smooth=smooth,tension=tension,cvdev=sprintf("cvdev_t%i_s%i",tension,round(smooth*100,0)),flags=c("c","overwrite"))
      valtab=read.dbf(sprintf("%s\\%s\\%s\\dbf\\cvdev_t%i_s%i.dbf",grassPath,location,mapset,tension,round(smooth*100,0)),as.is=FALSE)
      #valtab=read.dbf(sprintf("F:\\55_check_ert\\cdev_res02\\cvdev_t%i_s%i.dbf",tension,round(smooth*100,0)),as.is=FALSE)
      valtab["devsq"]=valtab[,2]**2
      rmse=sqrt(sum(valtab$devsq)/nrow(valtab))
      entry=data.frame(tension=tension,smooth=smooth,rmse=rmse)
      param.table=rbind(param.table,entry)
  }
}

write.table(param.table,"res_02.txt",col.names=F,row.names=F,quote=F)
