require(spgrass6)
require(rgdal)
require(foreign)

rootPath="F:\\55_check_ert"
setwd(rootPath)

##settings
location="Laterns"
grassPath="F:\\55_check_ert\\01_DATA\\grass"

##slope
filesPath="F:\\55_check_ert\\01_DATA\\Geoelektrik_Auswertung\\Bonacker\\2014_07_24_gp_lang120_bonacker"
maskFile="bonacker_slope_mask.shp"
resolution="5"
mapset="ERT_slopebonacker"
fileList=c("bf2_lang_120m_2014-07-24_11-00-00_Messung1_aligned.DAT","bf2_lang_120m_2014-07-24_11-20-00_Messung2_aligned.DAT")
setwd(filesPath)

##interpolation settings
tension=50
smooth=0

##FUNCTIONS
roundUp=function(x,to){
  to*(x%/%to+as.logical(x%%to))
}
roundDown=function(x,to){
  to*(x%/%to)
}

##set GRASS environment
initGRASS(gisBase="C:\\QGIS\\apps\\grass\\grass-6.4.3",
          home=tempdir(),
          gisDbase=grassPath,
          location=location,
          mapset=mapset,
          override=T)

##import mask shape
execGRASS("v.in.ogr",dsn=sprintf("%s\\%s",rootPath,maskFile),output="mask",flags=c("o","overwrite"))
##set region
execGRASS("g.region",vect="mask")
##round to next int
region=execGRASS("g.region",intern=T,flag="p")
north=as.character(roundUp(as.numeric(substr(region[5],start=10,stop=nchar(region[5]))),10))
south=as.character(roundDown(as.numeric(substr(region[6],start=10,stop=nchar(region[6]))),10))
west=as.character(roundDown(as.numeric(substr(region[7],start=10,stop=nchar(region[7]))),10))
east=as.character(roundUp(as.numeric(substr(region[8],start=10,stop=nchar(region[8]))),10))
execGRASS("g.region",n=north,s=south,w=west,e=east,res=resolution)
execGRASS("g.region",flag="p")

##set mask
execGRASS("v.to.rast",input="mask",output="MASK",use="val",flags=c("overwrite"))

count=1
for (element in fileList){
  ##import data
  timeERT=sprintf("%s_%0.2i",gsub("-","_",substr(element,26,33)),count)
  count=count+1
  
  execGRASS("v.in.ascii",input=sprintf("%s\\%s",filesPath,element),output=sprintf("ert_%s",timeERT),fs=";",x=1,y=2,columns="x double precision, y double precision, resistiv double precision, log_resi double precision",flags=c("quiet","overwrite"))
  
  ##interpolate spline with validated parameters
  execGRASS("v.surf.rst",input=sprintf("ert_%s",timeERT),zcolumn="resistiv",smooth=smooth,tension=tension,elev=sprintf("ert_%s",timeERT),flags=c("overwrite"))
}
