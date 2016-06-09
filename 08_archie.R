##plot resistivity reduction against water content
rrData20=data.frame(resist=c(-0.01109639,-0.02348743,-0.04964289,-0.10460372,-0.21900430,-0.45250017,-0.91054364,-1.74301179,-3.06526300,-4.77400123,-6.47778081,-7.78942356,-8.61220196,-9.06392609,-9.29393142,-9.40655558,-9.46064748,-9.48638590),
                    vol=c(35.5,35.5,34.7,35.6,35.2,33.7,34.8,34.4,35.9,35.4,35.3,37.7,39.8,37.7,39.9,38.9,40.5,39.4))
rrData60=data.frame(resist=c(-0.001269012,-0.002539575,-0.005080405,-0.010155933,-0.020272689,-0.040350589,-0.079856877,-0.156294442,-0.299486384,-0.552189028,-0.954489957,-1.500590135,-2.101050636,-2.625919385,-3.000312306,-3.230374971,-3.359034241,-3.427217344),
                    vol=c(36.4,37.8,37.0,36.3,37.5,37.2,36.2,36.7,36.3,37.8,37.9,37.1,38.3,37.3,37.7,38.7,38.6,39.7))

par(pty="s",xaxs="i",yaxs="i")
plot(rrData20$vol,rrData20$resist,col="dark green",ylim=c(-10,0),xlim=c(32,42))
points(rrData60$vol,rrData60$resist,col="red",ylim=c(-10,0),xlim=c(32,42))

##fit linear model
##linFit=lm(resist~vol,data=rrData20)
##summary(linFit)
##abline(linFit,col="dark green")
##newdata=data.frame(vol=seq(33,40,0.1))
##newdata["y"]=predict(linFit,newdata)




##Spezifischer Widerstand Leitungswasser
rho_w=20 ##Ohm m

##Tortuositaetsfaktor
a=1 ##0.6 bis 1.0

##Porositaet
theta=0.511

TDRdata
##initial saturation (TDR measurements before irrigation)
#s_init20=mean(c(36.5,36.4,35.0,36.4,34.9,35.2,35.8,35.5,35.5,35.7,35.5,35.3,34.6,34.7,35.5,35.5,34.7,35.6,35.2,33.7))/100
##measurements between 13:25
s_init20=mean(c(35.5,35.5,34.7,35.6,35.2,33.7))/100
s_init60=mean(c(36.4,37.8,37.0,36.3,37.5,37.2))/100

##Zementationsexponent
m=2

##Saettigungsexponent
n=0.5

##specify the number of data points
datapoints=10



##archie's law
##--> does not apply because of substantial clay fraction and therefore surface conductivity
##Archie-Gleichung fuer partielle Saettigung (Archie 1942)
# archie=function(rho_w,a,theta,m,S_w,n){
#   rho_b=rho_w*a*theta^(-m)*S_w^(-n)
#   return(rho_b)
# }
#
# par(mfrow=c(1,1))
# rho_s0=archie(rho_w,a,theta,m,s_init20/theta,n)
# ##vary saturation for measured value range
# data=data.frame()
# for (S_w in seq(s_init20/theta,0.42/theta,length.out=datapoints)){
#   rho_s=archie(rho_w,a,theta,m,S_w,n)
#   data=rbind(data,c(S_w,rho_s))
# }
#
# lines(theta*data[,1]*100,(data[,2]/rho_s0-1)*100,xlim=c(0.3,0.4),ylim=c(-30,0),type="l")




##derive soil type from german texture triangle
# require(soiltexture)
# soilData=data.frame(depth=c("0-10","10-20","20-30","40-50"),
#                     CLAY=c(12.2757,12.5509,14.9101,16.8005),
#                     SILT=c(72.8426,65.3664,69.3988,68.9310),
#                     SAND=c(14.8817,22.0828,15.6911,14.2686)
#                     )
#
# TT.plot(
#   class.sys="DE.BK94.TT",
#   tri.data=soilData[,c(2,3,4)],
#   grid.show=F,
#   col="red",
#   frame.bg.col="white",
#   font.main="Helvetica",
#   font="Helvetica",
#   fg="white",
#   lwd=1,
#   lwd.axis=0.5,
#   lwd.lab=0.5,
#   arrows.show=1
# )
##texture class according to BK94: Ut3 (mittel toniger Schluff)
##KAKpot after KA5 (2005, p. 369): 11 cmol/kg
##=0.11 meq/g
##or use formula: KAK [cmol/kg] = 0.5 * CLAY + 0.05 * SILT
##mean(soilData$CLAY)*0.5+mean(soilData$SILT)*0.05
##=0.1052 cmol/kg
##bulk density=1.34 g/cm3
##Q_v=0.11*1.34=0.15 meq/cm3



##get estimation from conductivity of water from measurements in the laternser valley
# conductivity=read.csv("http://ehyd.gv.at/eHYD/MessstellenExtraData/qu?id=395731&file=2",sep=";",dec=",",skip=23,header=F)
# names(conductivity)=c("datum","conductivity")
# head(conductivity)
#
# ##convert measurements to numbers
# conductivity$conductivity=as.numeric(conductivity$conductivity)
#
# mean(conductivity$conductivity)



##get estimation of water temperature
# temperature=read.csv("http://ehyd.gv.at/eHYD/MessstellenExtraData/qu?id=395731&file=4",sep=";",dec=",",skip=23,header=F)
# names(temperature)=c("datum","temperature")
# head(temperature)
# tail(temperature)
#
# ##convert measurements to numbers
# temperature$temperature=as.numeric(temperature$temperature)
#
# mean(temperature$temperature)



for (n in c(2,1,0.5)){
  
  ##Waxman and Smits (1968) after Cassiani etal 2009
  sigma_w=0.023
  Q_v=0.15
  
  waxman=function(s_init,theta,m,S_w,n,Q_v,sigma_w){
    B=(1.93*m)/(1+0.7/sigma_w)
    rr=(((s_init/theta)/S_w)^n)*(((B*Q_v)/(S_w)+sigma_w)/((B*Q_v)/(s_init/theta)+sigma_w))
    return(rr)
  }
  
  data=data.frame()
  for (S_w in seq(s_init20/theta,0.42/theta,length.out=datapoints)){
    rho_s=waxman(s_init20,theta,m,S_w,n,Q_v,sigma_w)
    data=rbind(data,c(S_w,rho_s))
  }
  
  lines(theta*data[,1]*100,(data[,2]-1)*100,xlim=c(0.3,0.4),ylim=c(-30,0),type="l")
  
  
  data=data.frame()
  for (S_w in seq(s_init60/theta,0.42/theta,length.out=datapoints)){
    rho_s=waxman(s_init60,theta,m,S_w,n,Q_v,sigma_w)
    data=rbind(data,c(S_w,rho_s))
  }
  
  lines(theta*data[,1]*100,(data[,2]-1)*100,xlim=c(0.3,0.4),ylim=c(-30,0),type="l")
  
}


##test temperature
# test=data.frame()
# for (t in 1:40){
#   rr=0.02125*(t-25)+1
#   test=rbind(test,c(t,rr))
# }
#
# plot(test,type="l")
# 