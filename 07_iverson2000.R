require(raster)
require(RColorBrewer)
require(lattice)


#require(NORMT3)
require(pracma)

##regolith depth
zmax=2
##starting depth
zmin=0.1
##depth of water table
d=1.34

##saturated hydraulic conductivity
ks=2.511886e-07
##specific storage
ss=0.005011872


##hydraulic diffusivity
d0=ks/ss
##slope angle [radian]
slope=31.3*pi/180

##time
#time=c(0,3600,7200,10800,14400,18000,19200)
time=c(0,420,820,1260,1680,2100,2520,2940,3360,3780,4200,4620,5040,5460,5880,6300,6720)
#time=c(0,0,0,420,820,1260,1680,2100,2520,2940,3360,3780,4200,4620,5040,5460,5880,6300,6720)
#time=c(0,300,600,900)
##precipitation (27.5 mm/h)
#iz=c(0.0,0.0,7.63888e-6,7.63888e-6,7.63888e-6,7.63888e-6,7.63888e-6,7.63888e-6)
iz=c(0.0,0.0,0.0,0.0,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889,0.00000763889)
#iz=c(0.0,0.00000763889,0.00000763889)


##number of time steps
tsteps=length(time)
nper=length(iz)

##coordiante transformation for slope-parallel ground water flow
#beta=cos(slope)^2
beta=19.88624 
##correction of the original Iverson (2000) formula dm=4.0*d0*beta
##see Baum et al. 2010
dm=4.0*d0/cos(slope)^2


##beta-line (saturated)
psi0=(seq(zmin,zmax,zmin)-d)*beta
par(xaxs="i",yaxs="i")
plot(psi0,seq(zmin,zmax,zmin),type="l",xlim=c(-40,0),ylim=c(zmax,0),ylab="Depth [m]",xlab="Pressure head [m]")
##beta-line (initial)
psisat=(seq(zmin,zmax,zmin))*beta
lines(psisat,seq(zmin,zmax,zmin))
abline(v=0)

##all pwp time steps
pwpTime=data.frame()

# ##own scripts ()
# cols=rev(topo.colors(nper+1))
# for (tstep in 1:(nper)){
#   tstar1=time[tstep]/(seq(zmin,zmax,zmin)^2/dm)
#   rstar1=(tstar1/pi)^0.5*exp(-1/tstar1)-erfc(1/(tstar1^0.5))
#   psistep1=beta*(1-d/seq(zmin,zmax,zmin))+iz[tstep]/ks*rstar1
#   psi1=seq(zmin,zmax,zmin)*psistep1
#   
#   ##limit is saturated beta-line
#   psi1[psi1>psisat]=psisat[psi1>psisat]
#   #print(psi1)
#   
#   lines(psi1,type="l",
#         seq(zmin,zmax,zmin),
#         ylab="Depth [m]",
#         xlab="pore pressure [m]",
#         col=cols[tstep]
#         )
#   
#   pwpTime=rbind(pwpTime,psi1)
# }
# #legend("topright",legend=time,col=cols,lty=1)





cols=rev(topo.colors(nper+1))
tdata=data.frame(NA)
ptrans=0.0
for (tstep1 in 1:(nper+1)){
  t0=time[tstep1]
  zdata=data.frame()
  for (z in seq(zmin,zmax,zmin)){
    pzero=beta*(z-d)
    zstar=z**2/(4.*d0/cos(slope)^2)
    tstar=t0/zstar
    psi=0.0
    for (tstep2 in 1:nper){
      captstar1=time[tstep2]/zstar 
      tdif1=tstar-captstar1
      if(tdif1 > 0.0){
        rfa=((tdif1/pi)^0.5)*exp(-1./tdif1)-erfc(1./((tdif1)^0.5))
      }else{
        rfa=0.0
      }
      captstar2=time[tstep2+1]/zstar
      tdif2=tstar-captstar2
      if (tdif2 > 0.0){
        rfb=((tdif2/pi)^0.5)*exp(-1./tdif2)-erfc(1./((tdif2)^0.5))
      }else{
        rfb=0.0
      }
      ##ORIGINAL TRIGRS
      #psi=psi+iz[tstep2]/ks*(rfa-rfb)
      ##ADAPTIERT
      psi=psi+iz[tstep2]/ks*(rfa-rfb)*z
    }
    pmax=z*beta
    if (abs(psi)>0.0){
      ptrans=psi
    }
    p=pzero+ptrans
    if (p>pmax){
      p=pmax
    }
    zdata=rbind(zdata,p)
  }
  lines(zdata[,1],seq(zmin,zmax,zmin),col=cols[tstep1])
  tdata=cbind(tdata,zdata)
}
pwpTime=t(tdata[,c(-1,-length(time))])



# data.c=pwpTime
# ##create color ramp
# cols=colorRampPalette(brewer.pal(3,"RdBu"))(100)
# ##convert to matrix
# data.m=as.matrix(t(data.c)[1:10,])
# data.r=raster(data.m)
# 
# ##stack rasters and compute correlation coefficient
# raststack=stack(data.r,Suctionrast)
# corel=layerStats(raststack,'pearson')
# corel$'pearson correlation coefficient'[1,2]
# 
# 
# ##scale in regard to measurements
# zMin=-30
# zMax=-10
# data.r[data.r>zMax]=zMax
# data.r[data.r<zMin]=zMin
# 
# image(data.r)
# 
# ##plot
# colors=rev(colorRampPalette(brewer.pal(9,"RdBu"))(100))
# par(xaxs="i",yaxs="i",mar=c(4,5,4,6))
# plot(NA,NA,xlim=c(0,1),ylim=c(0,1),axes=F,ylab="",xlab="")
# axis(side=1,at=seq((1/(length(time)-1)/2),1-(1/(length(time)-1)/2),length.out=(length(time)-1)),labels=time[2:length(time)]/60)
# axis(side=2,at=seq(0,1,0.2),labels=seq(1,0,-0.2),las=1)
# mtext(side=1,"Time [min]",line=2.5)
# mtext(side=2,"Depth [m]",line=2.5,las=0)
# mtext(side=4,line=5,"Pressure head [m]",las=0)
# #plot(data.r,col=rev(colors),zlim=c(-0.5,-0.2),add=T)
# plot(data.r,col=rev(colors),zlim=c(-30,-10),add=T)
