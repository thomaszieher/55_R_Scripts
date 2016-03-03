setwd("F:\\55_check_ert\\01_DATA\\")

require(RColorBrewer)

data=read.table("rmse_res_1.txt",header=F,sep=" ",stringsAsFactors=F)
names(data)=c("tension","smooth","rmse")

cols=colorRampPalette(brewer.pal(6,"Spectral"))(length(unique(data$smooth)))

par(xaxs="i",yaxs="i")
plot(NA,NA,xlim=c(0,150),ylim=c(0,100),ylab="",xlab="",axes="")
count=1
for (smoothing in unique(data$smooth)){
  data.sub=subset(data,smooth==smoothing)
  lines(data.sub$tension,data.sub$rmse,col=cols[count],lwd=2)
  count=count+1
}
axis(side=1,at=seq(0,150,by=25))
mtext(side=1,"Tension",line=3)
axis(side=2,at=seq(0,100,by=25))
mtext(side=2,"RMSE [Ohmm]",line=3,las=0)

legend("topright",title="Smoothing",legend=unique(data$smooth),lty=1,col=cols,lwd=2)

##best settings
data[data$rmse==min(data$rmse),]



##check best parameters
errors=read.table("cvdev_res1_t50_s0.txt",sep="\t",header=T,stringsAsFactors=F)

hist(errors$cv_error,breaks=100,xlim=c(-20,20))
