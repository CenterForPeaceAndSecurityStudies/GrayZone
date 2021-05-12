################################################################
##    Benson Clinton
##    JCR Replication File
################################################################

# First Compute the Estimates using MCMCPack
# Then Analyze the results

# Compute Estimates Assuming Independence Between Dimensions (so run separately)

library(foreign)
library(MCMCpack)

# Get Data and Recode
setwd("/Users/clintojd/Dropbox/irscaling/DATA")
alldata<-read.dta(file="BensonClintonMainNonAggression.dta")

#  Drop Missing Data
alldata<-alldata[complete.cases(alldata),]
alldata<-as.data.frame(alldata)

# Dimension 1 - Signatory Strength

alldata$distance<-exp(alldata$ldistance)
alldata$distance<-alldata$distance-1

Dimension1<-MCMCfactanal(~lallycount+lsumcap+polity+maxmajpow+distance+s_wt_glo,
                         data=alldata,
                         factors=1,
                         lambda.constraints=list(lsumcap=list(1,"+")),
                         std.mean=TRUE, std.var=TRUE,verbose=100000,
                         mcmc = 1000000,burnin=100000,thin=1000,store.scores=TRUE)

save(Dimension1,file="Dimension1.R")

factor.load1<-Dimension1[,1:12]
apply(factor.load1,2,mean)
rbd<-Dimension1[,-seq(1:12)]
Dimension1.score<-apply(rbd,2,mean)

# Dimension 2 - Cost and Signaling Strength

depth<-MCMCfactanal(~milcontact+commondef+intcom+milaid+base+contrib+organ1+ecaid+secrecy,
                    data=alldata,
                    factors=1,
                    lambda.constraints=list(base=list(1,"+")),
                    std.mean=TRUE, std.var=TRUE,verbose=100000,
                    mcmc = 1000000,burnin=100000,thin=1000,store.scores=TRUE)

save(depth,file="depth_factanal.R")

factor.load2<-depth[,1:18]
apply(factor.load2,2,mean)
rbd<-depth[,-seq(1:18)]
depth.score<-apply(rbd,2,mean)

# Dimension 3 - Scope

scope<-MCMCfactanal(~macowe+maconc+maconp+nmacowe+nmaroir+condother+renounceok+renounceproh+renouncecond+compel+deterministic+unconditional+offenseatop+defenseatop+neutatop+consultatop,
                    data=alldata,
                    factors=1,
                    lambda.constraints=list(compel=list(1,"+")),
                    std.mean=TRUE, std.var=TRUE,verbose=100000,
                    mcmc = 1000000,burnin=100000,thin=1000,store.scores=TRUE)

save(scope,file="scope_factanal.R")

factor.load3<-scope[,1:32]
apply(factor.load3,2,mean)
rbd<-scope[,-seq(1:32)]
scope.score<-apply(rbd,2,mean)

# Save Data to File

AllianceDataScore<-as.data.frame(cbind(alldata$atopid,Dimension1.score,depth.score,scope.score))
AllianceDataScore<-round(AllianceDataScore,digits=3)
write.csv(AllianceDataScore,file="AllianceDataScore.csv")

#################################################
#     Now Analyze
#     Produce Plots in Paper and Appendix
#################################################


################################################################################
#  READ FILES
################################################################################

# Get Data
alldata<-read.dta(file="BensonClintonMainNonAggression.dta")
undata<-read.dta(file="ATOPUNAffinity.dta")
alldata<-alldata[complete.cases(alldata),]
alldata<-merge(alldata,undata,by.y="atopid",by.x="atopid")

load(file="Dimension1.R")
load(file="depth_factanal.R")
load(file="scope_factanal.R")

factor.load1<-Dimension1[,1:12]
apply(factor.load1,2,mean)
rbd<-Dimension1[,-seq(1:12)]
PotMilCapacity.score<-apply(rbd,2,mean)

factor.load2<-depth[,1:18]
apply(factor.load2,2,mean)
rbd<-depth[,-seq(1:18)]
Depth.score<-apply(rbd,2,mean)

factor.load3<-scope[,1:32]
apply(factor.load3,2,mean)
rbd<-scope[,-seq(1:32)]
Scope.score<-apply(rbd,2,mean)

indx<-seq(1,nrow(alldata))
AllianceData<-cbind(alldata,PotMilCapacity.score,Depth.score,Scope.score,indx)
AllianceData<-round(AllianceData,digits=3)
write.csv(AllianceData,file="AllianceDataScoreJCR_RR.csv")

##############################################################################
# Compare Estimates - Alliances
##############################################################################

# Franco-Belgium
AllianceData[alldata$atopid==2055,]
# 1915 alliance
AllianceData[alldata$atopid==2025,]
# UK-Ethioia
AllianceData[alldata$atopid==2580,]
########################################################################
#	Now create 2D plots for each
########################################################################
library(ellipse)

factor.load1<-Dimension1[,1:12]
apply(factor.load1,2,mean)
amatrix1<-Dimension1[,-seq(1:12)]

factor.load2<-depth[,1:18]
apply(factor.load2,2,mean)
amatrix2<-depth[,-seq(1:18)]

factor.load3<-scope[,1:32]
apply(factor.load3,2,mean)
amatrix3<-scope[,-seq(1:32)]

afmean1<-apply(amatrix1,2,mean)
afsd1<-apply(amatrix1,2,sd)
afmean2<-apply(amatrix2,2,mean)
afsd2<-apply(amatrix2,2,sd)
afmean3<-apply(amatrix3,2,mean)
afsd3<-apply(amatrix3,2,sd)


################################################
#       FIGURE 2 in TEXT
################################################

pdf(file="AllianceScoresDimension1Dimension2.pdf")
plot(afmean1,afmean2,xlab="Potential Military Capacity",ylab="Depth of Agreement",type="n",xlim=c(-2,2),ylim=c(-1,3.5),axes=FALSE)

points(afmean1,afmean2,pch=18,col="GREY",cex=.75)
axis(1)
axis(2)

p<-as.logical(alldata$atopid==2550)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix2[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix2[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix2[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix2[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix2[,p]-.25),"WWII Allies",cex=.75)

p<-as.logical(alldata$atopid==3480)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix2[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix2[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix2[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix2[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix2[,p]+.25),"US-Spain (1963)",cex=.75)

p<-as.logical(alldata$atopid==2055)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix2[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix2[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix2[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix2[,p]),pch=18)
text(mean(amatrix1[,p]-.5),mean(amatrix2[,p]-.25),"Franco-Belgian Accord (1920)",cex=.75)

p<-as.logical(alldata$atopid==3345)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix2[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix2[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix2[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix2[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix2[,p]-.25),"U.A.E-Yemen (1958)",cex=.75)

p<-as.logical(alldata$atopid==4515)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix2[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix2[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix2[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix2[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix2[,p]-.25),"Belarus-Bulgaria (1993)",cex=.75)
dev.off()

################################################
#       FIGURE 3 in TEXT
################################################

pdf(file="AllianceScoresDimension1Dimension3.pdf")
plot(afmean1,afmean3,xlab="Potential Military Capacity",ylab="Scope of Agreement",type="n",xlim=c(-2,2),ylim=c(-1,3),axes=FALSE)

points(afmean1,afmean3,pch=18,col="GREY",cex=.75)
axis(1)
axis(2)

p<-as.logical(alldata$atopid==2550)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix3[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix3[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix3[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix3[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix3[,p]-.25),"WWII Allies",cex=.75)

p<-as.logical(alldata$atopid==3480)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix3[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix2[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix3[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix3[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix3[,p]-.25),"US-Spain (1963)",cex=.75)

p<-as.logical(alldata$atopid==2055)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix3[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix3[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix3[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix3[,p]),pch=18)
text(mean(amatrix1[,p]-.5),mean(amatrix3[,p]-.25),"Franco-Belgian Accord (1920)",cex=.75)

p<-as.logical(alldata$atopid==3345)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix3[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix3[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix3[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix3[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix3[,p]-.25),"U.A.E-Yemen (1958)",cex=.75)

p<-as.logical(alldata$atopid==4515)*seq(1,nrow(alldata))
points(ellipse(cor(amatrix1[,p],amatrix3[,p]),scale=c(sd(amatrix1[,p]),sd(amatrix3[,p])),centre=c(mean(amatrix1[,p]),mean(amatrix3[,p])),level=.9),type="l")
points(mean(amatrix1[,p]),mean(amatrix3[,p]),pch=18)
text(mean(amatrix1[,p]),mean(amatrix3[,p]-.25),"Belarus-Bulgaria (1993)",cex=.75)
dev.off()

################################################
#       FIGURE 4 in TEXT
################################################

factor.load2<-depth[,1:18]
flm<-apply(factor.load2,2,mean)
lq<-apply(factor.load2,2,quantile,probs=c(.025))
uq<-apply(factor.load2,2,quantile,probs=c(.975))
scorem<-cbind(flm,lq,uq)
n<-length(apply(factor.load2,2,mean))
indx<-seq(1:n)
scorem<-scorem[1:9,]

fname<-c("Military Contact","Common Defense","Integrated Command","Military Aid","Military Bases","Specific Oblig.","Organization","Economic Aid","Secrecy")
indx<-sort(flm[1:9],index.return=TRUE)

pdf(file="Dimension2FactorScores.pdf")
plot(scorem[indx$ix,1],seq(1,9),pch=18,xlab="",ylab="",xlim=c(-2,1.5),type="n",axes=FALSE)  
for(i in 1:nrow(scorem)){  
  segments(scorem[indx$ix[i],2],i,scorem[indx$ix[i],3],i)    
  points(scorem[indx$ix[i],1],i,pch=18)
  text(-2,i,fname[indx$ix[i]],pos=4)
}
abline(v=0)
axis(1,at=seq(-.5,1,by=.5))
axis(3,at=seq(-.5,1,by=.5))
dev.off()

################################################
#       FIGURE 5 in TEXT
################################################

factor.load3<-scope[,1:32]
flm<-apply(factor.load3,2,mean)
lq<-apply(factor.load3,2,quantile,probs=c(.025))
uq<-apply(factor.load3,2,quantile,probs=c(.975))
scorem<-cbind(flm,lq,uq)
n<-length(apply(factor.load3,2,mean))
indx<-seq(1:n)
scorem<-scorem[1:16,]

fname<-c("Mil. Action depend on war env.","Mil. Action depend on non-compliance","Mil. action depend on non-provoc.",
         "Non-mil. action depend on war env.","Non-mil. action reqd only if requested","Conditional (other)",
         "Renunciation Allowed","Renunciation Prohibited","Renunciation Conditional","Compellent",
         "Deterministic","Unconditional","Offensive Pact","Defensive Pact","Neutrality Pact","Consultation Pact")
indx<-sort(flm[1:16],index.return=TRUE)

pdf(file="Dimension3FactorScores.pdf")
plot(scorem[indx$ix,1],seq(1,16),pch=18,xlab="",ylab="",xlim=c(-3,1.5),type="n",axes=FALSE)  
for(i in 1:nrow(scorem)){  
  segments(scorem[indx$ix[i],2],i,scorem[indx$ix[i],3],i)    
  points(scorem[indx$ix[i],1],i,pch=18)
  text(-3,i,fname[indx$ix[i]],pos=4,cex=.75)
}
abline(v=0)
axis(1,at=seq(-.5,1,by=.5))
axis(3,at=seq(-.5,1,by=.5))
dev.off()

################################################
################################################
#       APPENDIX
################################################
################################################

#####################################################
# Figure 1 in Appendix
#####################################################

wwi<-AllianceData[AllianceData$atopid==1350 | AllianceData$atopid==1385 | AllianceData$atopid==2030 | AllianceData$atopid==2025,] 
wwin<-c("Triple Alliance (1882)","Franco-Russian Alliance (1893)","WWI Axis (1915)","WWI Allies (1915)")

wwii<-AllianceData[AllianceData$atopid==2445 | AllianceData$atopid==2540 | AllianceData$atopid==2550,] 
wwiin<-c("Pact of Steel (1939)","WWII Axis (1941)","WWII Allies (1942)")

ai<-AllianceData[AllianceData$atopid==3200 | AllianceData$atopid==3220 | AllianceData$atopid==3240 | AllianceData$atopid==3270 | AllianceData$atopid==3375 | AllianceData$atopid==3440 | AllianceData$atopid==3445,] 
an<-c("USSR-China (1950)","US-Japan (1951)","US-South Korea (1953)","US-Republic of China (1954)","US-Japan (1960)","USSR-North Korea (1961)","China-North Korea (1961)")

plotindx<-c(wwi$indx,wwii$indx,ai$indx)
plotn<-c(wwin,wwiin,an)

smalld1<-amatrix1[,plotindx]
smalld3<-amatrix3[,plotindx]

SS<-length(plotn)

#sorti<-sort(dim1[plotindx],index.return=TRUE)$ix

q1025<-rep(NA,times=SS)
q2025<-rep(NA,times=SS)
q1975<-rep(NA,times=SS)
q2975<-rep(NA,times=SS)


for(i in 1:SS){
  q1025[i]<-quantile(smalld1[,i],.025)  #	95 CI
  q2025[i]<-quantile(smalld3[,i],.025)
  q1975[i]<-quantile(smalld1[,i],.975)
  q2975[i]<-quantile(smalld3[,i],.975)
}

xbar1<-cbind(afmean1[plotindx],q1025,q1975)
colnames(xbar1)<-c("mean","lo","up")

xbar3<-cbind(afmean3[plotindx],q2025,q2975)
colnames(xbar3)<-c("mean","lo","up")

#####################################################
#	CREATE FIGURE 3
#####################################################

pdf(file = "SelectedAlliancesDim1Dim3.pdf")
par(mfrow=c(1,3),mar=c(2,.5,2,.5))
plot(x=c(0,3),y=c(1,15),type="n",axes=F,xlab="",ylab="",ylim=c(.5,15.5),yaxs="i")

abline(v=seq(0,3,by=1),lty=2,col=1,lwd=.5)
text(1.5,SS+1,"Potential Military Capacity")
for(i in 1:SS){
  lines(y=c(i,i),x=c(xbar1[i,"lo"],xbar1[i,"up"]),lwd=1,col="GREY")
  points(y=i,x=xbar1[i,"mean"],pch=18)
  #  text(5,i,plotn[i],cex=.5,pos=4)  
}  
abline(h=4.5,lwd=1,col="GREY")
abline(h=7.5,lwd=1,col="GREY")

axis(1,
     at=c(seq(0,3,by=1)),
     labels=c("0","1","2","3"),
     cex.axis=.75,lwd=2)
axis(3,
     at=c(seq(0,3,by=1)),
     labels=c("0","1","2","3"),
     cex.axis=.75,lwd=2)

plot(x=c(-2,1),y=c(1,15),type="n",axes=FALSE)
for(i in 1:SS){
  text(-.5,i,plotn[i],cex=1.25)  
}  
abline(h=4.5,lwd=1,col="GREY")
abline(h=7.5,lwd=1,col="GREY")

plot(x=c(-1,3),y=c(1,15),type="n",axes=F,xlab="",ylab="",ylim=c(.5,15.5),yaxs="i")
text(1,SS+1,"Alliance Scope")

abline(v=seq(-1,3,by=1),lty=2,col=1,lwd=.5)
for(i in 1:SS){
  lines(y=c(i,i),x=c(xbar3[i,"lo"],xbar3[i,"up"]),lwd=1,col="GREY")
  points(y=i,x=xbar3[i,"mean"],pch=18)
}  

abline(h=4.5,lwd=1,col="GREY")
abline(h=7.5,lwd=1,col="GREY")

axis(1,
     at=c(seq(-1,3,by=1)),
     labels=c("-1","0","1","2","3"),
     cex.axis=.75,lwd=2)
axis(3,
     at=c(seq(-1,3,by=1)),
     labels=c("-1","0","1","2","3"),
     cex.axis=.75,lwd=2)
dev.off()

##########################
# Figure 2 in Appendix
##########################

indx<-NULL
indx[alldata$neutatop==1]<-1
indx[alldata$nagatop==1]<-2
indx[alldata$offenseatop==1]<-3
indx[alldata$defenseatop==1]<-4

pdf(file="Dimension3CorrelatesDensity.pdf")
par(mfrow=c(3,1))
plot(density(afmean3[alldata$offenseatop==1]),xlab="",main="Offensive",xlim=c(-1,3))
plot(density(afmean3[alldata$defenseatop==1]),xlab="",main="Defensive",xlim=c(-1,3))
plot(density(afmean3[alldata$neutatop==1]),xlab="",main="Neutrality",xlim=c(-1,3))
dev.off()

##########################
# Figure 3 in Appendix
##########################

factor.load1<-Dimension1[,1:12]
flm<-apply(factor.load1,2,mean)
lq<-apply(factor.load1,2,quantile,probs=c(.025))
uq<-apply(factor.load1,2,quantile,probs=c(.975))
scorem<-cbind(flm,lq,uq)
n<-length(apply(factor.load1,2,mean))
indx<-seq(1:n)
scorem<-scorem[1:6,]
fname<-c("log(allycount)","Log(Sum Capacity)","Avg. Polity IV Score","Major Power Involved","Avg. Distance","Avg. S-Score")
indx<-sort(flm[1:6],index.return=TRUE)

pdf(file="Dimension1FactorScores.pdf")
plot(scorem[indx$ix,1],seq(1,6),pch=18,xlab="",ylab="",xlim=c(-2,1.5),type="n",axes=FALSE)  
for(i in 1:nrow(scorem)){  
  segments(scorem[indx$ix[i],2],i,scorem[indx$ix[i],3],i)    
  points(scorem[indx$ix[i],1],i,pch=18)
  text(-2,i,fname[indx$ix[i]],pos=4)
}
abline(v=0)
axis(1,at=seq(-.5,1,by=.5))
axis(3,at=seq(-.5,1,by=.5))
dev.off()







