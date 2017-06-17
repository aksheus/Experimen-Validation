#hear rate range is [40-140]
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# declaration section start
rm(list=ls(all=TRUE)) # remove previous environment 
require(xlsx)
require(ggplot2)
require(grid)
# declaration section end 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# get all hr files from BL,PD etc. start
all_directories=list.dirs(getwd(),recursive = TRUE)
bl_directories=all_directories[grep("*BL",all_directories)] #1
pd_directories=all_directories[grep("*PD",all_directories)] #2
rd_directories=all_directories[grep("*RD",all_directories)] #3
nd_directories=all_directories[grep("*ND",all_directories)] #4
cd_directories=all_directories[grep("*CD",all_directories)] #5
ed_directories=all_directories[grep("*ED",all_directories)] #6
md_directories=all_directories[grep("*MD",all_directories)] #7
# leave fd for now need to verify regex later 
fd_directories=all_directories[grep("*FD*",all_directories)] #8
bl_directories=lapply(bl_directories,paste,"/",sep="")
pd_directories=lapply(pd_directories,paste,"/",sep="")
rd_directories=lapply(rd_directories,paste,"/",sep="")
nd_directories=lapply(nd_directories,paste,"/",sep="")
cd_directories=lapply(cd_directories,paste,"/",sep="")
ed_directories=lapply(ed_directories,paste,"/",sep="")
md_directories=lapply(md_directories,paste,"/",sep="")
fd_directories=lapply(fd_directories,paste,"/",sep="")
######### BL
hr_from_bl=list()
for(d in bl_directories){hr_from_bl=c(hr_from_bl,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_bl,bl_directories)
hr_from_bl=hr_from_bl[! hr_from_bl %in% removeList]
all_bl_hr_frames=lapply(hr_from_bl,read.xlsx,sheetIndex=1,startRow=9)
########## BL 
######### PD
hr_from_pd=list()
for(d in pd_directories){hr_from_pd=c(hr_from_pd,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_pd,pd_directories)
hr_from_pd=hr_from_pd[! hr_from_pd %in% removeList]
all_pd_hr_frames=lapply(hr_from_pd,read.xlsx,sheetIndex=1,startRow=9)
##########  PD
##########  RD
hr_from_rd=list()
for(d in rd_directories){hr_from_rd=c(hr_from_rd,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_rd,rd_directories)
hr_from_rd=hr_from_rd[! hr_from_rd %in% removeList]
all_rd_hr_frames=lapply(hr_from_rd,read.xlsx,sheetIndex=1,startRow=9)
############ RD 
###########  ND
hr_from_nd=list()
for(d in nd_directories){hr_from_nd=c(hr_from_nd,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_nd,nd_directories)
hr_from_nd=hr_from_nd[! hr_from_nd %in% removeList]
all_nd_hr_frames=lapply(hr_from_nd,read.xlsx,sheetIndex=1,startRow=9)
############ ND 
##########   CD 
hr_from_cd=list()
for(d in cd_directories){hr_from_cd=c(hr_from_cd,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_cd,cd_directories)
hr_from_cd=hr_from_cd[! hr_from_cd %in% removeList]
all_cd_hr_frames=lapply(hr_from_cd,read.xlsx,sheetIndex=1,startRow=9)
############# CD 
############# ED 
hr_from_ed=list()
for(d in ed_directories){hr_from_ed=c(hr_from_ed,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_ed,ed_directories)
hr_from_ed=hr_from_ed[! hr_from_ed %in% removeList]
all_ed_hr_frames=lapply(hr_from_ed,read.xlsx,sheetIndex=1,startRow=9)
############ ED
########### MD 
hr_from_md=list()
for(d in md_directories){hr_from_md=c(hr_from_md,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_md,md_directories)
hr_from_md=hr_from_md[! hr_from_md %in% removeList]
all_md_hr_frames=lapply(hr_from_md,read.xlsx,sheetIndex=1,startRow=9)
########### MD
########### FD 
hr_from_fd=list()
for(d in fd_directories){hr_from_fd=c(hr_from_fd,paste(d,list.files(d,pattern = "*.HR",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_fd,fd_directories)
hr_from_fd=hr_from_fd[! hr_from_fd %in% removeList]
all_fd_hr_frames=lapply(hr_from_fd,read.xlsx,sheetIndex=1,startRow=9)
########### FD 
#get all hr files from BL,PD etc. end 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# this below code apply to all data frames start
prune=function(x){ return(sum(x$Heart.Rate < 40 | x$Heart.Rate > 120)>0)}
pdf("HeartRate.pdf",width = 15,height = 15)
par(mfrow=c(8,2)) #mar=c(2,2,2,2))
myYlabel="HR[bpm]"
myYLimits=c(40,140)
yRawLimits=c(0,myYLimits[2]*2)
rawTitle="Heart Rate[bpm] raw signal sets"
cleanedTitle="Heart Rate[bpm] valid signal sets"
#for BL
plot(all_bl_hr_frames[[1]]$Time,all_bl_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim = yRawLimits)
iter=c(2:length(all_bl_hr_frames))
for(i in iter){lines(all_bl_hr_frames[[i]]$Time,all_bl_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_bl_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("BL",s),bty="n")
dropSignals=sapply(all_bl_hr_frames,prune)
n=length(all_bl_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_bl_hr_frames=all_bl_hr_frames[!dropSignals]
plot(all_bl_hr_frames[[1]]$Time,all_bl_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_bl_hr_frames))
for(i in iter){lines(all_bl_hr_frames[[i]]$Time,all_bl_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("BL",s),bty="n")
#for BL 
# for PD
plot(all_pd_hr_frames[[1]]$Time,all_pd_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_pd_hr_frames))
for(i in iter){lines(all_pd_hr_frames[[i]]$Time,all_pd_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_pd_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("PD",s),bty="n")
dropSignals=sapply(all_pd_hr_frames,prune)
n=length(all_pd_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_pd_hr_frames=all_pd_hr_frames[!dropSignals]
plot(all_pd_hr_frames[[1]]$Time,all_pd_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_pd_hr_frames))
for(i in iter){lines(all_pd_hr_frames[[i]]$Time,all_pd_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("PD",s),bty="n")
# for PD
#for RD
plot(all_rd_hr_frames[[1]]$Time,all_rd_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_rd_hr_frames))
for(i in iter){lines(all_rd_hr_frames[[i]]$Time,all_rd_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_rd_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("RD",s),bty="n")
dropSignals=sapply(all_rd_hr_frames,prune)
n=length(all_rd_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_rd_hr_frames=all_rd_hr_frames[!dropSignals]
plot(all_rd_hr_frames[[1]]$Time,all_rd_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_rd_hr_frames))
for(i in iter){lines(all_rd_hr_frames[[i]]$Time,all_rd_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("RD",s),bty="n")
#for RD
#for ND
plot(all_nd_hr_frames[[1]]$Time,all_nd_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_nd_hr_frames))
for(i in iter){lines(all_nd_hr_frames[[i]]$Time,all_nd_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_nd_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("ND",s),bty="n")
dropSignals=sapply(all_nd_hr_frames,prune)
n=length(all_nd_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_nd_hr_frames=all_nd_hr_frames[!dropSignals]
plot(all_nd_hr_frames[[1]]$Time,all_nd_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_nd_hr_frames))
for(i in iter){lines(all_nd_hr_frames[[i]]$Time,all_nd_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("ND",s),bty="n")
#for ND
#for CD
plot(all_cd_hr_frames[[1]]$Time,all_cd_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_cd_hr_frames))
for(i in iter){lines(all_cd_hr_frames[[i]]$Time,all_cd_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_cd_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("CD",s),bty="n")
dropSignals=sapply(all_cd_hr_frames,prune)
n=length(all_cd_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_cd_hr_frames=all_cd_hr_frames[!dropSignals]
plot(all_cd_hr_frames[[1]]$Time,all_cd_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_cd_hr_frames))
for(i in iter){lines(all_cd_hr_frames[[i]]$Time,all_cd_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("CD",s),bty="n")
#for CD 
#for ED
plot(all_ed_hr_frames[[1]]$Time,all_ed_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_ed_hr_frames))
for(i in iter){lines(all_ed_hr_frames[[i]]$Time,all_ed_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_ed_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("ED",s),bty="n")
dropSignals=sapply(all_ed_hr_frames,prune)
n=length(all_ed_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_ed_hr_frames=all_ed_hr_frames[!dropSignals]
plot(all_ed_hr_frames[[1]]$Time,all_ed_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_ed_hr_frames))
for(i in iter){lines(all_ed_hr_frames[[i]]$Time,all_ed_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("ED",s),bty="n")
#for ED
#for MD
plot(all_md_hr_frames[[1]]$Time,all_md_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_md_hr_frames))
for(i in iter){lines(all_md_hr_frames[[i]]$Time,all_md_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_md_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("MD",s),bty="n")
dropSignals=sapply(all_md_hr_frames,prune)
n=length(all_md_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_md_hr_frames=all_md_hr_frames[!dropSignals]
plot(all_md_hr_frames[[1]]$Time,all_md_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_md_hr_frames))
for(i in iter){lines(all_md_hr_frames[[i]]$Time,all_md_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("MD",s),bty="n")
#for MD
#for FD
plot(all_fd_hr_frames[[1]]$Time,all_fd_hr_frames[[1]]$Heart.Rate,"l",main=rawTitle,xlab="Time[s]",ylab=myYlabel,ylim=yRawLimits)
iter=c(2:length(all_fd_hr_frames))
for(i in iter){lines(all_fd_hr_frames[[i]]$Time,all_fd_hr_frames[[i]]$Heart.Rate,"l")}
n=length(all_fd_hr_frames)
s=paste("n-",n,sep="")
legend(x="topright",legend=c("FD",s),bty="n")
dropSignals=sapply(all_fd_hr_frames,prune)
n=length(all_fd_hr_frames)-sum(dropSignals)
s=paste("n-",n,sep="")
all_fd_hr_frames=all_fd_hr_frames[!dropSignals]
plot(all_fd_hr_frames[[1]]$Time,all_fd_hr_frames[[1]]$Heart.Rate,"l",main=cleanedTitle,xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(all_fd_hr_frames))
for(i in iter){lines(all_fd_hr_frames[[i]]$Time,all_fd_hr_frames[[i]]$Heart.Rate,"l")}
legend(x="topright",legend=c("FD",s),bty="n")
#for FD
dev.off()











