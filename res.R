rm(list=ls(all=TRUE)) # remove previous environment 
require(xlsx)
require(ggplot2)
require(grid)
#######################################################################################################
all_directories=list.dirs(getwd(),recursive = TRUE)
prune=function(x){ return(sum(x$Acceleration < 0 | x$Acceleration > 90)>0)}
pdf("res.pdf",width = 15,height = 15)
par(mfrow=c(7,5)) #mar=c(2,2,2,2))
myYlabel="Acceleration[*]"
myYLimits=c(0,90)
#######################################################################################################
####################################### PD BEGIN#######################################################
legendText="PD"
pd_directories=all_directories[grep("*PD",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
############################################################## PD END #######################################################################
########################################################### RD begin ########################################################################
legendText="RD"
pd_directories=all_directories[grep("*RD",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
######################################################### RD end #######################################################
########################################################## ND begin ########################################################
legendText="ND"
pd_directories=all_directories[grep("*ND",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
################################################################# ND end ########################################################
############################################################## CD begin ##########################################################
legendText="CD"
pd_directories=all_directories[grep("*CD",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
############################################################## CD end ######################################################
############################################################ ED begin ######################################################
legendText="ED"
pd_directories=all_directories[grep("*ED",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
################################################################## ED end ################################################################
############################################################### MD Begin ##################################################################
legendText="MD"
pd_directories=all_directories[grep("*MD",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
############################################################## MD end ###########################################################
################################################################ FD begin #######################################################
legendText="FD"
pd_directories=all_directories[grep("*FD",all_directories)] #2
pd_directories=lapply(pd_directories,paste,"/",sep="")
res_from_pd=list()
for(d in pd_directories){res_from_pd=c(res_from_pd,paste(d,list.files(d,pattern = "*.res",recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(res_from_pd,pd_directories)
res_from_pd=res_from_pd[! res_from_pd %in% removeList]
all_pd_res_frames=lapply(res_from_pd,read.xlsx,sheetIndex=1,startRow=9)
remove(pd_directories)
remove(res_from_pd)
#########################################################################################################
plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Speed,"l",xlab="Time[s]",ylab="Speed [Km/h]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Speed,"l")}
legend(x="topright",legend=legendText,bty="n")

dropSignals=sapply(all_pd_res_frames,prune)
pdResFramesCopy=all_pd_res_frames[!dropSignals]
plot(pdResFramesCopy[[1]]$Time,pdResFramesCopy[[1]]$Acceleration,"l",xlab="Time[s]",ylab=myYlabel,ylim=myYLimits)
iter=c(2:length(pdResFramesCopy))
for(i in iter){lines(pdResFramesCopy[[i]]$Time,pdResFramesCopy[[i]]$Acceleration,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(pdResFramesCopy)

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Braking,"l",xlab="Time[s]",ylab="Breaking [N]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Braking,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Steering,"l",xlab="Time[s]",ylab="Steering [rad]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")

plot(all_pd_res_frames[[1]]$Time,all_pd_res_frames[[1]]$Lane.Position,"l",xlab="Time[s]",ylab="Lane Position [m]")
iter=c(2:length(all_pd_res_frames))
for(i in iter){lines(all_pd_res_frames[[i]]$Time,all_pd_res_frames[[i]]$Steering,"l")}
legend(x="topright",legend=legendText,bty="n")
remove(all_pd_res_frames)
################################################################## FD END ###################################################################
dev.off()
