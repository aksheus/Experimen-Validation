########### section variables and initialization ###############################
setwd("C:\\Users\\abkma\\stats\\Stat")
rm(list=ls(all=TRUE)) # remove previous environment 
require(xlsx)
require(ggplot2)
require(grid)
all_directories=list.dirs(getwd(),recursive = TRUE)
cd_directories=all_directories[grep("*CD",all_directories)]
nd_directories=all_directories[grep("*ND",all_directories)] 
stm_pattern="*.stm"
stm_sheetindex=1
stm_startrow=9
stm_endrow=11
stm_colindex=c(1,2)
stm_from_cd=list()
hr_pattern="*.peda"
hr_sheetindex=1
hr_startrow=9
#significence_first= 0.0125 #0.05
significence_second=0.01 # 0.0025
significence_third= 0.001 # 0.00025
significence_fourth= 0.0001 # 0.000025
confidence_interval=0.9875
prune=function(x){ return(sum(x$Palm.EDA < 10 | x$Palm.EDA > 4700)>0)}
cd_directories=lapply(cd_directories,paste,"/",sep="")
for(d in cd_directories){stm_from_cd=c(stm_from_cd,paste(d,list.files(d,pattern = stm_pattern,recursive = TRUE),sep ="")) } 
removeList=intersect(stm_from_cd,cd_directories)
stm_from_cd=stm_from_cd[! stm_from_cd %in% removeList]
stm_frames=lapply(stm_from_cd,read.xlsx,sheetIndex=stm_sheetindex,startRow=stm_startrow,endRow=stm_endrow,colIndex=stm_colindex)
########################################################### delim
hr_from_cd=list()
for(d in cd_directories){hr_from_cd=c(hr_from_cd,paste(d,list.files(d,pattern = hr_pattern,recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_cd,cd_directories)
hr_from_cd=hr_from_cd[! hr_from_cd %in% removeList]
all_cd_hr_frames=lapply(hr_from_cd,read.xlsx,sheetIndex=hr_sheetindex,startRow=hr_startrow)
dropSignals=sapply(all_cd_hr_frames,prune)
all_cd_hr_frames=all_cd_hr_frames[!dropSignals]
############################################################## delim
hr_from_nd=list()
nd_directories=lapply(nd_directories,paste,"/",sep="")
for(d in nd_directories){hr_from_nd=c(hr_from_nd,paste(d,list.files(d,pattern = hr_pattern,recursive = TRUE),sep ="")) } # check 1 recursive need not be true
removeList=intersect(hr_from_nd,nd_directories)
hr_from_nd=hr_from_nd[! hr_from_nd %in% removeList]
all_nd_hr_frames=lapply(hr_from_nd,read.xlsx,sheetIndex=1,startRow=9)
dropSignals=sapply(all_nd_hr_frames,prune)
all_nd_hr_frames=all_nd_hr_frames[!dropSignals]
############# section variables and initialization #########################################
##### Section get list of split vectors ##########################
arg_time_name="Time"
arg_start_time="StartTime"
arg_end_time="EndTime"
splitvector_list=list()
GetSplitVectorEdges=function(data_frame,time_name){
  c(min(data_frame[,time_name]),max(data_frame[,time_name]))
}
GetSplitVectorMiddle=function(stm_frame,start_time,end_time){
  c( min(stm_frame[,start_time]),min(stm_frame[,end_time]),
     max(stm_frame[,start_time]),max(stm_frame[,end_time]))
}
edges=lapply(all_cd_hr_frames,GetSplitVectorEdges,time_name=arg_time_name)
middles=lapply(stm_frames,GetSplitVectorMiddle,start_time=arg_start_time,end_time=arg_end_time)
for( i in 1:length(edges)){ splitvector_list[[i]]=append(edges[[i]],middles[[i]],after = 1)}
################ section get list of split vectors ##############################
############## logic to cut df ########################################
#split_vector=c(min(all_cd_hr_frames[[1]]$Time),stm_frames[[1]]$StartTime[1],stm_frames[[1]]$EndTime[1],stm_frames[[1]]$StartTime[2],stm_frames[[1]]$EndTime[2],max(all_cd_hr_frames[[1]]$Time))
#phases_list=split(all_cd_hr_frames[[1]],cut(all_cd_hr_frames[[1]]$Time,split_vector,include.lowest = TRUE))
cd_phases_list=list()
for( i in 1:length(splitvector_list)){
  cd_phases_list[[i]]=split(all_cd_hr_frames[[i]],cut(all_cd_hr_frames[[i]]$Time,splitvector_list[[i]],include.lowest = TRUE))
}
nd_phases_list=list()
for( i in 1:length(splitvector_list)){
  nd_phases_list[[i]]=split(all_nd_hr_frames[[i]],cut(all_nd_hr_frames[[i]]$Time,splitvector_list[[i]],include.lowest = TRUE))
}
#################### logic to cut df ####################################################
######################### get mean vectors for all 5 phases #####################################
phase1=1
phase2=2
phase3=3
phase4=4
phase5=5
cd_phase1_means=numeric()
cd_phase2_means=numeric()
cd_phase3_means=numeric()
cd_phase4_means=numeric()
cd_phase5_means=numeric()
for( i in 1:length(cd_phases_list)){ cd_phase1_means[i]=mean(cd_phases_list[[i]][[phase1]]$Palm.EDA)}
for( i in 1:length(cd_phases_list)){ cd_phase2_means[i]=mean(cd_phases_list[[i]][[phase2]]$Palm.EDA)}
for( i in 1:length(cd_phases_list)){ cd_phase3_means[i]=mean(cd_phases_list[[i]][[phase3]]$Palm.EDA)}
for( i in 1:length(cd_phases_list)){ cd_phase4_means[i]=mean(cd_phases_list[[i]][[phase4]]$Palm.EDA)}
for( i in 1:length(cd_phases_list)){ cd_phase5_means[i]=mean(cd_phases_list[[i]][[phase5]]$Palm.EDA)}
nd_phase1_means=numeric()
nd_phase2_means=numeric()
nd_phase3_means=numeric()
nd_phase4_means=numeric()
nd_phase5_means=numeric()
for( i in 1:length(nd_phases_list)){ nd_phase1_means[i]=mean(nd_phases_list[[i]][[phase1]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase2_means[i]=mean(nd_phases_list[[i]][[phase2]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase3_means[i]=mean(nd_phases_list[[i]][[phase3]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase4_means[i]=mean(nd_phases_list[[i]][[phase4]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase5_means[i]=mean(nd_phases_list[[i]][[phase5]]$Palm.EDA)}
######################### need to make vectors same length ##########################
if(length(cd_phase1_means) > length(nd_phase1_means)){
  cd_phase1_means=cd_phase1_means[1:length(nd_phase1_means)]
} else if (length(nd_phase1_means) > length(cd_phase1_means)){
  nd_phase1_means=nd_phase1_means[1: length(cd_phase1_means)]
}
if(length(cd_phase2_means) > length(nd_phase2_means)){
  cd_phase2_means=cd_phase2_means[1:length(nd_phase2_means)]
} else if (length(nd_phase2_means) > length(cd_phase2_means)){
  nd_phase2_means=nd_phase2_means[1: length(cd_phase2_means)]
}
if(length(cd_phase3_means) > length(nd_phase3_means)){
  cd_phase3_means=cd_phase3_means[1:length(nd_phase3_means)]
} else if (length(nd_phase3_means) > length(cd_phase3_means)){
  nd_phase3_means=nd_phase3_means[1: length(cd_phase3_means)]
}
if(length(cd_phase4_means) > length(nd_phase4_means)){
  cd_phase4_means=cd_phase4_means[1:length(nd_phase4_means)]
} else if (length(nd_phase4_means) > length(cd_phase4_means)){
  nd_phase4_means=nd_phase4_means[1: length(cd_phase4_means)]
}
if(length(cd_phase5_means) > length(nd_phase5_means)){
  cd_phase5_means=cd_phase5_means[1:length(nd_phase5_means)]
} else if (length(nd_phase5_means) > length(cd_phase5_means)){
  nd_phase5_means=nd_phase5_means[1: length(cd_phase5_means)]
}
phase1_differences=cd_phase1_means-nd_phase1_means
phase2_differences=cd_phase2_means-nd_phase2_means
phase3_differences=cd_phase3_means-nd_phase3_means
phase4_differences=cd_phase4_means-nd_phase4_means
phase5_differences=cd_phase5_means-nd_phase5_means
######################### need to make vectors same length ##########################
############################ making differences positive ############################
min1=min(phase1_differences[!is.nan(phase1_differences)])
min2=min(phase2_differences[!is.nan(phase2_differences)])
min3=min(phase3_differences[!is.nan(phase3_differences)])
min4=min(phase4_differences[!is.nan(phase4_differences)])
min5=min(phase5_differences[!is.nan(phase5_differences)])
mins=c(min1,min2,min3,min4,min5)
mins=mins[!is.nan(mins)]
positive_displacement=10+(-min(mins))
phase1_differences=phase1_differences+positive_displacement
phase2_differences=phase2_differences+positive_displacement
phase3_differences=phase3_differences+positive_displacement
phase4_differences=phase4_differences+positive_displacement
phase5_differences=phase5_differences+positive_displacement
print("CD positive displacement")
print(positive_displacement)
#################################### t tests and stars computation ###################
#t.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
alpha_normal=0.05
shapiro_result1=shapiro.test(phase1_differences)
shapiro_result2=shapiro.test(phase2_differences)
shapiro_result3=shapiro.test(phase3_differences)
shapiro_result4=shapiro.test(phase4_differences)
shapiro_result5=shapiro.test(phase5_differences)
phase_test1=NULL
phase_test2=NULL
phase_test3=NULL
phase_test4=NULL
phase_test5=NULL
if(shapiro_result1$p.value > alpha_normal ){
  phase_test1=t.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase1_differences))$p.value > alpha_normal){
    phase_test1=t.test(log10(phase1_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase1_differences))$p.value > alpha_normal){
    phase_test1=t.test(sqrt(phase1_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test1=wilcox.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test1)
if(shapiro_result2$p.value > alpha_normal ){
  phase_test2=t.test(phase2_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase2_differences))$p.value > alpha_normal){
    phase_test2=t.test(log10(phase2_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase2_differences))$p.value > alpha_normal){
    phase_test2=t.test(sqrt(phase2_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test2=wilcox.test(phase2_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test2)
if(shapiro_result3$p.value > alpha_normal ){
  phase_test3=t.test(phase3_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase3_differences))$p.value > alpha_normal){
    phase_test3=t.test(log10(phase3_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater"
    )
  }else if(shapiro.test(sqrt(phase3_differences))$p.value > alpha_normal){
    phase_test3=t.test(sqrt(phase3_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test3=wilcox.test(phase3_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test3)
if(shapiro_result4$p.value > alpha_normal ){
  phase_test4=t.test(phase4_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase4_differences))$p.value > alpha_normal){
    phase_test4=t.test(log10(phase4_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase4_differences))$p.value > alpha_normal){
    phase_test4=t.test(sqrt(phase4_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test4=wilcox.test(phase4_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test4)
if(shapiro_result5$p.value > alpha_normal ){
  phase_test5=t.test(phase5_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase5_differences))$p.value > alpha_normal){
    phase_test5=t.test(log10(phase5_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase5_differences))$p.value > alpha_normal){
    phase_test5=t.test(sqrt(phase5_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test5=wilcox.test(phase5_differences,conf.level = confidence_interval,mu=positive_displacement
    )
  }
}
print(phase_test5)
stars=character()
if(phase_test1$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test1$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test1$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test2$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test2$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test2$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test3$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test3$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test3$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test4$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test4$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test4$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test5$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test5$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test5$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
################################### t tests and stars computation ####################
############################# subsection plotting boxplot #############################################
pdf("PalmEDAOutput.pdf",width = 15, height= 15)
par(mfrow=c(3,1))
dfphases=data.frame(Phase1=phase1_differences,Phase2=phase2_differences,Phase3=phase3_differences,Phase4=phase4_differences,Phase5=phase5_differences)
stress_color="firebrick1"
nostress_color="deepskyblue"
box_title="Differences of means across phases compared to baseline(black)"
box_ylabel="Palm EDA [KOhm] Phase-wise Means"
legend_text="CD"
ylim_upper=max(apply(dfphases,2,max,na.rm=TRUE))+5
ylim_lower=min(apply(dfphases,2,min,na.rm=TRUE))-5
bxp=boxplot(dfphases,main=box_title,ylab=box_ylabel,col=c(nostress_color,stress_color,nostress_color,stress_color,nostress_color),ylim = c(ylim_lower,ylim_upper))
abline(h=positive_displacement,lwd=2)
legend(x="topright",legend=legend_text,bty="n")
xcoords=c(1:5)
ycoords=apply(bxp$stats,2,max)
ntext=paste("n=",bxp$n)
text(x=xcoords,y=ycoords+2,labels = ntext,offset = 0.9,cex=1.2)
if(length(stars)>0){
  text(x=xcoords,y=ycoords+5,labels = stars,offset = 0.9,cex=2.5) } 
############################## subsection plottong boxplot ###########################################
######################### get mean vectors for all 5 phases ###################################################
cd_directories=NULL
all_cd_hr_frames=NULL
nd_phases_list=NULL
stm_frames=NULL
stm_from_cd=NULL
splitvector_list=NULL
edges=NULL
middles=NULL
nd_phases_list=NULL

################# $$$$$$$$ Similar code #repeat for #ED $$$$$$$$$$$$ ####################################################
################# $$$$$$$$$$$$$$$$$$$$$$$$$  BEGIN $$$$$$$$$$$$$$ ###############################################
ed_directories=all_directories[grep("*ED",all_directories)]
ed_directories=lapply(ed_directories,paste,"/",sep="")
stm_from_ed=list()
for(d in ed_directories){stm_from_ed=c(stm_from_ed,paste(d,list.files(d,pattern = stm_pattern,recursive = TRUE),sep ="")) } 
removeList=intersect(stm_from_ed,ed_directories)
stm_from_ed=stm_from_ed[! stm_from_ed %in% removeList]
stm_frames=lapply(stm_from_ed,read.xlsx,sheetIndex=stm_sheetindex,startRow=stm_startrow,endRow=stm_endrow,colIndex=stm_colindex)
####################### delim
hr_from_ed=list()
for(d in ed_directories){hr_from_ed=c(hr_from_ed,paste(d,list.files(d,pattern = hr_pattern,recursive = TRUE),sep ="")) } 
removeList=intersect(hr_from_ed,ed_directories)
hr_from_ed=hr_from_ed[! hr_from_ed %in% removeList]
all_ed_hr_frames=lapply(hr_from_ed,read.xlsx,sheetIndex=hr_sheetindex,startRow=hr_startrow)
dropSignals=sapply(all_ed_hr_frames,prune)
all_ed_hr_frames=all_ed_hr_frames[!dropSignals]
####################### delim
##### Section get list of split vectors ##########################
splitvector_list=list()
edges=lapply(all_ed_hr_frames,GetSplitVectorEdges,time_name=arg_time_name)
middles=lapply(stm_frames,GetSplitVectorMiddle,start_time=arg_start_time,end_time=arg_end_time)
for( i in 1:length(edges)){ splitvector_list[[i]]=append(edges[[i]],middles[[i]],after = 1)}
################ section get list of split vectors ##############################
############## logic to cut df ########################################
ed_phases_list=list()
for( i in 1:length(splitvector_list)){
  ed_phases_list[[i]]=split(all_ed_hr_frames[[i]],cut(all_ed_hr_frames[[i]]$Time,splitvector_list[[i]],include.lowest = TRUE))
}
nd_phases_list=list()
for( i in 1:length(splitvector_list)){
  nd_phases_list[[i]]=split(all_nd_hr_frames[[i]],cut(all_nd_hr_frames[[i]]$Time,splitvector_list[[i]],include.lowest = TRUE))
}
############## logic to cut df ########################################
######################### get mean vectors for all 5 phases #####################################
ed_phase1_means=numeric()
ed_phase2_means=numeric()
ed_phase3_means=numeric()
ed_phase4_means=numeric()
ed_phase5_means=numeric()
for( i in 1:length(ed_phases_list)){ ed_phase1_means[i]=mean(ed_phases_list[[i]][[phase1]]$Palm.EDA)}
for( i in 1:length(ed_phases_list)){ ed_phase2_means[i]=mean(ed_phases_list[[i]][[phase2]]$Palm.EDA)}
for( i in 1:length(ed_phases_list)){ ed_phase3_means[i]=mean(ed_phases_list[[i]][[phase3]]$Palm.EDA)}
for( i in 1:length(ed_phases_list)){ ed_phase4_means[i]=mean(ed_phases_list[[i]][[phase4]]$Palm.EDA)}
for( i in 1:length(ed_phases_list)){ ed_phase5_means[i]=mean(ed_phases_list[[i]][[phase5]]$Palm.EDA)}
nd_phase1_means=numeric()
nd_phase2_means=numeric()
nd_phase3_means=numeric()
nd_phase4_means=numeric()
nd_phase5_means=numeric()
for( i in 1:length(nd_phases_list)){ nd_phase1_means[i]=mean(nd_phases_list[[i]][[phase1]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase2_means[i]=mean(nd_phases_list[[i]][[phase2]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase3_means[i]=mean(nd_phases_list[[i]][[phase3]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase4_means[i]=mean(nd_phases_list[[i]][[phase4]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase5_means[i]=mean(nd_phases_list[[i]][[phase5]]$Palm.EDA)}
################# need to make the vectors same length ####################################
if(length(ed_phase1_means) > length(nd_phase1_means)){
  ed_phase1_means=ed_phase1_means[1:length(nd_phase1_means)]
} else if (length(nd_phase1_means) > length(ed_phase1_means)){
  nd_phase1_means=nd_phase1_means[1: length(ed_phase1_means)]
}
if(length(ed_phase2_means) > length(nd_phase2_means)){
  ed_phase2_means=ed_phase2_means[1:length(nd_phase2_means)]
} else if (length(nd_phase2_means) > length(ed_phase2_means)){
  nd_phase2_means=nd_phase2_means[1: length(ed_phase2_means)]
}
if(length(ed_phase3_means) > length(nd_phase3_means)){
  ed_phase3_means=ed_phase3_means[1:length(nd_phase3_means)]
} else if (length(nd_phase3_means) > length(ed_phase3_means)){
  nd_phase3_means=nd_phase3_means[1: length(ed_phase3_means)]
}
if(length(ed_phase4_means) > length(nd_phase4_means)){
  ed_phase4_means=ed_phase4_means[1:length(nd_phase4_means)]
} else if (length(nd_phase4_means) > length(ed_phase4_means)){
  nd_phase4_means=nd_phase4_means[1: length(ed_phase4_means)]
}
if(length(ed_phase5_means) > length(nd_phase5_means)){
  ed_phase5_means=ed_phase5_means[1:length(nd_phase5_means)]
} else if (length(nd_phase5_means) > length(ed_phase5_means)){
  nd_phase5_means=nd_phase5_means[1: length(ed_phase5_means)]
}
phase1_differences=ed_phase1_means-nd_phase1_means
phase2_differences=ed_phase2_means-nd_phase2_means
phase3_differences=ed_phase3_means-nd_phase3_means
phase4_differences=ed_phase4_means-nd_phase4_means
phase5_differences=ed_phase5_means-nd_phase5_means
######################## need to make vectors same length #################################
############################ making differences positive ############################
min1=min(phase1_differences[!is.nan(phase1_differences)])
min2=min(phase2_differences[!is.nan(phase2_differences)])
min3=min(phase3_differences[!is.nan(phase3_differences)])
min4=min(phase4_differences[!is.nan(phase4_differences)])
min5=min(phase5_differences[!is.nan(phase5_differences)])
mins=c(min1,min2,min3,min4,min5)
mins=mins[!is.nan(mins)]
positive_displacement=10+(-min(mins))
phase1_differences=phase1_differences+positive_displacement
phase2_differences=phase2_differences+positive_displacement
phase3_differences=phase3_differences+positive_displacement
phase4_differences=phase4_differences+positive_displacement
phase5_differences=phase5_differences+positive_displacement
print("ED positive displacement")
print(positive_displacement)
######################### get mean vectors for all 5 phases #####################################
########################## t tests and stars computation #######################################
alpha_normal=0.05
shapiro_result1=shapiro.test(phase1_differences)
shapiro_result2=shapiro.test(phase2_differences)
shapiro_result3=shapiro.test(phase3_differences)
shapiro_result4=shapiro.test(phase4_differences)
shapiro_result5=shapiro.test(phase5_differences)
phase_test1=NULL
phase_test2=NULL
phase_test3=NULL
phase_test4=NULL
phase_test5=NULL
if(shapiro_result1$p.value > alpha_normal ){
  phase_test1=t.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase1_differences))$p.value > alpha_normal){
    phase_test1=t.test(log10(phase1_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase1_differences))$p.value > alpha_normal){
    phase_test1=t.test(sqrt(phase1_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test1=wilcox.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test1)
if(shapiro_result2$p.value > alpha_normal ){
  phase_test2=t.test(phase2_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase2_differences))$p.value > alpha_normal){
    phase_test2=t.test(log10(phase2_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase2_differences))$p.value > alpha_normal){
    phase_test2=t.test(sqrt(phase2_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test2=wilcox.test(phase2_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test2)
if(shapiro_result3$p.value > alpha_normal ){
  phase_test3=t.test(phase3_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase3_differences))$p.value > alpha_normal){
    phase_test3=t.test(log10(phase3_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater"
    )
  }else if(shapiro.test(sqrt(phase3_differences))$p.value > alpha_normal){
    phase_test3=t.test(sqrt(phase3_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test3=wilcox.test(phase3_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test3)
if(shapiro_result4$p.value > alpha_normal ){
  phase_test4=t.test(phase4_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase4_differences))$p.value > alpha_normal){
    phase_test4=t.test(log10(phase4_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase4_differences))$p.value > alpha_normal){
    phase_test4=t.test(sqrt(phase4_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test4=wilcox.test(phase4_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test4)
if(shapiro_result5$p.value > alpha_normal ){
  phase_test5=t.test(phase5_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase5_differences))$p.value > alpha_normal){
    phase_test5=t.test(log10(phase5_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase5_differences))$p.value > alpha_normal){
    phase_test5=t.test(sqrt(phase5_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test5=wilcox.test(phase5_differences,conf.level = confidence_interval,mu=positive_displacement
    )
  }
}
print(phase_test5)
stars=character()
if(phase_test1$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test1$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test1$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test2$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test2$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test2$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test3$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test3$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test3$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test4$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test4$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test4$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test5$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test5$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test5$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
######################### t tests and stars computation ########################################
############################# subsection plotting boxplot #############################################
dfphases=data.frame(Phase1=phase1_differences,Phase2=phase2_differences,Phase3=phase3_differences,Phase4=phase4_differences,Phase5=phase5_differences)
stress_color="firebrick1"
nostress_color="deepskyblue"
box_title="Differences of means across phases compared to baseline(black)"
box_ylabel="Palm EDA [KOhm] Phase-wise Means"
legend_text="ED"
ylim_upper=max(apply(dfphases,2,max,na.rm=TRUE))+5
ylim_lower=min(apply(dfphases,2,min,na.rm=TRUE))-5
bxp=boxplot(dfphases,main=box_title,ylab=box_ylabel,col=c(nostress_color,stress_color,nostress_color,stress_color,nostress_color),ylim = c(ylim_lower,ylim_upper))
abline(h=positive_displacement,lwd=2)
legend(x="topright",legend=legend_text,bty="n")
xcoords=c(1:5)
ycoords=apply(bxp$stats,2,max)
ntext=paste("n=",bxp$n)
text(x=xcoords,y=ycoords+2,labels = ntext,offset = 0.9,cex=1.2)
if(length(stars)>0){
  text(x=xcoords,y=ycoords+5,labels = stars,offset = 0.9,cex=2.5)}
############################## subsection plottong boxplot ###########################################
################# @@@@@@@@@@@@@@@@@@@@@@@@ END @@@@@@@@@@@@@@@  #################################################
################# $$$$$$$$ Similar code #repeat for #MD $$$$$$$$$$$$ ####################################################
ed_directories=NULL
all_ed_hr_frames=NULL
nd_phases_list=NULL
stm_frames=NULL
stm_from_ed=NULL
splitvector_list=NULL
edges=NULL
middles=NULL
nd_phases_list=NULL
################# $$$$$$$$$$$$$$$$$$$$$$$$$  BEGIN $$$$$$$$$$$$$$ ###############################################
md_directories=all_directories[grep("*MD",all_directories)]
md_directories=lapply(md_directories,paste,"/",sep="")
stm_from_md=list()
for(d in md_directories){stm_from_md=c(stm_from_md,paste(d,list.files(d,pattern = stm_pattern,recursive = TRUE),sep ="")) } 
removeList=intersect(stm_from_md,md_directories)
stm_from_md=stm_from_md[! stm_from_md %in% removeList]
stm_frames=lapply(stm_from_md,read.xlsx,sheetIndex=stm_sheetindex,startRow=stm_startrow,endRow=stm_endrow,colIndex=stm_colindex)
####################### delim
hr_from_md=list()
for(d in md_directories){hr_from_md=c(hr_from_md,paste(d,list.files(d,pattern = hr_pattern,recursive = TRUE),sep ="")) } 
removeList=intersect(hr_from_md,md_directories)
hr_from_md=hr_from_md[! hr_from_md %in% removeList]
all_md_hr_frames=lapply(hr_from_md,read.xlsx,sheetIndex=hr_sheetindex,startRow=hr_startrow)
dropSignals=sapply(all_md_hr_frames,prune)
all_md_hr_frames=all_md_hr_frames[!dropSignals]
####################### delim
##### Section get list of split vectors ##########################
splitvector_list=list()
edges=lapply(all_md_hr_frames,GetSplitVectorEdges,time_name=arg_time_name)
middles=lapply(stm_frames,GetSplitVectorMiddle,start_time=arg_start_time,end_time=arg_end_time)
for( i in 1:length(edges)){ splitvector_list[[i]]=append(edges[[i]],middles[[i]],after = 1)}
################ section get list of split vectors ##############################
####################### tackle that non unique bastard ###########################
for(i in 1:length(splitvector_list)){
  if(length(unique(splitvector_list[[i]]))!=length(splitvector_list[[i]])){
    splitvector_list[[i]][which(duplicated(splitvector_list[[i]])==TRUE)]=splitvector_list[[i]][which(duplicated(splitvector_list[[i]])==TRUE)]+1
  }
}
############## logic to cut df ########################################
md_phases_list=list()
for( i in 1:length(splitvector_list)){
  
  md_phases_list[[i]]=split(all_md_hr_frames[[i]],cut(all_md_hr_frames[[i]]$Time,splitvector_list[[i]],include.lowest = TRUE))
}
nd_phases_list=list()
for( i in 1:length(splitvector_list)){
  nd_phases_list[[i]]=split(all_nd_hr_frames[[i]],cut(all_nd_hr_frames[[i]]$Time,splitvector_list[[i]],include.lowest = TRUE))
}
############## logic to cut df ########################################
######################### get mean vectors for all 5 phases #####################################
md_phase1_means=numeric()
md_phase2_means=numeric()
md_phase3_means=numeric()
md_phase4_means=numeric()
md_phase5_means=numeric()
for( i in 1:length(md_phases_list)){ md_phase1_means[i]=mean(md_phases_list[[i]][[phase1]]$Palm.EDA)}
for( i in 1:length(md_phases_list)){ md_phase2_means[i]=mean(md_phases_list[[i]][[phase2]]$Palm.EDA)}
for( i in 1:length(md_phases_list)){ md_phase3_means[i]=mean(md_phases_list[[i]][[phase3]]$Palm.EDA)}
for( i in 1:length(md_phases_list)){ md_phase4_means[i]=mean(md_phases_list[[i]][[phase4]]$Palm.EDA)}
for( i in 1:length(md_phases_list)){ md_phase5_means[i]=mean(md_phases_list[[i]][[phase5]]$Palm.EDA)}
nd_phase1_means=numeric()
nd_phase2_means=numeric()
nd_phase3_means=numeric()
nd_phase4_means=numeric()
nd_phase5_means=numeric()
for( i in 1:length(nd_phases_list)){ nd_phase1_means[i]=mean(nd_phases_list[[i]][[phase1]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase2_means[i]=mean(nd_phases_list[[i]][[phase2]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase3_means[i]=mean(nd_phases_list[[i]][[phase3]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase4_means[i]=mean(nd_phases_list[[i]][[phase4]]$Palm.EDA)}
for( i in 1:length(nd_phases_list)){ nd_phase5_means[i]=mean(nd_phases_list[[i]][[phase5]]$Palm.EDA)}
######################### get mean vectors for all 5 phases #####################################
########################## need to make vectors equal length ###################################
if(length(md_phase1_means) > length(nd_phase1_means)){
  md_phase1_means=md_phase1_means[1:length(nd_phase1_means)]
} else if (length(nd_phase1_means) > length(md_phase1_means)){
  nd_phase1_means=nd_phase1_means[1: length(md_phase1_means)]
}
if(length(md_phase2_means) > length(nd_phase2_means)){
  md_phase2_means=md_phase2_means[1:length(nd_phase2_means)]
} else if (length(nd_phase2_means) > length(md_phase2_means)){
  nd_phase2_means=nd_phase2_means[1: length(md_phase2_means)]
}
if(length(md_phase3_means) > length(nd_phase3_means)){
  md_phase3_means=md_phase3_means[1:length(nd_phase3_means)]
} else if (length(nd_phase3_means) > length(md_phase3_means)){
  nd_phase3_means=nd_phase3_means[1: length(md_phase3_means)]
}
if(length(md_phase4_means) > length(nd_phase4_means)){
  md_phase4_means=md_phase4_means[1:length(nd_phase4_means)]
} else if (length(nd_phase4_means) > length(md_phase4_means)){
  nd_phase4_means=nd_phase4_means[1: length(md_phase4_means)]
}
if(length(md_phase5_means) > length(nd_phase5_means)){
  md_phase5_means=md_phase5_means[1:length(nd_phase5_means)]
} else if (length(nd_phase5_means) > length(md_phase5_means)){
  nd_phase5_means=nd_phase5_means[1: length(md_phase5_means)]
}
phase1_differences=md_phase1_means-nd_phase1_means
phase2_differences=md_phase2_means-nd_phase2_means
phase3_differences=md_phase3_means-nd_phase3_means
phase4_differences=md_phase4_means-nd_phase4_means
phase5_differences=md_phase5_means-nd_phase5_means
########################## need to make vectors equal length ###################################
############################ making differences positive ############################
min1=min(phase1_differences[!is.nan(phase1_differences)])
min2=min(phase2_differences[!is.nan(phase2_differences)])
min3=min(phase3_differences[!is.nan(phase3_differences)])
min4=min(phase4_differences[!is.nan(phase4_differences)])
min5=min(phase5_differences[!is.nan(phase5_differences)])
mins=c(min1,min2,min3,min4,min5)
mins=mins[!is.nan(mins)]
positive_displacement=10+(-min(mins))
phase1_differences=phase1_differences+positive_displacement
phase2_differences=phase2_differences+positive_displacement
phase3_differences=phase3_differences+positive_displacement
phase4_differences=phase4_differences+positive_displacement
phase5_differences=phase5_differences+positive_displacement
print("MD positive displacement")
print(positive_displacement)
############################## t tests and stars computation ###################################
alpha_normal=0.05
shapiro_result1=shapiro.test(phase1_differences)
shapiro_result2=shapiro.test(phase2_differences)
shapiro_result3=shapiro.test(phase3_differences)
shapiro_result4=shapiro.test(phase4_differences)
shapiro_result5=shapiro.test(phase5_differences)
phase_test1=NULL
phase_test2=NULL
phase_test3=NULL
phase_test4=NULL
phase_test5=NULL
if(shapiro_result1$p.value > alpha_normal ){
  phase_test1=t.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase1_differences))$p.value > alpha_normal){
    phase_test1=t.test(log10(phase1_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase1_differences))$p.value > alpha_normal){
    phase_test1=t.test(sqrt(phase1_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test1=wilcox.test(phase1_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test1)
if(shapiro_result2$p.value > alpha_normal ){
  phase_test2=t.test(phase2_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase2_differences))$p.value > alpha_normal){
    phase_test2=t.test(log10(phase2_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase2_differences))$p.value > alpha_normal){
    phase_test2=t.test(sqrt(phase2_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test2=wilcox.test(phase2_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test2)
if(shapiro_result3$p.value > alpha_normal ){
  phase_test3=t.test(phase3_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase3_differences))$p.value > alpha_normal){
    phase_test3=t.test(log10(phase3_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater"
    )
  }else if(shapiro.test(sqrt(phase3_differences))$p.value > alpha_normal){
    phase_test3=t.test(sqrt(phase3_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test3=wilcox.test(phase3_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test3)
if(shapiro_result4$p.value > alpha_normal ){
  phase_test4=t.test(phase4_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase4_differences))$p.value > alpha_normal){
    phase_test4=t.test(log10(phase4_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase4_differences))$p.value > alpha_normal){
    phase_test4=t.test(sqrt(phase4_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test4=wilcox.test(phase4_differences,conf.level = confidence_interval,mu=positive_displacement)
  }
}
print(phase_test4)
if(shapiro_result5$p.value > alpha_normal ){
  phase_test5=t.test(phase5_differences,conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
}else{
  if(shapiro.test(log10(phase5_differences))$p.value > alpha_normal){
    phase_test5=t.test(log10(phase5_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else if(shapiro.test(sqrt(phase5_differences))$p.value > alpha_normal){
    phase_test5=t.test(sqrt(phase5_differences),conf.level = confidence_interval,mu=positive_displacement,alternative = "greater")
  }else{
    phase_test5=wilcox.test(phase5_differences,conf.level = confidence_interval,mu=positive_displacement
    )
  }
}
print(phase_test5)
stars=character()
if(phase_test1$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test1$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test1$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test2$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test2$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test2$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test3$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test3$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test3$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test4$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test4$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test4$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
if(phase_test5$p.value <= significence_second){
  stars=append(stars,"**")
}else if(phase_test5$p.value <= significence_third){
  stars=append(stars,"***")
}else if(phase_test5$p.value <= significence_fourth){
  stars=append(stars,"****")
}else{
  stars=append(stars,"")
}
############################## t tests and stars computation ####################################
############################# subsection plotting boxplot #############################################
dfphases=data.frame(Phase1=phase1_differences,Phase2=phase2_differences,Phase3=phase3_differences,Phase4=phase4_differences,Phase5=phase5_differences)
stress_color="firebrick1"
nostress_color="deepskyblue"
box_title="Differences of means across phases compared to baseline(black)"
box_ylabel="Palm EDA [KOhm] Phase-wise Means"
legend_text="MD"
ylim_upper=max(apply(dfphases,2,max,na.rm=TRUE))+5
ylim_lower=min(apply(dfphases,2,min,na.rm=TRUE))-5
bxp=boxplot(dfphases,main=box_title,ylab=box_ylabel,col=c(nostress_color,stress_color,nostress_color,stress_color,nostress_color),ylim = c(ylim_lower,ylim_upper))
abline(h=positive_displacement,lwd=2)
legend(x="topright",legend=legend_text,bty="n")
xcoords=c(1:5)
ycoords=apply(bxp$stats,2,max)
ntext=paste("n=",bxp$n)
text(x=xcoords,y=ycoords+2,labels = ntext,offset = 0.9,cex=1.2)
if(length(stars)>0){
  text(x=xcoords,y=ycoords+5,labels = stars,offset = 0.9,cex=2.5)}
############################## subsection plottong boxplot ###########################################
dev.off()
################# @@@@@@@@@@@@@@@@@@@@@@@@ END @@@@@@@@@@@@@@@  #################################################

