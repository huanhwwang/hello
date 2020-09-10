

library(reshape2)
library(ggplot2)
library(tidyr)
library(stringr)
library(plyr); library(dplyr)
library(pracma)
library(scales)
###Section 1. for lab study
nba <- read.csv('/Users/LondonLab/Desktop/nba_18_subs.csv')

#nba=nba_raw[-(1:12),]
ratings=cbind(nba$feeling,nba$agree,nba$know_dont_shareV,nba$know_shareV,nba$DK_shareV,nba$DK_dont_shareV)
ratings_df<-data.frame(ratings)
ratings_melted<-melt(ratings_df)
ss_df=data.frame(nba$participant)
##create z score for resp bias analysis, z scoring across 6 questions before caulcating GAVG
ratings_df_z_6=scale(t(ratings_df))
ratings_df_z_6[is.nan(ratings_df_z_6)]<-0
ratings_df_z_6=t(ratings_df_z_6)

ratings_df_z_aff=scale(t(ratings_df[,1:2]))
ratings_df_z_aff[is.nan(ratings_df_z_aff)]<-0
ratings_df_z_aff=t(ratings_df_z_aff)

ratings_df_z_sh=scale(t(ratings_df[,3:6]))
ratings_df_z_sh[is.nan(ratings_df_z_sh)]<-0
ratings_df_z_sh=t(ratings_df_z_sh)

vid=nba$Video_Number
vid_df=data.frame(vid)
vid_m=rbind(vid_df,vid_df,vid_df,vid_df,vid_df,vid_df)

###z scoring for each question separately (across video) before calculating GAVG

ratings_df_z_6_vid=cbind(ratings_df,vid_df,ss_df)
ssz=data.frame()
ssvid=data.frame()
for (isub in 1:length(unique(ratings_df_z_6_vid$nba.participant))) {
  ssz=rbind(ssz,scale(ratings_df_z_6_vid[ratings_df_z_6_vid$nba.participant==unique(ratings_df_z_6_vid$nba.participant)[isub],][,1:6]))
  ssvid=rbind(ssvid,data.frame(ratings_df_z_6_vid[ratings_df_z_6_vid$nba.participant==unique(ratings_df_z_6_vid$nba.participant)[isub],][,7]))
}
colnames(ssvid)='vid'
ssz_p=cbind(abs(ssz),ssvid)
  
ssz_gavg_p=aggregate(ssz_p[,1:6],by=list(ssz_p$vid),FUN=mean)
ssz_gavg_p=melt(ssz_gavg_p)

pdata=cbind(ratings_melted,vid_m)
pdata$value=pdata$value-4
pdata_z_6=cbind(melt(abs(ratings_df_z_6)),vid_m)
pdata_z_aff=cbind(melt(abs(ratings_df_z_aff)),vid_m[1:(473*2),])
pdata_z_sh=cbind(melt(abs(ratings_df_z_sh)),vid_m[(473*2+1):dim(vid_m)[1],])
colnames(pdata_z_aff)[4]="vid"
colnames(pdata_z_sh)[4]="vid"

#pdata_avg=pdata %>%
 # group_by(vid) %>%
  #summarize(avg=mean(pdata$value, na.rm= TRUE ))
pdata_avg=aggregate(pdata$value,by=list(pdata$vid,pdata$variable),FUN=mean)
pdata_z_avg6=aggregate(pdata_z_6$value,by=list(pdata_z_6$vid,pdata_z_6$Var2),FUN=mean)
pdata_z_avgaff=aggregate(pdata_z_aff$value,by=list(pdata_z_aff$vid,pdata_z_aff$Var2),FUN=mean)
pdata_z_avgsh=aggregate(pdata_z_sh$value,by=list(pdata_z_sh$vid,pdata_z_sh$Var2),FUN=mean)

####calculate composite score
nba_comp=cbind(abs(ratings_df-4))

nba_avg_comp_aff=data.frame(rowMeans(nba_comp[,1:2],na.rm = TRUE)) # affect+agree/disagree
nba_avg_comp_sh=data.frame(rowMeans(nba_comp[,3:6],na.rm = TRUE)) # 4 share questions
nba_avg_comp_6=data.frame(rowMeans(nba_comp[,1:6],na.rm = TRUE)) # 6

pdata_comp_aff=cbind(nba_avg_comp_aff,vid_df)
pdata_comp_sh=cbind(nba_avg_comp_sh,vid_df)
pdata_comp_6=cbind(nba_avg_comp_6,vid_df)

pdata_avg_comp_aff=aggregate(pdata_comp_aff,by=list(pdata_comp_aff$vid),FUN=mean)
pdata_avg_comp_sh=aggregate(pdata_comp_sh,by=list(pdata_comp_sh$vid),FUN=mean)
pdata_avg_comp_6=aggregate(pdata_comp_6,by=list(pdata_comp_6$vid),FUN=mean)

##grab group label
cate_df_uni=unique(nba[,c("Category","Video_Number")])
pdata_avg_comp_aff_lb=cbind(pdata_avg_comp_aff,cate_df_uni[order(cate_df_uni$Video_Number),])
pdata_avg_comp_sh_lb=cbind(pdata_avg_comp_sh,cate_df_uni[order(cate_df_uni$Video_Number),])
pdata_avg_comp_6_lb=cbind(pdata_avg_comp_6,cate_df_uni[order(cate_df_uni$Video_Number),])

  #calculate category label avg compscore
pdata_compbycate=aggregate(pdata_avg_comp_lb$CompScore,by=list(pdata_avg_comp_lb$Category),FUN=mean)
  #factorise vidnum labels and sort it by Category (for plotting)
pdata_avg_comp_aff_lb_srt=pdata_avg_comp_aff_lb
colnames(pdata_avg_comp_aff_lb_srt)[2]="CompScore"
pdata_avg_comp_aff_lb_srt$Video_Number=factor(pdata_avg_comp_aff_lb$Video_Number,levels=pdata_avg_comp_aff_lb$Video_Number[order(pdata_avg_comp_aff_lb$Category)])
pdata_avg_comp_aff_lb_srt$Category=gsub("AntiVaxxers","Vaccination",pdata_avg_comp_aff_lb_srt$Category)

pdata_avg_comp_sh_lb_srt=pdata_avg_comp_sh_lb
colnames(pdata_avg_comp_sh_lb_srt)[2]="CompScore"
pdata_avg_comp_sh_lb_srt$Video_Number=factor(pdata_avg_comp_sh_lb$Video_Number,levels=pdata_avg_comp_sh_lb$Video_Number[order(pdata_avg_comp_sh_lb$Category)])
pdata_avg_comp_sh_lb_srt$Category=gsub("AntiVaxxers","Vaccination",pdata_avg_comp_sh_lb_srt$Category)

pdata_avg_comp_6_lb_srt=pdata_avg_comp_6_lb
colnames(pdata_avg_comp_6_lb_srt)[2]="CompScore"
pdata_avg_comp_6_lb_srt$Video_Number=factor(pdata_avg_comp_6_lb$Video_Number,levels=pdata_avg_comp_6_lb$Video_Number[order(pdata_avg_comp_6_lb$Category)])
pdata_avg_comp_6_lb_srt$Category=gsub("AntiVaxxers","Vaccination",pdata_avg_comp_6_lb_srt$Category)


##topic of interests
toi=aggregate(nba$Category,by=list(nba$participant),FUN=unique,simplify=TRUE)
freq_toi=table(toi$x)

  #plotting freq table
par(mar=c(8,4,2,2))
plot(freq_toi,type="h",las=2)

# heatmap
ggplot(data = pdata_avg, aes(x=Group.2,y=Group.1, fill=x)) + 
  geom_tile()+scale_fill_gradient2(low = "green", mid="white",high = "red",na.value = "black")

ggplot(data = pdata_z_avg6, aes(x=Group.2,y=Group.1, fill=x)) + 
  geom_tile()+scale_fill_gradient2(low = "green", mid="white",high = "red",na.value = "black")

ggplot(data = rbind(pdata_z_avgaff,pdata_z_avgsh), aes(x=Group.2,y=Group.1, fill=x)) + 
  geom_tile()+scale_fill_gradient2(low = "green", mid="white",high = "red",na.value = "black")

ggplot(data = ssz_gavg_p, aes(x=variable,y=Group.1, fill=value)) + 
  geom_tile()+scale_fill_gradient2(high = "red",mid="white",low="white",midpoint=0.5,na.value = "black")


##bar graph
  #vid level grouped by category
ggplot(data=pdata_avg_comp_aff_lb_srt, aes(y=CompScore, x=Video_Number)) +
  geom_bar(aes(fill=Category),position="dodge",stat="identity",width=0.7)+coord_flip()+theme(legend.key.size = unit(1, "cm"))
ggplot(data=pdata_avg_comp_sh_lb_srt, aes(y=CompScore, x=Video_Number)) +
  geom_bar(aes(fill=Category),position="dodge",stat="identity",width=0.7)+coord_flip()+theme(legend.key.size = unit(1, "cm"))
ggplot(data=pdata_avg_comp_6_lb_srt, aes(y=CompScore, x=Video_Number)) +
  geom_bar(aes(fill=Category),position="dodge",stat="identity",width=0.7)+coord_flip()+theme(legend.key.size = unit(1, "cm"))


  #cate level
ggplot(data=pdata_compbycate, aes(y=x, x=Group.1)) +
  geom_bar(position="dodge",stat="identity",color="blue",fill="blue",width=0.1)+coord_flip()+labs(y="CompScore",x="Topics")


#########                     ######
###Section 2. for Mturk study ######
###########                   ######
  #use csv directly for now...
  #mt_lb=pdata_avg_comp_lb[,c("vidnum","Category")]
  #mt_lb$vidnum=gsub(".mp4","",mt_lb$vidnum)
  #colnames(mt_lb)=c("vid","cate")

  ######Section 2.1 generate bar plots of z scores for resp bias per vid for the mturk data, 
  ######sort the plot from high to low and color code each vid catogory

  mturk_d=read.csv("/Users/hwang/Google_Drive/DARPA/analysis/scripts/Mturk/Mturk_alldata_cleaned.csv")
  mturk_l=read.csv("/Users/hwang/Google_Drive/DARPA/analysis/scripts/Mturn_labels.csv")
          # mturk_d_srt=mturk_d[sort(mturk_d$WorkerID),]
          # IDidx=seq_along(mturk_d_srt$WorkerID)[duplicated(mturk_d_srt$WorkerID)]
          # which(duplicated(mturk_d_srt$WorkerID))
          # which(!duplicated(mturk_d_srt$WorkerID))
          # seq_along(mturk_d_srt$WorkerID)[!duplicated(mturk_d_srt$WorkerID)]
          # seq_along(mturk_d_srt$WorkerID)[!duplicated(mturk_d_srt$WorkerID)]
          length(unique(mturk_d$WorkerID))
          mturk_d[mturk_d$WorkerID==unique(mturk_d$WorkerID)[1],1:5]
  mturk_z_raw={}
  for (imtid in 1:length(unique(mturk_d$WorkerID))){
    mturk_z_raw=rbind(mturk_z_raw,cbind(mturk_d[mturk_d$WorkerID==unique(mturk_d$WorkerID)[imtid],1:5],apply(mturk_d[mturk_d$WorkerID==unique(mturk_d$WorkerID)[imtid],6:11],2,function(x) (x-mean(x))/std(x))))
  }
  mturk_z_gavg=aggregate(mturk_z_raw[,6:11],by=list(mturk_z_raw$VidNum),FUN=function(x) mean(abs(x),na.rm=TRUE))
  mturk_z_gavg=cbind(mturk_z_gavg,mturk_l[order(mturk_l$vid),])
  
  mt_z_plot=mturk_z_gavg  

  mturk_m_raw=cbind(vid=mturk_d[,2],data.frame(apply(mturk_d[,6:11],2,function(x) abs(x-4))))
  
  #mturk_m_raw=cbind(vid=mturk_d[,2],data.frame(apply(mturk_d[,6:11],2,function(x) (x-4))))
  
  mturk_m=aggregate(mturk_m_raw,by=list(mturk_m_raw$vid),FUN=function(x) mean(x,na.omit=TRUE))
  write.csv(mturk_m,"/Users/hwang/Google_Drive/DARPA/analysis/scripts/Mturk/Mturk_avgRESP.csv")
  
  mtplot_avg_resp=cbind(mturk_m,cate=mturk_l[order(mturk_l$vid),][,2]) ##resp
  mtplot_avg_resp_comp=apply(mtplot_avg_resp[,3:8],mean)
  #mturk_z_1=lapply(mturk_z_raw[],function(x){c(abs=abs(x),mean=mean(x))})
      #mturk_z={}
      # ###this is wrong we need to link z score to vidnum and avg across ss rather than vids.
      # for (irow in 1:6){
      # d_abs=lapply(mturk_z_raw[,irow+1],FUN=abs)
      # d_avg=lapply(d_abs,FUN=mean,na.omit=TRUE)
      # mturk_z=cbind(mturk_z,d_avg)
      # }
      # colnames(mturk_z)=c('Agree','Feel','KnDis','KnAg','DKDis','DKAg')
      # mtplot_avg=lapply(mturk_z,FUN=mean,na.omit=TRUE)
      # mtplot_avg=data.frame(AvgZscores=rowMeans(matrix(unlist(mtplot_avg),nrow=253, byrow=T))) ##convert it from list to numeric matrix and then take average by row
      # mtplot_avg=cbind(mtplot_avg,mturk_l) ##z scores
      # mturk_z=data.frame(mturk_z)

    mtplot_x1=cbind(matrix(unlist(mturk_z[2]),,nrow=111, byrow=T),mturk_l)
    colnames(mtplot_x1)=c('Zscores','vid','cate')
    mtplot_x2=cbind(matrix(unlist(mturk_z[1]),,nrow=111, byrow=T),mturk_l)
    colnames(mtplot_x2)=c('Zscores','vid','cate')
    mtplot_x3=cbind(matrix(unlist(mturk_z[4]),,nrow=111, byrow=T),mturk_l)
    colnames(mtplot_x3)=c('Zscores','vid','cate')
    mtplot_x4=cbind(matrix(unlist(mturk_z[6]),,nrow=111, byrow=T),mturk_l)
    colnames(mtplot_x4)=c('Zscores','vid','cate')
    mtplot_x5=cbind(matrix(unlist(mturk_z[3]),,nrow=111, byrow=T),mturk_l)
    colnames(mtplot_x5)=c('Zscores','vid','cate')
    mtplot_x6=cbind(matrix(unlist(mturk_z[5]),,nrow=111, byrow=T),mturk_l)
    colnames(mtplot_x6)=c('Zscores','vid','cate')

  
  ##plotting
  mturk_z_gavg$vid=factor(mtplot_avg$vid,levels=mtplot_avg$vid[order(mtplot_avg$AvgZscores)])
  p=ggplot(data=mtplot_avg, aes(x=vid, y=AvgZscores)) + geom_bar(aes(fill=cate),position="dodge",stat="identity",width=0.7)+coord_flip()
  ggsave(filename = 'RespZscores_avg_sorted.png',plot = p,device=png,dpi=300,width = 907,height = 1280,path='/Users/LondonLab/Google_Drive/DARPA/analysis_scripts/Plots_for_1-23-19Meeting/',limitsize = FALSE)
  
  mtplot_x3$vid=factor(mtplot_x3$vid,levels=mtplot_x3$vid[order(mtplot_x3$cate)])
  p=ggplot(data=mtplot_x3, aes(x=vid, y=Zscores)) + geom_bar(aes(fill=cate),position="dodge",stat="identity",width=0.7)  #+coord_flip()
  ggsave(filename = 'RespZscores_KnAg.png',plot = p,device=png,dpi=300,width = 907,height = 1280,path='/Users/LondonLab/Google_Drive/DARPA/analysis_scripts/Plots_for_1-23-19Meeting/',limitsize = FALSE)

  mtplot_avg_resp$vid=factor(mtplot_avg_resp$vid,levels=mtplot_avg_resp$vid[order(mtplot_avg_resp$Answer_Feel)])
  p=ggplot(data=mtplot_avg_resp, aes(x=vid, y=Answer_Feel)) +ylim(0,3) + geom_bar(aes(fill=cate),position="dodge",stat="identity",width=0.7)   #+coord_flip()
  ggsave(filename = 'RawRespScores_Feel_sorted.png',plot = p,device=png,dpi=300,width = 2860,height = 1280,path='/Users/LondonLab/Google_Drive/DARPA/analysis_scripts/Plots_for_1-23-19Meeting/',limitsize = FALSE)

  mt_z_plot$vid=factor(mt_z_plot$vid,levels=mt_z_plot$vid[order(mt_z_plot$Answer_Feel)])
  p=ggplot(data=mt_z_plot, aes(x=vid, y=Answer_Feel)) +ylim(0,1.5) + geom_bar(aes(fill=cate),position="dodge",stat="identity",width=0.7)   #+coord_flip()
  ggsave(filename = 'RawRespScores_Feel_sorted.png',plot = p,device=png,dpi=300,width = 2860,height = 1280,path='/Users/LondonLab/Google_Drive/DARPA/analysis_scripts/Plots_for_1-23-19Meeting/',limitsize = FALSE)


       #mturk_plot=cbind(mturk_d,mturk_l)
        #mturk_plot_srt=mturk_plot
        #mturk_plot_srt$vid_nums=factor(mturk_plot_srt$vid_nums,levels=mturk_plot_srt$vid_nums[order(mturk_plot_srt$cate)])
        #ggplot(data=mturk_plot_srt, aes(y=mean_composite_score, x=vid_nums)) +
         # geom_bar(aes(fill=cate),position="dodge",stat="identity",width=0.7)+coord_flip()
