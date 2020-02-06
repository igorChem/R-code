library(ggpubr)

#=======================================================================

# ------------------- Loading Data ----------------------#

ff       <-read.table("foa_foa",header=T)
fd       <-read.table("foa_fd",header=T)
fdfd     <-read.table("fd_fd",header=T)
foa_fd_o <-read.table("foa_fd_o",header=T)

bdc <-read.table("band_comp",header=T)

ffdf <- data.frame(ff) 
fddf <-data.frame(fd)
fdfddf <-data.frame(fdfd)
foa_fdodf <-data.frame(foa_fd_o)

bdc_df <-data.frame(bdc)

#=======================================================================

attach(ffdf)

#***********************************************************************
p<-ggplot(ffdf, aes(x=SEH, y=EAS,fill=Peptide)) +
  geom_bar(stat="identity",position=position_dodge())+
                labs(y = "EAS",x ="")+
                scale_fill_manual(values=c('black','red','orange','blue','cyan','yellow',
                'green','purple'))

p2<-ggplot(ffdf, aes(x=SEH, y=NAS,fill=Peptide)) +
  geom_bar(stat="identity",position=position_dodge())+
                labs(x = " ",y = "NAS")+
                scale_fill_manual(values=c('black','red','orange','blue','cyan','yellow',
                'green','purple'))

p3<-ggplot(ffdf, aes(x=SEH, y=RAS,fill=Peptide)) +
  geom_bar(stat="identity",position=position_dodge())+
                labs(x = " ",y = "RAS")+
                scale_fill_manual(values=c('black','red','orange','blue','cyan','yellow',
                'green','purple'))
                
p4 <-ggplot(ffdf, aes(x=SEH, y=DualFukui,fill=Peptide)) +
  geom_bar(stat="identity",position=position_dodge())+
                labs(x = " ",y = "Dual Fukui")+
                scale_fill_manual(values=c('black','red','orange','blue','cyan','yellow',
                'green','purple'))
#_______________________________________________________________________
ras_mean_ff_s <-aggregate(RAS~SEH,FUN=mean)
ras_sd_ff_s <-aggregate(RAS~SEH,FUN=sd)
ras_mean_ff_p <-aggregate(RAS~Peptide,FUN=mean)
ras_sd_ff_p <-aggregate(RAS~Peptide,FUN=sd)

df_ras_bar_ff_s <-data.frame(ras_mean_ff_s,ras_sd_ff_s)
df_ras_bar_ff_p <-data.frame(ras_mean_ff_p,ras_sd_ff_p)

#------------------------------------------------------
#plot by semiempitical method
detach(ffdf)
attach(df_ras_bar_ff_s)
          
p<- ggplot(df_ras_bar_ff_s, aes(x=SEH, y=RAS)) + 
  geom_bar(stat="identity", color="black",fill="lightgreen", 
           position=position_dodge()) +
           coord_flip()+
  geom_errorbar(aes(ymin=RAS-RAS.1, ymax=RAS+RAS.1), width=.2,
                 position=position_dodge(.9)) 
            
png("ras_mean_by_seh_foa_foa",width = 4, height = 6, units = 'in', res = 400)
p
dev.off()
detach(df_ras_bar_ff_s)
#-----------------------------------------------------------------------
attach(df_ras_bar_ff_p)
#plots by peptide
p<- ggplot(df_ras_bar_ff_p, aes(x=Peptide, y=RAS)) + 
  geom_bar(stat="identity", color="black",fill="lightgreen", 
           position=position_dodge()) +
           coord_flip()+
  geom_errorbar(aes(ymin=RAS-RAS.1, ymax=RAS+RAS.1), width=.2,
                 position=position_dodge(.9)) 
            
png("ras_mean_by_peptide_foa_foa",width = 3.5, height = 5, units = 'in', res = 1000)
p
dev.off()
detach(df_ras_bar_ff_p)
#_______________________________________________________________________

#============================================
# foa_fd
attach(fddf)
ras_mean_fd_s <-aggregate(RAS~SEH,FUN=mean)
ras_sd_fd_s <-aggregate(RAS~SEH,FUN=sd)
ras_mean_fd_p <-aggregate(RAS~Peptide,FUN=mean)
ras_sd_fd_p <-aggregate(RAS~Peptide,FUN=sd)

df_ras_bar_fd_s <-data.frame(ras_mean_fd_s,ras_sd_fd_s)
df_ras_bar_fd_p <-data.frame(ras_mean_fd_p,ras_sd_fd_p)

detach(fddf)
attach(df_ras_bar_fd_s)
p<- ggplot(df_ras_bar_fd_s, aes(x=SEH, y=RAS)) + 
  geom_bar(stat="identity", color="black",fill="lightgreen", 
           position=position_dodge()) +
           coord_flip()+
  geom_errorbar(aes(ymin=RAS-RAS.1, ymax=RAS+RAS.1), width=.2,
                 position=position_dodge(.9)) 
            
png("ras_mean_by_seh_foa_fd",width = 3.5, height = 5, units = 'in', res = 400)
p
dev.off()

#============================================
# fd_fd plots by peptide
attach(fdfddf)
ras_mean_fd_p <-aggregate(RAS~Peptide,FUN=mean)
ras_sd_fd_p <-aggregate(RAS~Peptide,FUN=sd)

df_ras_bar_fd_p <-data.frame(ras_mean_fd_p,ras_sd_fd_p)
detach(fdfddf)
attach(df_ras_bar_fd_p)

p<- ggplot(df_ras_bar_fd_p, aes(x=Peptide, y=RAS)) + 
  geom_bar(stat="identity", color="black",fill="lightgreen", 
           position=position_dodge()) +
           coord_flip()+
  geom_errorbar(aes(ymin=RAS-RAS.1, ymax=RAS+RAS.1), width=.2,
                 position=position_dodge(.9)) 
            
png("ras_mean_by_peptide_fd_fd",width = 3.5, height = 5, units = 'in', res = 1000)
p
dev.off()
#============================================
# foa_fd plots by peptide
attach(foa_fdodf)
ras_mean_foa_fd_p <-aggregate(RAS~Peptide,FUN=mean)
ras_sd_foa_fd_p <-aggregate(RAS~Peptide,FUN=sd)

df_ras_bar_foa_fd_p <-data.frame(ras_mean_foa_fd_p,ras_sd_foa_fd_p)
detach(foa_fdodf)
attach(df_ras_bar_foa_fd_p)

p<- ggplot(df_ras_bar_foa_fd_p, aes(x=Peptide, y=RAS)) + 
  geom_bar(stat="identity", color="black",fill="lightgreen", 
           position=position_dodge()) +
            coord_flip()+
  geom_errorbar(aes(ymin=RAS-RAS.1, ymax=RAS+RAS.1), width=.2,
                 position=position_dodge(.9)) 
            
png("ras_mean_by_peptide_foa_fd",width = 3.5, height = 5, units = 'in', res = 1000)
p
dev.off()
#============================================
attach(bdc_df)

p<-ggplot(bdc_df, aes(x=Peptide, y=RAS,fill=type))+
  geom_bar(stat="identity",color="black",position=position_dodge())+
                labs(y = "RAS",x ="Peptide")+
                 scale_fill_manual(values=c("yellow","lightgreen"))+
                 coord_flip()
png("band",width = 4.5, height = 5, units = 'in', res = 400)
p
dev.off()
