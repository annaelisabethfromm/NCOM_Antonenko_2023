################"Stats_NCOMMS-22-47025" #################
###Author: Daria Antonenko, Anna E. Fromm, Ulrike Grittner  
#R version 4.1.2 (2021-11-01)
#Windows 7 x64 (build 7601) SP 1system   x86_64, mingw32ui  

############linear  models and correlations ###########

rm(list=ls())

library(openxlsx)
library(rstatix)
library(dplyr)
library(foreign)
library(emmeans) 
library(ggpubr)
library(jtools)
library(GGally)
library(readxl)
library(janitor)
library(lavaan)
library(lsr)
library(lattice)
library(ggplot2)
library(readr)
library(Rmisc)
library(devtools)
library(gghalves)
library(GGally)



setwd("YOUR PATH")

df1 <- read.xlsx("Source_Data.xlsx"
                 ,sheet = "All_data", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)

df1<-clean_names(df1)
head(df1)


#####################################*
#####################################*
##### Results Paragraph 1 ############## Title: White matter microstructure in individual prefrontal pathways is increased after brain stimulation.########
#####################################*
#####################################*

m1a =lm(fa_post10 ~ fa_pre10 + stim + age + sex , data=df1)
summary(m1a)
confint(m1a)
#anova(m1, m1a)
em1a<-emmeans(m1a,~stim)
em1a
pairs(em1a, adjust='none')

etaSquared(m1a,type=3)

#FINAL model for Trakt Vol#################
m2 =lm(post_l_mid_fg_tractvol_10 ~ pre_l_mid_fg_tractvol_10 + stim + age + sex + pre_l_mid_fg_tractvol_10*stim, data=df1)
summary(m2)

m2a =lm(post_l_mid_fg_tractvol_10 ~ pre_l_mid_fg_tractvol_10 + stim + age + sex, data=df1)
summary(m2a)
confint(m2a)
em2a<-emmeans(m2a,~stim)
em2a
pairs(em2a, adjust='none')

etaSquared(m2a,type=3)

#######RainCloud Figure 2: White Matter Pathways microstructure ##############
df1_long <-reshape(df1,
                   varying = c("fa_pre10", "fa_post10"),
                   v.names="FA",
                   timevar="Tract",
                   times=c("fa_pre10", "fa_post10"),
                   new.row.names = 1:10000, 
                   direction = "long")

df1_long <- subset(df1_long, select=c(id, stim, FA, Tract))
df0 <- df1_long %>%
  mutate(Session = case_when(
    grepl("pre", Tract) ~ "1",
    grepl("post", Tract) ~ "2"
  ))

df0 <- df0[with(df0, order(stim, Session)),]

Pre0 <- df0[which(df0$stim=="0"
                  & df0$Session=="1"), ]
Pre0 <- Pre0$FA

Post0 <- df0[which(df0$stim=="0"
                   & df0$Session=="2"), ]
Post0 <- Post0$FA
n0 <-length(Pre0)
d0 <-data.frame(y=c(Pre0, Post0),
                x=rep(c(4,5), each=n0),
                id =factor(rep(1:n0,2)))
set.seed(321)
d0$xj <-jitter(d0$x,amount=.09)
Pre1 <- df0[which(df0$stim=="1"
                  & df0$Session=="1"), ]
Pre1 <- Pre1$FA

Post1 <- df0[which(df0$stim=="1"
                   & df0$Session=="2"), ]
Post1 <- Post1$FA
n1 <-length(Pre1)
d1 <-data.frame(y=c(Pre1, Post1),
                x=rep(c(1,2), each=n1),
                id =factor(rep(1:n1,2)))
set.seed(321)
d1$xj <-jitter(d1$x,amount=.09)
score_mean_Pre0 <- mean(Pre0, na.rm=TRUE)
score_mean_Post0 <- mean(Post0, na.rm=TRUE)
y <- c(score_mean_Pre0, score_mean_Post0)
x <- c(4,5)
dfm0<- data.frame(y, x)
score_mean_Pre1 <- mean(Pre1, na.rm=TRUE)
score_mean_Post1 <- mean(Post1, na.rm=TRUE)
y <- c(score_mean_Pre1, score_mean_Post1)
x <- c(1,2)
dfm1 <- data.frame(y, x)


Fig2 <- ggplot(NULL, aes(y=y))+
  geom_point(data=d1 %>% filter(x=="1"), aes(x=xj), color="orange", size=6.0, alpha=.6)+
  geom_point(data=d1 %>% filter(x=="2"), aes(x=xj), color="red", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="4"), aes(x=xj), color="skyblue2", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="5"), aes(x=xj), color="dodgerblue", size=6.0, alpha=.6)+
  geom_point(data=dfm1 %>% filter(x=="1"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm1 %>% filter(x=="2"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="4"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="5"), aes(x=x), color="black", size=8, shape=18)+
  geom_segment(aes(x = 1, y =   score_mean_Pre1      , xend = 2, yend = score_mean_Post1        ), color= "black",size= 0.8)+
  geom_segment(aes(x = 4, y = score_mean_Pre0        , xend = 5, yend = score_mean_Post0       ), color= "black",size= 0.8)+
  geom_line(data=d1,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_line(data=d0,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_half_boxplot(
    data = d1 %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.51),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "orange")+
  geom_half_boxplot(
    data = d1 %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.4),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "red")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="4"), aes(x=x, y = y), position = position_nudge(x = 2.2),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "skyblue2")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="5"), aes(x=x, y = y), position = position_nudge(x = 1.32),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue")+
  geom_half_violin(
    data=d1 %>% filter(x=="1"),aes(x=x, y=y), position=position_nudge(x=-0.6),
    side="l", fill="orange", alpha=.5, color="orange", trim=TRUE)+
  geom_half_violin(
    data=d1 %>% filter(x=="2"),aes(x=x, y=y), position=position_nudge(x=-1.6),
    side="l", fill="red", alpha=.5, color="red", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="4"),aes(x=x, y=y), position=position_nudge(x=2.7),
    side="r", fill="skyblue2", alpha=.5, color="skyblue2", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="5"),aes(x=x, y=y), position=position_nudge(x=1.7),
    side="r", fill="dodgerblue", alpha=.5, color="dodgerblue", trim=TRUE)+
  scale_x_continuous(breaks=c(1,2,4,5), labels=c("Pre", "Post", "Pre", "Post"))+
  xlab("Time") +ylab("tract FA")+
  annotate("text", x = 1.5, y = 0.42, label = c("anodal"), fontface = 'italic')+
  annotate("text", x = 4.5, y = 0.42, label = c("sham"), fontface= 'italic')+
  labs(title = "White matter pathways' microstructure")+
  theme_classic()
Fig2


#####################################*
#####################################*
##### Results Paragraph 2 ############## Title: Grey matter microstructure is increased after brain stimulation when baseline values are low.########
#####################################*
#####################################*
#####################################*
df1$l_mid_pre_100<-df1$l_mid_fg_mul_cmfg_add_rmfg_pre*100
df1$l_mid_post_100<-df1$l_mid_fg_mul_cmfg_add_rmfg_post*100

#FINAL model MD in Stim Target
m3 =lm(l_mid_post_100 ~ l_mid_pre_100 + stim+ l_mid_pre_100*stim + sex + age, data=df1)
summary(m3) 
etaSquared(m3,type=3)
em3 <- emmeans(m3, ~stim|l_mid_pre_100, at=list(l_mid_pre_100=c(0.0988, 0.105,0.1128)))
em3
confint(pairs(em3, adjust='none'))
pairs(em3, adjust='none')

em3<-as.data.frame(em3)
em3$l_mid_pre_100<-if_else(em3$stim==0, em3$l_mid_pre_100-0.0008, em3$l_mid_pre_100)
em3$l_mid_pre_100<-if_else(em3$stim==1, em3$l_mid_pre_100+0.0008, em3$l_mid_pre_100)


ls3b<-emmeans(m3,~stim|l_mid_pre_100,at=list(l_mid_pre_100=c(0.09,0.115)),adjust='none' )#at 25/75 percentile
ls3b
options(digits=3)
confint(pairs(ls3b, adjust='none'))
pairs(ls3b,adjust='none')


#FINAL model Volume in Stim Target (m4a - without interaction)
m4 =lm(gm_vol_r_mfg_post ~ gm_vol_r_mfg_pre + stim + age + sex + gm_vol_r_mfg_pre*stim, data=df1)
summary(m4)

m4a =lm(gm_vol_r_mfg_post ~ gm_vol_r_mfg_pre + stim + age + sex , data=df1)
summary(m4a)
confint(m4a)
etaSquared(m4a,type=3)


#######RainCloud Figure 3 Grey Matter Microstructure ##############

df1_long <-reshape(df1,
                   varying = c("l_mid_fg_mul_cmfg_add_rmfg_pre", "l_mid_fg_mul_cmfg_add_rmfg_post"),
                   v.names="MD",
                   timevar="Tract",
                   times=c("l_mid_fg_mul_cmfg_add_rmfg_pre", "l_mid_fg_mul_cmfg_add_rmfg_post"),
                   new.row.names = 1:10000, 
                   direction = "long")

df1_long <- subset(df1_long, select=c(id, stim, MD, Tract))
# Adding column based on other column:
df0 <- df1_long %>%
  mutate(Session = case_when(
    grepl("pre", Tract) ~ "1",
    grepl("post",Tract) ~ "2"
  ))

df0 <- df0[with(df0, order(stim, Session)),]
Pre0 <- df0[which(df0$stim=="0"
                  & df0$Session=="1"), ]
Pre0 <- Pre0$MD

Post0 <- df0[which(df0$stim=="0"
                   & df0$Session=="2"), ]
Post0 <- Post0$MD

n0 <-length(Pre0)
d0 <-data.frame(y=c(Pre0, Post0),
                x=rep(c(4,5), each=n0),
                id =factor(rep(1:n0,2)))
set.seed(321)
d0$xj <-jitter(d0$x,amount=.09)


Pre1 <- df0[which(df0$stim=="1"
                  & df0$Session=="1"), ]
Pre1 <- Pre1$MD

Post1 <- df0[which(df0$stim=="1"
                   & df0$Session=="2"), ]
Post1 <- Post1$MD

n1 <-length(Pre1)
d1 <-data.frame(y=c(Pre1, Post1),
                x=rep(c(1,2), each=n1),
                id =factor(rep(1:n1,2)))
set.seed(321)
d1$xj <-jitter(d1$x,amount=.09)

score_mean_Pre0 <- mean(Pre0, na.rm=TRUE)
score_mean_Post0 <- mean(Post0, na.rm=TRUE)
y <- c(score_mean_Pre0, score_mean_Post0)
x <- c(4,5)
dfm0<- data.frame(y, x)

score_mean_Pre1 <- mean(Pre1, na.rm=TRUE)
score_mean_Post1 <- mean(Post1, na.rm=TRUE)
y <- c(score_mean_Pre1, score_mean_Post1)
x <- c(1,2)
dfm1 <- data.frame(y, x)


Fig3 <- ggplot(NULL, aes(y=y))+
  geom_point(data=d1 %>% filter(x=="1"), aes(x=xj), color="orange", size=3.0, alpha=.6)+
  geom_point(data=d1 %>% filter(x=="2"), aes(x=xj), color="red", size=3.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="4"), aes(x=xj), color="skyblue2", size=3.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="5"), aes(x=xj), color="dodgerblue", size=3.0, alpha=.6)+
  geom_point(data=dfm1 %>% filter(x=="1"), aes(x=x), color="black", size=5, shape=18)+
  geom_point(data=dfm1 %>% filter(x=="2"), aes(x=x), color="black", size=5, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="4"), aes(x=x), color="black", size=5, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="5"), aes(x=x), color="black", size=5, shape=18)+
  geom_segment(aes(x = 1, y = score_mean_Pre1      , xend = 2, yend = score_mean_Post1        ), color= "black",size= 0.8)+
  geom_segment(aes(x = 4, y = score_mean_Pre0        , xend = 5, yend = score_mean_Post0       ), color= "black",size= 0.8)+
  geom_line(data=d1,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_line(data=d0,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_half_boxplot(
    data = d1 %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.51),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "orange")+
  geom_half_boxplot(
    data = d1 %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.4),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "red")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="4"), aes(x=x, y = y), position = position_nudge(x = 2.2),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "skyblue2")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="5"), aes(x=x, y = y), position = position_nudge(x = 1.32),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue")+
  geom_half_violin(
    data=d1 %>% filter(x=="1"),aes(x=x, y=y), position=position_nudge(x=-0.6),
    side="l", fill="orange", alpha=.5, color="orange", trim=TRUE)+
  geom_half_violin(
    data=d1 %>% filter(x=="2"),aes(x=x, y=y), position=position_nudge(x=-1.6),
    side="l", fill="red", alpha=.5, color="red", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="4"),aes(x=x, y=y), position=position_nudge(x=2.7),
    side="r", fill="skyblue2", alpha=.5, color="skyblue2", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="5"),aes(x=x, y=y), position=position_nudge(x=1.7),
    side="r", fill="dodgerblue", alpha=.5, color="dodgerblue", trim=TRUE)+
  scale_x_continuous(breaks=c(1,2,4,5), labels=c("Pre", "Post", "Pre", "Post"))+
  xlab("Time") +ylab("GM MD")+
  annotate("text", x = 1.5, y = 0.0015, label = c("anodal"), fontface = 'italic')+
  annotate("text", x = 4.5, y = 0.0015, label = c("sham"), fontface= 'italic')+
  labs(title = "Grey Matter Microstructure")+
  theme_classic()
Fig3


#####################################*
#####################################*
##### Results Paragraph 3 ############## Title: Functional connectivity is increased after brain stimulation.########
#####################################*
#####################################*

#EXPLORATIVE model for FC
m5 =lm(cluster_post ~ cluster_pre + stim + age + sex + cluster_pre*stim, data=df1)
summary(m5)

m5a =lm(cluster_post ~ cluster_pre + stim + age + sex, data=df1)
summary(m5a)
confint(m5a)
em5a<-emmeans(m5a,~stim)
em5a
pairs(em5a, adjust='none')
etaSquared(m5a,type=3)

####Raincloud Figure 4: Seed-based functional connectivity ######
df1_long <-reshape(df1,
                   varying = c("cluster_pre", "cluster_post"),
                   v.names="Connectivity",
                   timevar="Cluster",
                   times=c("cluster_pre", "cluster_post"),
                   new.row.names = 1:10000, 
                   direction = "long")

df1_long <- subset(df1_long, select=c(id, stim, Connectivity, Cluster))

df0 <- df1_long %>%
  mutate(Session = case_when(
    grepl("pre", Cluster) ~ "1",
    grepl("post", Cluster) ~ "2"
  ))

df0 <- df0[with(df0, order(stim, Session)),]
Pre0 <- df0[which(df0$stim=="0"
                  & df0$Session=="1"), ]
Pre0 <- Pre0$Connectivity

Post0 <- df0[which(df0$stim=="0"
                   & df0$Session=="2"), ]
Post0 <- Post0$Connectivity
n0 <-length(Pre0)
d0 <-data.frame(y=c(Pre0, Post0),
                x=rep(c(4,5), each=n0),
                id =factor(rep(1:n0,2)))
set.seed(321)
d0$xj <-jitter(d0$x,amount=.09)

Pre1 <- df0[which(df0$stim=="1"
                  & df0$Session=="1"), ]
Pre1 <- Pre1$Connectivity

Post1 <- df0[which(df0$stim=="1"
                   & df0$Session=="2"), ]
Post1 <- Post1$Connectivity

n1 <-length(Pre1)
d1 <-data.frame(y=c(Pre1, Post1),
                x=rep(c(1,2), each=n1),
                id =factor(rep(1:n1,2)))
set.seed(321)
d1$xj <-jitter(d1$x,amount=.09)

score_mean_Pre0 <- mean(Pre0, na.rm=TRUE)
score_mean_Post0 <- mean(Post0, na.rm=TRUE)
y <- c(score_mean_Pre0, score_mean_Post0)
x <- c(4,5)
dfm0<- data.frame(y, x)
score_mean_Pre1 <- mean(Pre1, na.rm=TRUE)
score_mean_Post1 <- mean(Post1, na.rm=TRUE)
y <- c(score_mean_Pre1, score_mean_Post1)
x <- c(1,2)
dfm1 <- data.frame(y, x)

Fig4 <- ggplot(NULL, aes(y=y))+
  geom_point(data=d1 %>% filter(x=="1"), aes(x=xj), color="orange", size=6.0, alpha=.6)+
  geom_point(data=d1 %>% filter(x=="2"), aes(x=xj), color="red", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="4"), aes(x=xj), color="skyblue2", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="5"), aes(x=xj), color="dodgerblue", size=6.0, alpha=.6)+
  geom_point(data=dfm1 %>% filter(x=="1"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm1 %>% filter(x=="2"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="4"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="5"), aes(x=x), color="black", size=8, shape=18)+
  geom_segment(aes(x = 1, y =   0.0959      , xend = 2, yend = 0.2200        ), color= "black",size= 0.8, data = dfm1)+
  geom_segment(aes(x = 4, y = 0.174        , xend = 5, yend = 0.101       ), color= "black",size= 0.8, data = dfm0)+
  geom_line(data=d1,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_line(data=d0,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_half_boxplot(
    data = d1 %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.51),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "orange")+
  geom_half_boxplot(
    data = d1 %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.4),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "red")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="4"), aes(x=x, y = y), position = position_nudge(x = 2.2),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "skyblue2")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="5"), aes(x=x, y = y), position = position_nudge(x = 1.32),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue")+
  geom_half_violin(
    data=d1 %>% filter(x=="1"),aes(x=x, y=y), position=position_nudge(x=-0.6),
    side="l", fill="orange", alpha=.5, color="orange", trim=TRUE)+
  geom_half_violin(
    data=d1 %>% filter(x=="2"),aes(x=x, y=y), position=position_nudge(x=-1.6),
    side="l", fill="red", alpha=.5, color="red", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="4"),aes(x=x, y=y), position=position_nudge(x=2.7),
    side="r", fill="skyblue2", alpha=.5, color="skyblue2", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="5"),aes(x=x, y=y), position=position_nudge(x=1.7),
    side="r", fill="dodgerblue", alpha=.5, color="dodgerblue", trim=TRUE)+
  scale_x_continuous(breaks=c(1,2,4,5), labels=c("Pre", "Post", "Pre", "Post"))+
  xlab("Time") +ylab("sbFC lMFG-cluster")+
  annotate("text", x = 1.5, y = 0.6, label = c("anodal"), fontface = 'italic')+
  annotate("text", x = 4.5, y = 0.6, label = c("sham"), fontface= 'italic')+
  labs(title = "Seed-based functional connectivity")+
  theme_classic()
Fig4

##### Results Paragraph 4 ############## Title:Pathways' microstructural plasticity is associated with individual behavioral memory benefit.########

data <- read.xlsx("Source_Data.xlsx"
                         ,sheet = "All_data", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)

data$stim <-as.factor(data$stim)

lowerfun <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_point(size=1.5)+
    geom_smooth(aes(group=data$stim), method="lm", size=0.5)
   # stat_cor(method="spearman", cor.coef.name = "rho")
  # scale_x_continuous(limits = c(2000,20000))+
  #  scale_y_continuous(limits = c(2000,20000))
}  

my_dens <- function(data, mapping) {
  ggplot(data = data, mapping=mapping) +
    geom_density(  alpha = 0.7) 
}

columns = c("diff_POSTMINUSPRE_fa10", "lMidFG_mul_cmfg_add_rmfg_postMINUSpre1000", "clusterPOSTminusPRE","LU_diff_PostPre", "nback_perccorr_diff_PostPre")
plot1<-ggpairs(data,columns, upper=list(continuous=wrap("cor",size=3.5, method="spearman")),lower = list(continuous = lowerfun), diag =list(continuous = "barDiag"), mapping = aes(color = stim), columnLabels = c("FA Change", "MD Change", "FC Change", "LU Change", "N-back Change"))+
  ggplot2::theme_bw(base_size=9)
plot1



##### Results Supplements ############## 

data <- read.xlsx("Source_Data.xlsx"
                  ,sheet = '1', startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)

data$stim <-as.factor(data$stim)

lowerfun <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_point(size=1.5)+
    geom_smooth(aes(group=data$stim), method="lm", size=0.5)
  # stat_cor(method="spearman", cor.coef.name = "rho")
  # scale_x_continuous(limits = c(2000,20000))+
  #  scale_y_continuous(limits = c(2000,20000))
}  

my_dens <- function(data, mapping) {
  ggplot(data = data, mapping=mapping) +
    geom_density(  alpha = 0.7) 
}

columns = c("fa_pre10", "lMidFG_mul_cmfg_add_rmfg_pre", "clusterPre", "nback_perccorr_diff_PostPre")
plot2<-ggpairs(data,columns, upper=list(continuous=wrap("cor",size=3.5, method="spearman")),lower = list(continuous = lowerfun), diag =list(continuous = "barDiag"), mapping = aes(color = stim))+
  ggplot2::theme_bw(base_size=9)
plot2



###Table R1 Adverse Events#######

dat <- read.xlsx("Source_Data.xlsx"
                        ,sheet = "Suppl_AE", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)

dat$stim <- as.factor(dat$stim)
###

count(dat$stim)

########observation time in days (mean (SD))
mean(dat$period)
sd(dat$period)
aggregate(dat$period, list(dat$stim), FUN=mean)
aggregate(dat$period, list(dat$stim), FUN=sd)

#total ir
fit = glm(sum_ae ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

#means: anodal
fit1 = glm(sum_ae[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

#means sham
fit1 = glm(sum_ae[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits =1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(sum_ae ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))

###################################################################################
#itching
#total ir
fit = glm(itching ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

fit1 = glm(itching[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits =1)
round(exp(confint(fit1))*100, digits = 1)

fit1 = glm(itching[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(itching ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


###################################################################################
#pain
aggregate(dat$pain, list(dat$stim), FUN=sum)
#total ir
fit = glm(pain ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

fit1 = glm(pain[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

fit1 = glm(pain[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(pain ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


###################################################################################
#burning
aggregate(dat$burn, list(dat$stim), FUN=sum)
#total ir
fit = glm(burn ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)


fit1 = glm(burn[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)


fit1 = glm(burn[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(burn ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


###################################################################################
###warmth/heat

#total ir
fit = glm(heat ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

fit1 = glm(heat[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

fit1 = glm(heat[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(heat ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


###################################################################################
#metallic/iron taste
aggregate(dat$taste, list(dat$stim), FUN=sum)
#total ir
fit = glm(taste ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

fit1 = glm(taste[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

fit1 = glm(taste[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(taste ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


###################################################################################
#fatigue /decreased alertness
aggregate(dat$fatigue, list(dat$stim), FUN=sum)

#total ir
fit = glm(fatigue ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

fit1 = glm(fatigue[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

fit1 = glm(fatigue[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(fatigue ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


###################################################################################
#other
aggregate(dat$others, list(dat$stim), FUN=sum)
#total ir
fit = glm(others ~ offset(log(period)), data = dat, family = poisson(link = "log"))
round(exp(coef(fit))*100, digits = 1)
round(exp(confint(fit))*100, digits = 1)

fit1 = glm(others[stim =="1"] ~ offset(log(period[stim =="1"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

fit1 = glm(others[stim =="0"] ~ offset(log(period[stim =="0"])), data = dat, family = poisson(link = "log"))
round(exp(coef(fit1))*100, digits = 1)
round(exp(confint(fit1))*100, digits = 1)

# IRR
m1 = glm(others ~ relevel(stim,ref="0") + offset(log(period)), data = dat, family = poisson(link = "log"))
summary(m1)
exp(coef(m1))
exp(confint(m1))


participants_ae <- dat[ which(dat$sum_ae>'0'), ]
count(participants_ae$stim)


##### Table R2 Blinding ########
library(BI)

df1 <- read.xlsx("NCOMMS-22-47025_data_revision.xlsx"
                 ,sheet = "Suppl_blinding", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)


df1$stim <- as.factor(df1$stim)
df1$opinion <- as.factor(df1$opinion)

df1$opinion <- recode(df1$opinion, "1" ="1_active", "2"="2_placebo", "3"="3_dk")

df1_anodal <- df1[ which(df1$stim=='1'), ]
vector_anodal <- dplyr::count(df1_anodal, opinion)
names(vector_anodal)[names(vector_anodal) == "n"] <- "Treatment"

df1_sham <- df1[ which(df1$stim=='0'), ]
vector_sham <- dplyr::count(df1_sham, opinion)
names(vector_sham)[names(vector_sham) == "n"] <- "Placebo"

blinding_table <- merge(vector_anodal,vector_sham, all=TRUE)
blinding_table

x <- matrix(c(14, 10, 3, 3, 5, 13), nrow = 3, ncol = 2, byrow = TRUE)
BI(x)
BI(x, alternative.B = "greater")
BI(x, alternative.B = "less")


######### Table R4. Linear regression analysis for the two (behavioral variables)########
data <- read.xlsx("Source_Data.xlsx"
                  ,sheet = "All_data", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)


data$stim <-as.factor(data$stim)

m1 =lm(nback_perccorr_diff_PostPre ~ diff_POSTMINUSPRE_fa10 + lMidFG_mul_cmfg_add_rmfg_postMINUSpre1000 + clusterPOSTminusPRE, data=data)
summary(m1)

m2 =lm(LU_diff_PostPre ~ diff_POSTMINUSPRE_fa10 + lMidFG_mul_cmfg_add_rmfg_postMINUSpre1000 + clusterPOSTminusPRE, data=data)
summary(m2)

m3 =lm(lMidFG_mul_cmfg_add_rmfg_postMINUSpre1000 ~ diff_POSTMINUSPRE_fa10  + clusterPOSTminusPRE  , data=data)
summary(m3)



###### R5 TBSS######

df1 <- read.xlsx("Source_Data.xlsx"
                 ,sheet = "Suppl_TBSS", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)


df1_long <-reshape(df1,
                   varying = c("statsXskel_FA_pre", "statsXskel_FA_post"),
                   v.names="FA",
                   timevar="Tract",
                   times=c("statsXskel_FA_pre", "statsXskel_FA_post"),
                   new.row.names = 1:10000, 
                   direction = "long")

df1_long <- subset(df1_long, select=c(id, stim, FA, Tract))
df0 <- df1_long %>%
  mutate(Session = case_when(
    grepl("pre", Tract) ~ "1",
    grepl("post", Tract) ~ "2"
  ))

df0 <- df0[with(df0, order(stim, Session)),]

Pre0 <- df0[which(df0$stim=="0"
                  & df0$Session=="1"), ]
Pre0 <- Pre0$FA

Post0 <- df0[which(df0$stim=="0"
                   & df0$Session=="2"), ]
Post0 <- Post0$FA
n0 <-length(Pre0)
d0 <-data.frame(y=c(Pre0, Post0),
                x=rep(c(4,5), each=n0),
                id =factor(rep(1:n0,2)))
set.seed(321)
d0$xj <-jitter(d0$x,amount=.09)
Pre1 <- df0[which(df0$stim=="1"
                  & df0$Session=="1"), ]
Pre1 <- Pre1$FA

Post1 <- df0[which(df0$stim=="1"
                   & df0$Session=="2"), ]
Post1 <- Post1$FA
n1 <-length(Pre1)
d1 <-data.frame(y=c(Pre1, Post1),
                x=rep(c(1,2), each=n1),
                id =factor(rep(1:n1,2)))
set.seed(321)
d1$xj <-jitter(d1$x,amount=.09)
score_mean_Pre0 <- mean(Pre0, na.rm=TRUE)
score_mean_Post0 <- mean(Post0, na.rm=TRUE)
y <- c(score_mean_Pre0, score_mean_Post0)
x <- c(4,5)
dfm0<- data.frame(y, x)
score_mean_Pre1 <- mean(Pre1, na.rm=TRUE)
score_mean_Post1 <- mean(Post1, na.rm=TRUE)
y <- c(score_mean_Pre1, score_mean_Post1)
x <- c(1,2)
dfm1 <- data.frame(y, x)


ggplot(NULL, aes(y=y))+
  geom_point(data=d1 %>% filter(x=="1"), aes(x=xj), color="orange", size=6.0, alpha=.6)+
  geom_point(data=d1 %>% filter(x=="2"), aes(x=xj), color="red", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="4"), aes(x=xj), color="skyblue2", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="5"), aes(x=xj), color="dodgerblue", size=6.0, alpha=.6)+
  geom_point(data=dfm1 %>% filter(x=="1"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm1 %>% filter(x=="2"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="4"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="5"), aes(x=x), color="black", size=8, shape=18)+
  geom_segment(aes(x = 1, y =   score_mean_Pre1      , xend = 2, yend = score_mean_Post1        ), color= "black",size= 0.8)+
  geom_segment(aes(x = 4, y = score_mean_Pre0        , xend = 5, yend = score_mean_Post0       ), color= "black",size= 0.8)+
  geom_line(data=d1,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_line(data=d0,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_half_boxplot(
    data = d1 %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.51),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "orange")+
  geom_half_boxplot(
    data = d1 %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.4),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "red")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="4"), aes(x=x, y = y), position = position_nudge(x = 2.2),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "skyblue2")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="5"), aes(x=x, y = y), position = position_nudge(x = 1.32),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue")+
  geom_half_violin(
    data=d1 %>% filter(x=="1"),aes(x=x, y=y), position=position_nudge(x=-0.6),
    side="l", fill="orange", alpha=.5, color="orange", trim=TRUE)+
  geom_half_violin(
    data=d1 %>% filter(x=="2"),aes(x=x, y=y), position=position_nudge(x=-1.6),
    side="l", fill="red", alpha=.5, color="red", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="4"),aes(x=x, y=y), position=position_nudge(x=2.7),
    side="r", fill="skyblue2", alpha=.5, color="skyblue2", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="5"),aes(x=x, y=y), position=position_nudge(x=1.7),
    side="r", fill="dodgerblue", alpha=.5, color="dodgerblue", trim=TRUE)+
  scale_x_continuous(breaks=c(1,2,4,5), labels=c("Pre", "Post", "Pre", "Post"))+
  xlab("Time") +ylab("Xskel")+
  labs(title = "Xskel over Time")+
  theme_classic()




##### R 5 TRACULA##########
df1 <- read.xlsx("Source_Data.xlsx"
                 ,sheet = "Suppl_TRACULA", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE,   skipEmptyRows = TRUE,   skipEmptyCols = TRUE,   rows = NULL,   cols = NULL,   sep.names = ".",   na.strings = "NA",   fillMergedCells = FALSE)


############Corpus Callosum ##########
df1_long <-reshape(df1,
                   varying = c("FA_Avg_cc.bodypf_pre", "FA_Avg_cc.bodypf_post"),
                   v.names="FA",
                   timevar="Tract",
                   times=c("FA_Avg_cc.bodypf_pre", "FA_Avg_cc.bodypf_post"),
                   new.row.names = 1:10000, 
                   direction = "long")

df1_long <- subset(df1_long, select=c(id, stim, FA, Tract))
df0 <- df1_long %>%
  mutate(Session = case_when(
    grepl("pre", Tract) ~ "1",
    grepl("post", Tract) ~ "2"
  ))

df0 <- df0[with(df0, order(stim, Session)),]

Pre0 <- df0[which(df0$stim=="0"
                  & df0$Session=="1"), ]
Pre0 <- Pre0$FA

Post0 <- df0[which(df0$stim=="0"
                   & df0$Session=="2"), ]
Post0 <- Post0$FA
n0 <-length(Pre0)
d0 <-data.frame(y=c(Pre0, Post0),
                x=rep(c(4,5), each=n0),
                id =factor(rep(1:n0,2)))
set.seed(321)
d0$xj <-jitter(d0$x,amount=.09)
Pre1 <- df0[which(df0$stim=="1"
                  & df0$Session=="1"), ]
Pre1 <- Pre1$FA

Post1 <- df0[which(df0$stim=="1"
                   & df0$Session=="2"), ]
Post1 <- Post1$FA
n1 <-length(Pre1)
d1 <-data.frame(y=c(Pre1, Post1),
                x=rep(c(1,2), each=n1),
                id =factor(rep(1:n1,2)))
set.seed(321)
d1$xj <-jitter(d1$x,amount=.09)
score_mean_Pre0 <- mean(Pre0, na.rm=TRUE)
score_mean_Post0 <- mean(Post0, na.rm=TRUE)
y <- c(score_mean_Pre0, score_mean_Post0)
x <- c(4,5)
dfm0<- data.frame(y, x)
score_mean_Pre1 <- mean(Pre1, na.rm=TRUE)
score_mean_Post1 <- mean(Post1, na.rm=TRUE)
y <- c(score_mean_Pre1, score_mean_Post1)
x <- c(1,2)
dfm1 <- data.frame(y, x)


ggplot(NULL, aes(y=y))+
  geom_point(data=d1 %>% filter(x=="1"), aes(x=xj), color="orange", size=6.0, alpha=.6)+
  geom_point(data=d1 %>% filter(x=="2"), aes(x=xj), color="red", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="4"), aes(x=xj), color="skyblue2", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="5"), aes(x=xj), color="dodgerblue", size=6.0, alpha=.6)+
  geom_point(data=dfm1 %>% filter(x=="1"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm1 %>% filter(x=="2"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="4"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="5"), aes(x=x), color="black", size=8, shape=18)+
  geom_segment(aes(x = 1, y =   score_mean_Pre1      , xend = 2, yend = score_mean_Post1        ), color= "black",size= 0.8)+
  geom_segment(aes(x = 4, y = score_mean_Pre0        , xend = 5, yend = score_mean_Post0       ), color= "black",size= 0.8)+
  geom_line(data=d1,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_line(data=d0,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_half_boxplot(
    data = d1 %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.51),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "orange")+
  geom_half_boxplot(
    data = d1 %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.4),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "red")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="4"), aes(x=x, y = y), position = position_nudge(x = 2.2),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "skyblue2")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="5"), aes(x=x, y = y), position = position_nudge(x = 1.32),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue")+
  geom_half_violin(
    data=d1 %>% filter(x=="1"),aes(x=x, y=y), position=position_nudge(x=-0.6),
    side="l", fill="orange", alpha=.5, color="orange", trim=TRUE)+
  geom_half_violin(
    data=d1 %>% filter(x=="2"),aes(x=x, y=y), position=position_nudge(x=-1.6),
    side="l", fill="red", alpha=.5, color="red", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="4"),aes(x=x, y=y), position=position_nudge(x=2.7),
    side="r", fill="skyblue2", alpha=.5, color="skyblue2", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="5"),aes(x=x, y=y), position=position_nudge(x=1.7),
    side="r", fill="dodgerblue", alpha=.5, color="dodgerblue", trim=TRUE)+
  scale_x_continuous(breaks=c(1,2,4,5), labels=c("Pre", "Post", "Pre", "Post"))+
  xlab("Time") +ylab("cc bodypf FA")+
  #annotate("text", x = 1.5, y = 0.42, label = c("anodal"), fontface = 'italic')+
  #annotate("text", x = 4.5, y = 0.42, label = c("sham"), fontface= 'italic')+
  labs(title = "Corpus callosum prefrontal FA over Time")+
  theme_classic()


#######Models ##########
m1 =lm(FA_Avg_cc.bodypf_post ~ FA_Avg_cc.bodypf_pre + stim + age + sex + FA_Avg_cc.bodypf_pre*stim , data=df1)
summary(m1)
confint(m1)
em1a<-emmeans(m1,~stim)
em1a
pairs(em1, adjust='none')

etaSquared(m1,type=3)

#### Interaktion ####
quantile(df1$FA_Avg_cc.bodypf_pre)

ls_cc<-emmeans(m1,~stim*FA_Avg_cc.bodypf_pre,at=list(FA_Avg_cc.bodypf_pre=c(0.5163400,0.5655965)))#at 25/75 percentile
#effect and confidence interval
ls_cc
confint(pairs(ls_cc), adjust="none")
pairs(ls_cc, adjust="none")

######################### SLF 2######################
df1_long <-reshape(df1,
                   varying = c("FA_Avg_lh.slft2_pre", "FA_Avg_lh.slft2_post"),
                   v.names="FA",
                   timevar="Tract",
                   times=c("FA_Avg_lh.slft2_pre", "FA_Avg_lh.slft2_post"),
                   new.row.names = 1:10000, 
                   direction = "long")

df1_long <- subset(df1_long, select=c(id, stim, FA, Tract))
df0 <- df1_long %>%
  mutate(Session = case_when(
    grepl("pre", Tract) ~ "1",
    grepl("post", Tract) ~ "2"
  ))

df0 <- df0[with(df0, order(stim, Session)),]

Pre0 <- df0[which(df0$stim=="0"
                  & df0$Session=="1"), ]
Pre0 <- Pre0$FA

Post0 <- df0[which(df0$stim=="0"
                   & df0$Session=="2"), ]
Post0 <- Post0$FA
n0 <-length(Pre0)
d0 <-data.frame(y=c(Pre0, Post0),
                x=rep(c(4,5), each=n0),
                id =factor(rep(1:n0,2)))
set.seed(321)
d0$xj <-jitter(d0$x,amount=.09)
Pre1 <- df0[which(df0$stim=="1"
                  & df0$Session=="1"), ]
Pre1 <- Pre1$FA

Post1 <- df0[which(df0$stim=="1"
                   & df0$Session=="2"), ]
Post1 <- Post1$FA
n1 <-length(Pre1)
d1 <-data.frame(y=c(Pre1, Post1),
                x=rep(c(1,2), each=n1),
                id =factor(rep(1:n1,2)))
set.seed(321)
d1$xj <-jitter(d1$x,amount=.09)
score_mean_Pre0 <- mean(Pre0, na.rm=TRUE)
score_mean_Post0 <- mean(Post0, na.rm=TRUE)
y <- c(score_mean_Pre0, score_mean_Post0)
x <- c(4,5)
dfm0<- data.frame(y, x)
score_mean_Pre1 <- mean(Pre1, na.rm=TRUE)
score_mean_Post1 <- mean(Post1, na.rm=TRUE)
y <- c(score_mean_Pre1, score_mean_Post1)
x <- c(1,2)
dfm1 <- data.frame(y, x)


ggplot(NULL, aes(y=y))+
  geom_point(data=d1 %>% filter(x=="1"), aes(x=xj), color="orange", size=6.0, alpha=.6)+
  geom_point(data=d1 %>% filter(x=="2"), aes(x=xj), color="red", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="4"), aes(x=xj), color="skyblue2", size=6.0, alpha=.6)+
  geom_point(data=d0 %>% filter(x=="5"), aes(x=xj), color="dodgerblue", size=6.0, alpha=.6)+
  geom_point(data=dfm1 %>% filter(x=="1"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm1 %>% filter(x=="2"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="4"), aes(x=x), color="black", size=8, shape=18)+
  geom_point(data=dfm0 %>% filter(x=="5"), aes(x=x), color="black", size=8, shape=18)+
  geom_segment(aes(x = 1, y =   score_mean_Pre1      , xend = 2, yend = score_mean_Post1        ), color= "black",size= 0.8)+
  geom_segment(aes(x = 4, y = score_mean_Pre0        , xend = 5, yend = score_mean_Post0       ), color= "black",size= 0.8)+
  geom_line(data=d1,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_line(data=d0,aes(x=xj, group=id), color="gray", alpha=.5)+
  geom_half_boxplot(
    data = d1 %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.51),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "orange")+
  geom_half_boxplot(
    data = d1 %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.4),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "red")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="4"), aes(x=x, y = y), position = position_nudge(x = 2.2),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "skyblue2")+
  geom_half_boxplot(
    data = d0 %>% filter(x=="5"), aes(x=x, y = y), position = position_nudge(x = 1.32),
    side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue")+
  geom_half_violin(
    data=d1 %>% filter(x=="1"),aes(x=x, y=y), position=position_nudge(x=-0.6),
    side="l", fill="orange", alpha=.5, color="orange", trim=TRUE)+
  geom_half_violin(
    data=d1 %>% filter(x=="2"),aes(x=x, y=y), position=position_nudge(x=-1.6),
    side="l", fill="red", alpha=.5, color="red", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="4"),aes(x=x, y=y), position=position_nudge(x=2.7),
    side="r", fill="skyblue2", alpha=.5, color="skyblue2", trim=TRUE)+
  geom_half_violin(
    data=d0 %>% filter(x=="5"),aes(x=x, y=y), position=position_nudge(x=1.7),
    side="r", fill="dodgerblue", alpha=.5, color="dodgerblue", trim=TRUE)+
  scale_x_continuous(breaks=c(1,2,4,5), labels=c("Pre", "Post", "Pre", "Post"))+
  xlab("Time") +ylab("left SLF2 FA")+
  #annotate("text", x = 1.5, y = 0.42, label = c("anodal"), fontface = 'italic')+
  #annotate("text", x = 4.5, y = 0.42, label = c("sham"), fontface= 'italic')+
  labs(title = "left SLF2 FA over Time")+
  theme_classic()


############Model ######
m2 =lm(FA_Avg_lh.slft2_post ~ FA_Avg_lh.slft2_pre + stim + age + sex + FA_Avg_lh.slft2_pre*stim, data=df1)
summary(m2)
confint(m2)
em2<-emmeans(m2,~stim)
em2
pairs(em2, adjust='none')
etaSquared(m2,type=3)



