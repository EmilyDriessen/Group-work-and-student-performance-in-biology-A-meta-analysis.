# Install metafor
install.packages("metafor")

#load metafor into Rstudio
library(metafor)

##load datasets

##datum 1 - Data 1SD Corr 0.9
datum1=read.csv(file.choose())

##datum 2 - Data 2SD Corr 0.9
datum2=read.csv(file.choose())

##datum 3 - Data 1SD Corr 0.5
datum3=read.csv(file.choose())

##datum 4 - Data 1SD Corr 0.1
datum4=read.csv(file.choose())

##datum 5 - Data 1SD Corr 0.9 - no weir
datum5=read.csv(file.choose())

##datum 6 - Data 1SD Corr 0.9 - no yapici
datum6=read.csv(file.choose())

##datum 7 - Data 1SD Corr 0.9 - no das - removed because graduate level class and I did not want this data point in the orchard plot for Figure 1. 
datum7=read.csv(file.choose())

##run random effects meta-analysis on full dataset (66 estimates) with paired designs set to correlation value of 0.90 to show comparative effect size + no random effects -   0.9987 with rma.mv - as included in the SI table - and 0.9074 with rma - used for TAF
results=rma.mv(yi, vi, data=datum1)
results7=rma(yi, vi, data=datum1)
summary(results)
summary(results7)
forest(results)
funnel(results)
funnel(results7)

### carry out trim-and-fill analysis - getting errors for this???? HELP
taf<-trimfill(results7)
summary(taf)


### draw funnel plot with missing studies filled in
funnel(taf, legend=FALSE)

## conduct a fail safe N Rosenthal test
fsn(yi, vi, data=datum1, type="Rosenthal", alpha=.05)

##run random effects meta-analysis on full dataset (66 estimates) with paired designs set to correlation value of 0.90 to show comparative effect size + random effects of author - yielded 0.72 
results1=rma.mv(yi, vi, data=datum1, random =  ~ 1 |Author)
summary(results1)

#run a forest plot on outcome of results1
forest(results1)

##Funnel plot
funnel(results1)

##run random effects meta-analysis on dataset without weir (40 estimates) with paired designs set to correlation value of 0.90 to show comparative effect size + author as random effect -  0.7071
results5=rma.mv(yi, vi, data=datum5, random =  ~ 1 |Author)
summary(results5)
forest(results5)
funnel(results5)

##run random effects meta-analysis on dataset without yapici (65 estimates) with paired designs set to correlation value of 0.90 to show comparative effect size + inputted 1SD + author as random effect -  0.6367
results6=rma.mv(yi, vi, data=datum6, random =  ~ 1 |Author)
summary(results6)
forest(results6)
funnel(results6)

##run random effects meta-analysis on full dataset (66 estimates) with paired designs set to correlation value of 0.90 and inputted SD doubled to show comparative effect size + author as random effect -  0.6447
results2=rma.mv(yi, vi, data=datum2, random =  ~ 1 |Author)
summary(results2)

##run random effects meta-analysis on full dataset (66 estimates) with paired designs set to correlation value of 0.50 and inputted 1SD to show comparative effect size + author as random effect -  0.7502
results3=rma.mv(yi, vi, data=datum3, random =  ~ 1 |Author)
summary(results3)

##run random effects meta-analysis on full dataset (66 estimates) with paired designs set to correlation value of 0.10 and inputted 1SD to show comparative effect size + author as random effect - 0.7523
results4=rma.mv(yi, vi, data=datum4, random =  ~ 1 |Author)
summary(results4)

##Orchard plot

#Load Libraries
install.packages("dplyr")
library(dplyr)
#install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
#install.packages("metafor")
library(metafor)
#install.packages("SciViews")
install.packages("SciViews")
library(SciViews)

##This section is to install the orchard plot package from github##
#install.packages("pacman")
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp)
devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)
library(orchaRd)
#http://www.i-deel.org/uploads/5/2/4/1/52416001/orchard_vignette.pdf

##Overall Orchard plot without gridlines and without I2 value with autosave as high dpi quality - use this one
plot=orchard_plot(results1, mod= "Intercept", xlab = "Hedges' g") + 
  xlim(-4.5, 4.5) +
  scale_y_discrete(labels=c(""))+
  ylab("Intercept") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),
        axis.text.y=element_text(angle=360),
        plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  scale_fill_manual(values ="red") + scale_colour_manual(values = "red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  legend.position = "none") +
  ggtitle("Overall")

##see plot
plot

##save plot
ggsave(plot, file="overall.jpg", dpi=300, width=5.6, height = 3)

##Class Size moderator orchard
results8=rma.mv(yi, vi, mods = ~class_size -1, method = "REML", random = ~ 1 | Author, data=datum1)
summary(results8)

##Create MS moderator class size plot
plot1=orchard_plot(results8, mod="Intercept", xlab="Hedges' g") +
  xlim(-4.5, 4.5) +
  scale_y_discrete(labels=c("Class_sizeLarge" = "Large", "Class_sizeMedium" = "Medium",
                            "Class_sizeSmall" = "Small")) +
  ylab("") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.y=element_text(),
        plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  scale_fill_manual(values=c("goldenrod", "gold", "lightgoldenrod")) + scale_colour_manual(values=c("goldenrod", "gold", "lightgoldenrod")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "none") +
  ggtitle("Class Size")

plot1
ggsave(plot1, file="classsize.jpg", dpi=300, width=5.6, height = 3)

##Create MS moderator class level plot
results9<- rma.mv(yi, vi, mods = ~class_level -1, method = "REML", random = ~ 1 | Author, data=datum7)
summary(results9)

##Removed Das et al from the data, so we would not have a graduate tag for class level. Only upper and lower. 
plot2=orchard_plot(results9, mod="Intercept", xlab="Hedges' g") + 
  xlim(-4.5, 4.5) +
  scale_y_discrete(labels=c("Class_levelLower" = "Lower", "Class_levelUpper" = "Upper")) +
  ylab("") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.y=element_text(),
        plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  scale_fill_manual(values=c("forestgreen", "limegreen")) + scale_colour_manual(values=c("forestgreen", "limegreen")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "none") +
  ggtitle("Class Level")

plot2

ggsave(plot2, file="classlevel.jpg", dpi=300, width=5.6, height = 3)

##Create MS moderator group size plot
results10<- rma.mv(yi, vi, mods = ~group_size -1, method = "REML", random = ~ 1 | Author, data=datum1)
summary(results10)

plot3=orchard_plot(results10, mod="Intercept", xlab="Hedges' g") +
  xlim(-4.5, 4.5) +
  scale_y_discrete(labels=c("Group_sizeLarge" = "Large", "Group_sizeMedium" = "Medium", "Group_sizeSmall" = "Small")) +
  ylab("") +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14, face="bold"),
    axis.text.y=element_text(),
    plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  scale_fill_manual(values=c("cyan4", "cyan3", "cyan2")) + scale_colour_manual(values=c("cyan4", "cyan3", "cyan2")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Group Size")

plot3
ggsave(plot3, file="groupsize.jpg", dpi=300, width=6.0, height = 3)

##Sensitivity analysis with research design analyzed as a moderator
results11=rma.mv(yi, vi, mods = ~design -1, method = "REML", random = ~ 1 | Author, data=datum1)
summary(results11)
plot4=orchard_plot(results11, mod="Intercept", xlab="Hedges' g") +
  xlim(-4.5, 4.5) +
  ylab("") +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14, face="bold"),
    axis.text.y=element_text(),
    plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  scale_fill_manual(values=c("cyan3", "cyan2")) + scale_colour_manual(values=c("cyan3", "cyan2")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Design")
plot4  

##Pairwise analysis
Design=rma.mv(yi, vi, mods = ~design, method = "REML", random = ~ 1 | Author, data=datum1)
summary(Design)

##Class level pairwise
classlevel=rma.mv(yi, vi, mods = ~class_level, method = "REML", random = ~ 1 | Author, data=datum7)
summary(classlevel)

##Class size pairwise
Classsize=rma.mv(yi, vi, mods = ~class_size, method = "REML", random = ~ 1 | Author, data=datum1)
summary(Classsize)

##Class size relevel
datum1$class_size=as.factor(datum1$class_size)
levels(datum1$class_size)
datum1$class_size=relevel(datum1$class_size, ref="Small")
Classsize2=rma.mv(yi, vi, mods = ~class_size, method = "REML", random = ~ 1 | Author, data=datum1)
summary(Classsize2)

##Group size pairwise
Groupsize= rma.mv(yi, vi, mods = ~group_size, method = "REML", random = ~ 1 | Author, data=datum1)
summary(Groupsize)

##relevel Group Size
datum1$group_size=as.factor(datum1$group_size)
levels(datum1$group_size)
datum1$group_size=relevel(datum1$group_size, ref="Small")
Groupsize2= rma.mv(yi, vi, mods = ~group_size, method = "REML", random = ~ 1 | Author, data=datum1)
summary(Groupsize2)


