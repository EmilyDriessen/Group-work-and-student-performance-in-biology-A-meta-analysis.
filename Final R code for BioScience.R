# Install metafor
install.packages("metafor")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("SciViews")
install.packages("pacman")
devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
               ape, phytools, flextable)

#load metafor into Rstudio
library(metafor)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(metafor)
library(SciViews)

##load datasets by opening files from desktop: "DatumRMod is used for the majority of these analyses. Datum Corr 0.1, Datum Corr 0.5, Datum Cor 0.9, and Datum2SD are used for the specific sensitivity analysis for which they are named. 

#use DatumRMod to calculate Hedges' G and Variance for 1SD for Paired Design
#First create a subset of the datum for just paired experimental designs
datumpaired<-subset(DatumRMod, Design == 'Paired')
Datum1<-metafor::escalc(measure="SMD", n1i=Control.postN, n2i=Group.preN, m1i=Control.postMean, m2i=Group.preMean, sd1i=Control.postSD, sd2i=Group.preSD, ri="Correlation",
       data=datumpaired)

#use Datum to calculate Hedges' G and Variance for 1SD for Independent Design
datumindependent<-subset(DatumRMod, Design == 'Independent')
Datum2<-metafor::escalc(measure="SMD", n1i=Group.preN, n2i=Control.postN, m1i=Group.preMean, m2i=Control.postMean, sd1i=Group.preSD, sd2i=Control.postSD,
                      data=datumindependent)

#After calculating Hedges' g and variance for both paired and independent design type experiments, I combined both datasets in Excel to make: DatumRMod.

##Now that I have my dataset it is time to run the linear models to find the overall estimate of the effect of group work on student performance. 

##run random effects meta-analysis on full dataset to show comparative effect size + no random effects -   
results=rma(Yi, Vi, data=DatumRMod)
summary(results)
forest(results)
funnel (results, digits= c(0.2, 1), back="white", hlines = "black", xlab="Hedges' g", ylab = "Standard Error")

F1<-funnel(results, digits= c(0.2, 1), back="white", hlines = "black", xlab="Hedges' g", ylab = "Standard Error")
ggsave(F1, file="funnelplotraw.png", width=10, height=5, dpi=600)

### carry out trim-and-fill analysis 
taf<-trimfill(results)
summary(taf)
funnel(taf, digits= c(0.2, 1), back="white", hlines = "black", xlab="Hedges' g", ylab = "Standard Error")
#No dots were added to this plot, so we know there is no difference. No studies filled in. This suggests confidence in our dataset. 

### run rma.mv and use this as the estimate. I only ran an rma without the mv previously, so I could conduct a trim and fill, but there were no datapoints filled in, so I'm proceeding with the rma.mv
##This is with total dataset, with inputted 1SD 
results=rma.mv(Yi, Vi, data=DatumRMod)
summary(results)
forest(results)
funnel(results)

## conduct a fail safe N Rosenthal test - 179395 fail safe N. 
fsn(Yi, Vi, data=DatumRMod, type="Rosenthal", alpha=.05)

##Add in random effect of author##
results1=rma.mv(Yi, Vi, data=DatumRMod, random =  ~ 1 |Reference)
summary(results1)
forest.rma (results1, order = "yi")
                                                                                                                      round(I2[1] * 100, 2)), color = "black", parse = TRUE, size = 5) + annotate(geom = "text", x = 1.9, y = 79.5, label = "%", color = "black", parse = FALSE, size = 5)
##Now that I have the overall estimate, it is time to run Sensitivity Analyses##

##run random effects meta-analysis on dataset without especially large estimates (remove any with estimates over  X SD) to show comparative effect size + inputted 1SD + author as random effect 
##calculate SD of estimates##
sqrt(var(DatumRMod$Yi)) #this gives us a 1.20 SD, given that the overall mean estimate is 1.003, any estimate over 3.40 or under -1.40. However, the negative limit never was reached, so we are going to eliminate any estimates from this subset analysis that are over 3.4
#check that taking the square root of variance actually give us the standard deviation
#subset data
#Check
sd(DatumRMod$Yi)
##so, it is correct. 
filtered_Nolargeestimates <- DatumRMod %>% 
  filter(Yi < 3.40)
results2=rma.mv(Yi, Vi, data=filtered_Nolargeestimates, random =  ~ 1 |Reference)
summary(results2)
forest(results2)
funnel(results2, digits= c(0.2, 1), back="white", hlines = "black", xlab="Hedges' g", ylab = "Standard Error")

results3=rma(Yi, Vi, data=filtered_Nolargeestimates)
funnel(results3, digits= c(0.2, 1), back="white", hlines = "black", xlab="Hedges' g", ylab = "Standard Error")
### carry out trim-and-fill analysis 
taf<-trimfill(results3)
summary(taf)
funnel(taf, digits= c(0.2, 1), back="white", hlines = "black", xlab="Hedges' g", ylab = "Standard Error")
#No dots were added to this plot, so we know there is no difference. No studies filled in. This suggests confidence in our dataset.

#Run fail safe N - 169, 518 articles, when outliers are removed
fsn(Yi, Vi, data=filtered_Nolargeestimates, type="Rosenthal", alpha=.05)

##run random effects meta-analysis on dataset without paired research design estimate  + author as random effect
#subsetdata
filtered_Nopaired <- DatumRMod %>% 
  filter(Design != "Paired")
results4=rma.mv(Yi, Vi, data=filtered_Nopaired, random =  ~ 1 |Reference)
summary(results4)


##run random effects meta-analysis on dataset without Weir (removed estimates from Meta-analysis that came from Weir study) to show comparative effect size + author as random effect - 0.9537
#subset data#
filtered_NoWeir <- DatumRMod %>% 
  #filter(Reference !="Weir, Laura K. et al. 2019. “Small Changes, Big Gains: A Curriculum-Wide Study of Teaching Practices and Student Learning in Undergraduate Biology.” PLoS ONE 14(8): 1–16.")
  filter(!row_number() %in% c(66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91))
results5=rma.mv(Yi, Vi, data=filtered_NoWeir, random =  ~ 1 |Reference)
summary(results5)
forest(results5)
funnel(results5)

##run random effects meta-analysis on dataset without the estimates with inputted SD + author as random effect 
filtered_NoinputtedSD <- DatumRMod %>% 
  filter(!row_number() %in% c(5, 8, 13, 14, 15, 18, 22, 32, 34, 46, 48, 49, 52, 53, 57))
results6=rma.mv(Yi, Vi, data=filtered_NoinputtedSD, random =  ~ 1 |Reference)
summary(results6)

##run random effects meta-analysis on full dataset (91 estimates) with paired designs set to correlation value of 0.90 and inputted 1SD + author as random effect
results7=rma.mv(Yi, Vi, data=Datum.corr.0.9, random =  ~ 1 |Reference)
summary(results7)

##run random effects meta-analysis on full dataset (91 estimates) with paired designs set to correlation value of 0.50 and inputted 1SD to show comparative effect size + author as random effect 
##Change correlation value first##
results8=rma.mv(Yi, Vi, data=Datum.Corr.0.5, random =  ~ 1 |Reference)
summary(results8)

##run random effects meta-analysis on full dataset (91 estimates) with paired designs set to correlation value of 0.10 and inputted 1SD to show comparative effect size + author as random effect 
results9=rma.mv(Yi, Vi, data=Datum.Corr.0.1, random =  ~ 1 |Reference)
summary(results9)

#run analysis with inputted SD doubled (multiplied by 2) + author as random effect. 
##Coding my moderators
results10=rma.mv(Yi, Vi, data=Datum2SD, random =  ~ 1 |Reference)
summary(results10)

#run analysis for instructor same versus not same 
results11<- rma.mv(Yi, Vi, mods = ~InstructorSame -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results11)

InstructorYes<-subset(DatumRMod, InstructorSame == 'Yes')

InstructorNo<-subset(DatumRMod, InstructorSame == 'No')

#run analysis with experiments with same instructor
SameInstructor <- DatumRMod %>% 
  filter(!row_number() %in% c(1, 6, 9, 10, 11, 13, 14, 16, 19, 21, 23, 25, 27, 28, 29, 30, 31, 32, 34, 35, 36))
results12=rma.mv(Yi, Vi, data=SameInstructor, random =  ~ 1 |Reference)
summary(results12)

#run analysis for only experiments that had comparable groups
ComparableGroups <- DatumRMod %>% 
  filter(!row_number() %in% c(1, 2, 8, 11, 13, 14, 16, 21, 23, 26, 27, 35))
results13=rma.mv(Yi, Vi, data=ComparableGroups, random =  ~ 1 |Reference)
summary(results13)

#run analysis for preperformancesame for treatment and control groups, yes versus no
results14<- rma.mv(Yi, Vi, mods = ~PrePerformanceSame -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results14)

PreperformanceNo<-subset(DatumRMod, InstructorSame == 'Yes')

PreperformanceYes<-subset(DatumRMod, InstructorSame == 'No')

#run analysis for majors versus nonmajors
results15<- rma.mv(Yi, Vi, mods = ~Majors -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results15)

BioMajorsYes<-subset(DatumRMod, Majors == 'Yes')
BioMajorsNo<-subset(DatumRMod, Majors == 'No')

#run analysis for experimental design
results16<- rma.mv(Yi, Vi, mods = ~Design -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results16)

DesignI<-subset(DatumRMod, Design == 'Independent')
DesignP<-subset(DatumRMod, Design == 'Paired')

#run analysis for duration of group Type
results17<- rma.mv(Yi, Vi, mods = ~GroupType -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results17)

#run analysis for duration of group work
results18<- rma.mv(Yi, Vi, mods = ~Duration -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results18)

Onehour<-subset(DatumRMod, Duration == '1 hour to four weeks')
CourseLong<-subset(DatumRMod, Duration == 'Semester long')

#run analysis for group assigned
#remove 16
Assigned <- DatumRMod %>% 
  filter(!row_number() %in% c(16))

results19<- rma.mv(Yi, Vi, mods = ~Group.Assigned -1, method = "REML", random = ~ 1 | Reference, data=Assigned)
summary(results19)

Assigned<-subset(DatumRMod, Group.Assigned == 'Yes')
NotAssigned<-subset(DatumRMod, Group.Assigned == 'No')

#graded or not
results20<- rma.mv(Yi, Vi, mods = ~Graded -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results20)

Gradedyes<-subset(DatumRMod, Graded == 'Yes')
GradedNo<-subset(DatumRMod, Graded == 'No')

#Now I use DatumR, which contains the raw numbers for moderators, including group size and class size, so I can figure out where to draw the lines for my moderator categories. 
#First, I want to figure out the categories for group size, so I'm making a histogram.
hist(DatumR$GroupSize, xlab="Group Size", xlim = c(0,14), ylim = c(0, 20), breaks = 12, main = NULL)

#make a cumulative density plot, so you can look at the y-axis and draw lines at 1/3 and 2/3 of the frequencyies for the cumulative distribution. 
CDF<-ecdf(DatumR$GroupSize)
plot(CDF, xlab="Group Size", ylab="Cumulative Frequency")

#Next, let's create categories for class size. 
DatumR<-DatumR %>% mutate_if(is.character, as.numeric)
summary(DatumR)

hist(DatumR$ClassSize, xlab="Class Size", breaks = 50, main = NULL)
CDF<-ecdf(DatumR$ClassSize)
plot(CDF, xlab="Class Size", ylab="Cumulative Frequency")


##Overall Orchard plot without gridlines and without I2 value with autosave as high dpi quality - use this one
orchaRd::orchard_plot(results1, mod = "1", group = "Reference", xlab = "Hedges' g", transfm = "none") + 
  #xlim(-1, 3.5) +
  #scale_y_discrete(labels=c(""))+
  ylab("Intercept") +
  theme(#axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),
        axis.text.y=element_text(angle=360),
        plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  scale_fill_manual(values ="red") + scale_colour_manual(values = "red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Overall")

##This plot is with the large estimates from Yapici and Yang removed becuase they made the plot tiny, since they were such large outliers. 

##save plot
ggsave(plot, file="overall.jpg", dpi=300, width=5.6, height = 3)

#caterpillars
orchaRd::caterpillars(results1, mod = "1", group = "Reference", xlab = "Hedges' g", transfm = "none") +
  xlim(-6, 12)

#Moderator Figures. 

#Class Size
#Of note, I took the values from the linear model below to make a dataset for the moderator
results21=rma.mv(Yi, Vi, mods = ~ClassSize -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results21)
##Class Size moderator Figure
Size<- c("Small", "Medium", "Large")
Effect<- c(1.00, 0.98, 1.42)
UpperCI<- c (1.42, 1.37, 1.82)
LowerCI<- c(0.57, 0.58, 1.02)
Classsizedatum<-data.frame(Size, Effect, UpperCI, LowerCI)

ggplot(Classsizedatum, 
  aes(x = Effect, y = Size, xmin = LowerCI, xmax = UpperCI))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Class Size") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))

#Group Size Moderator PLot
#Moderator Figures. Of note, I took the values from the linear model below to make a datasat for the moderator
results22=rma.mv(Yi, Vi, mods = ~GroupSize -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results22)
Size<- c("Small", "Medium", "Large")
Effect<- c(1.14, 1.34, 0.93)
UpperCI<- c (1.84, 1.95, 1.53)
LowerCI<- c(0.43, 0.73, 0.34)
Groupsizedatum<-data.frame(Size, Effect, UpperCI, LowerCI)

ggplot(Groupsizedatum, 
       aes(x = Effect, y = Size, xmin = LowerCI, xmax = UpperCI))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Group Size") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))

##Create MS moderator class level plot
results22<- rma.mv(yi, vi, mods = ~class_level -1, method = "REML", random = ~ 1 | Author, data=datum7)
summary(results22)

#Class Level
datumnograd<-subset(DatumRMod, Level == c("Lower", "Upper"))
results23=rma.mv(Yi, Vi, mods = ~Level -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results23)

#Class Level Moderator PLot
levelsclass<- c("ZLower", "XUpper", "Graduate")
Effect2<- c(1.07, 0.88, 0.33)
UpperCI2<- c (1.45, 1.40, 1.30)
LowerCI2<- c(0.69, 0.36, -0.64)
ClassLeveldatum<-data.frame(levelsclass, Effect2, UpperCI2, LowerCI2)

ggplot(ClassLeveldatum, 
       aes(x = Effect2, y = levelsclass, xmin = LowerCI2, xmax = UpperCI2))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Class Level") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))

#Majors or nonmajors plot
results24=rma.mv(Yi, Vi, mods = ~Majors -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results24)
Maj<- c("Majors", "Nonmajors")
Effect3<- c(0.83, 0.91)
UpperCI3<- c (1.26, 1.70)
LowerCI3<- c(0.40, 0.13)
Majornomaj<-data.frame(Maj, Effect3, UpperCI3, LowerCI3)

ggplot(Majornomaj, 
       aes(x = Effect3, y = Maj, xmin = LowerCI3, xmax = UpperCI3))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Majors or NonMajors") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))

#Group duration moderator plot
results25=rma.mv(Yi, Vi, mods = ~Duration -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results25)

Dur<- c("Semester/Quarter", "Less than the Semester/Quarter")
Effect4<- c(1.10, 0.64)
UpperCI4<- c (1.49, 1.40)
LowerCI4<- c(0.72, -0.13)
Durationgroup<-data.frame(Dur, Effect4, UpperCI4, LowerCI4)

ggplot(Durationgroup, 
       aes(x = Effect4, y = Dur, xmin = LowerCI4, xmax = UpperCI4))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Group Duration") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))

#Group work graded moderator
results26=rma.mv(Yi, Vi, mods = ~Graded -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results26)
Credit<- c("Graded", "Not Graded")
Effect5<- c(1.59, 0.97)
UpperCI5<- c (2.58, 2.22)
LowerCI5<- c(0.61, -0.27)
Gradedgroup<-data.frame(Credit, Effect5, UpperCI5, LowerCI5)

ggplot(Gradedgroup, 
       aes(x = Effect5, y = Credit, xmin = LowerCI5, xmax = UpperCI5))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Group Work Graded?") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))


#Group work assigned moderator plot
results27=rma.mv(Yi, Vi, mods = ~Group.Assigned -1, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results27)

Ass<- c("Groups Assigned", "Groups Not Assigned")
Effect6<- c(1.37, 0.82)
UpperCI6<- c (1.89, 1.38)
LowerCI6<- c(0.85, 0.25)
GroupsAssign<-data.frame(Ass, Effect6, UpperCI6, LowerCI6)

ggplot(GroupsAssign, 
       aes(x = Effect6, y = Ass, xmin = LowerCI6, xmax = UpperCI6))+
  theme_bw()+
  geom_point() +
  geom_linerange()+
  xlim(-1, 3)+
  ylab("Groups Assigned?") +
  theme(#axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.text.y=element_text(angle=360),
    plot.title = element_text(size=16, face="bold", hjust=0.5))

##Pairwise analysis
results28=rma.mv(Yi, Vi, mods = ~Design, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results28)

##Class level pairwise
results29=rma.mv(Yi, Vi, mods = ~Level, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results29)

##Class size pairwise
results30=rma.mv(Yi, Vi, mods = ~ClassSize, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results30)

##Class size relevel
DatumRMod$ClassSize=as.factor(DatumRMod$ClassSize)
levels(DatumRMod$ClassSize)
DatumRMod$ClassSize=relevel(DatumRMod$ClassSize, ref="Small")
results31=rma.mv(Yi, Vi, mods = ~ClassSize, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results31)

##Group size pairwise
results32=rma.mv(Yi, Vi, mods = ~GroupSize, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results32)

##relevel Group Size
datum1$group_size=as.factor(datum1$group_size)
levels(datum1$group_size)
datum1$group_size=relevel(datum1$group_size, ref="Small")
results33= rma.mv(yi, vi, mods = ~group_size, method = "REML", random = ~ 1 | Author, data=datum1)
summary(results33)

##Research design pairwise
results34 =rma.mv(Yi, Vi, mods = ~Design, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results34)

#majors or nonmajors pairwise
results35 =rma.mv(Yi, Vi, mods = ~Majors, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results35)

#graded pairwise
results36 = rma.mv(Yi, Vi, mods = ~Graded, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results36)

#graded assigned 
results37= rma.mv(Yi, Vi, mods = ~Group.Assigned, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results37)

#group duration
results38= rma.mv(Yi, Vi, mods = ~Duration, method = "REML", random = ~ 1 | Reference, data=DatumRMod)
summary(results38)
