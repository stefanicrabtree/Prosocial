## Summary Function for median etc. This is old code but works. Run this to have the summarySE function callable

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  
  # Rename the "mean" column    
  datac <- reshape::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

### CODE FOR THIS ANALYSIS

#install.packages("ggthemes") # Install 
library(ggthemes) # Load


#install.packages("ggpubr")
#library(ggpubr)

##We need these libraries
library(dplyr)
library(reshape)
library(lattice)
library (ggplot2)
library(gridExtra)
library(grid)
library(cowplot)


##the folder I'm using
#I'm switching to relative paths so its easier to both work on the script
getwd()
setwd("/Users/stefanicrabtree/Documents/Prosocial/April_30_2023_runs")


#MM <- read.csv ("Prosocial_Sept_19_mm_only_PGG_1_5-table.csv", skip=6, header=T)
#MM <- read.csv ("../Model Outputs/Nov_28/Prosocial_MM_only-table.csv", skip=6, header=T, na.strings="<RuntimePrimitiveException>")
#MM_4 <-subset(MM, public_goods_game_multiplier=="1.5")
#MM <- read.csv ("Prosocial_Sept_19_2022_mm_only_PGG_2-table.csv", skip=6, header=T)

MM <- read.csv ("Prosocial_July_7_mm_only_PGG_2_Prob_4-table.csv", skip=6, header=T)
MM <- read.csv ("Prosocial_July_7_mm_only_PGG_2_Prob_9-table.csv", skip=6, header=T)

MM <- read.csv ("Prosocial_July_7_mm_only_PGG_2_Prob_4-table.csv", skip=6, header=T)

## In MM we have varying sanction fines. We don't do that for the other types of plots
MM_4 <-subset(MM, sanction_fine=="4")
MM_5 <-subset(MM, sanction_fine=="5")
MM_6 <-subset(MM, sanction_fine=="6")
MM_10 <-subset(MM, sanction_fine=="10")


##First we will look at the MM pennies, and then the pops
## Change what comes after the underscore to get this correct



#
#
#
#
## Here are the MM pennies
a2 <- summarySE(MM_4, measurevar="monitor_pennies", groupvars=c("step"))
a2$highError <- (a2$monitor_pennies + a2$sd)
a2$lowError <- (a2$monitor_pennies - a2$sd)

b2 <- summarySE(MM_4, measurevar="always_defect_pennies", groupvars=c("step"))
b2$highError <- (b2$always_defect_pennies + b2$sd)
b2$lowError <- (b2$always_defect_pennies - b2$sd)

c2 <- summarySE(MM_4, measurevar="cooperator_pennies", groupvars=c("step"))
c2$highError <- (c2$cooperator_pennies + c2$sd)
c2$lowError <- (c2$cooperator_pennies - c2$sd)

#reluctant_cooperator_pennies
d2 <- summarySE(MM_4, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
d2$highError <- (d2$reluctant_cooperator_pennies + d2$sd)
d2$lowError <- (d2$reluctant_cooperator_pennies - d2$sd)

#reluctant_defector_pennies
e2 <- summarySE(MM_4, measurevar="reluctant_defector_pennies", groupvars=c("step"))
e2$highError <- (e2$reluctant_defector_pennies + e2$sd)
e2$lowError <- (e2$reluctant_defector_pennies - e2$sd)



#Here is the first plot, which looks at a sanction fine of 10 for MM only  


penny_plotMM4 = ggplot()+
  geom_ribbon(aes(x=a2$step, ymin = a2$lowError, ymax = a2$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b2$step, ymin = b2$lowError, ymax = b2$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c2$step, ymin = c2$lowError, ymax = c2$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d2$step, ymin = d2$lowError, ymax = d2$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e2$step, ymin = e2$lowError, ymax = e2$highError), fill = "mediumpurple3") +
  geom_line(data=a2, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=b2, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=c2, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=d2, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=e2, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plotMM4 = penny_plotMM4 + xlab("Monitor, PGG 1.5, Sanction Fine 4") + ylab("Pennies")
ppMM4 = penny_plotMM4 + coord_cartesian(xlim = c(-20, 150), ylim = c(-20, 150))
ppMM4
##
##
##


#
#
## Here are the MM pennies
a3 <- summarySE(MM_5, measurevar="monitor_pennies", groupvars=c("step"))
a3$highError <- (a3$monitor_pennies + a3$sd)
a3$lowError <- (a3$monitor_pennies - a3$sd)

b3 <- summarySE(MM_5, measurevar="always_defect_pennies", groupvars=c("step"))
b3$highError <- (b3$always_defect_pennies + b3$sd)
b3$lowError <- (b3$always_defect_pennies - b3$sd)

c3 <- summarySE(MM_5, measurevar="cooperator_pennies", groupvars=c("step"))
c3$highError <- (c3$cooperator_pennies + c3$sd)
c3$lowError <- (c3$cooperator_pennies - c3$sd)

#reluctant_cooperator_pennies
d3 <- summarySE(MM_5, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
d3$highError <- (d3$reluctant_cooperator_pennies + d3$sd)
d3$lowError <- (d3$reluctant_cooperator_pennies - d3$sd)

#reluctant_defector_pennies
e3 <- summarySE(MM_5, measurevar="reluctant_defector_pennies", groupvars=c("step"))
e3$highError <- (e3$reluctant_defector_pennies + e3$sd)
e3$lowError <- (e3$reluctant_defector_pennies - e3$sd)



#Here is the first plot, which looks at a sanction fine of 10 for MM only  


penny_plotMM5 = ggplot()+
  geom_ribbon(aes(x=a3$step, ymin = a3$lowError, ymax = a3$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b3$step, ymin = b3$lowError, ymax = b3$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c3$step, ymin = c3$lowError, ymax = c3$highError), fill = "dodgerblue3") +
  geom_ribbon(aes(x=d3$step, ymin = d3$lowError, ymax = d3$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e3$step, ymin = e3$lowError, ymax = e3$highError), fill = "mediumpurple3") +
  geom_line(data=a3, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=b3, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=c3, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=d3, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=e3, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plotMM5 = penny_plotMM5 + xlab("Monitor, PGG 1.5, Sanction Fine 5") + ylab("Pennies")
ppMM5 = penny_plotMM5 + coord_cartesian(xlim = c(-20, 150), ylim = c(-20, 150))
ppMM5
#
#
#
#
#
#
#
## Here are the MM pennies
a4 <- summarySE(MM_6, measurevar="monitor_pennies", groupvars=c("step"))
a4$highError <- (a4$monitor_pennies + a4$sd)
a4$lowError <- (a4$monitor_pennies - a4$sd)

b4 <- summarySE(MM_6, measurevar="always_defect_pennies", groupvars=c("step"))
b4$highError <- (b4$always_defect_pennies + b4$sd)
b4$lowError <- (b4$always_defect_pennies - b4$sd)

c4 <- summarySE(MM_6, measurevar="cooperator_pennies", groupvars=c("step"))
c4$highError <- (c4$cooperator_pennies + c4$sd)
c4$lowError <- (c4$cooperator_pennies - c4$sd)

#reluctant_cooperator_pennies
d4 <- summarySE(MM_6, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
d4$highError <- (d4$reluctant_cooperator_pennies + d4$sd)
d4$lowError <- (d4$reluctant_cooperator_pennies - d4$sd)

#reluctant_defector_pennies
e4 <- summarySE(MM_6, measurevar="reluctant_defector_pennies", groupvars=c("step"))
e4$highError <- (e4$reluctant_defector_pennies + e4$sd)
e4$lowError <- (e4$reluctant_defector_pennies - e4$sd)



#Here is the first plot, which looks at a sanction fine of 10 for MM only  


penny_plotMM6 = ggplot()+
  geom_ribbon(aes(x=a4$step, ymin = a4$lowError, ymax = a4$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b4$step, ymin = b4$lowError, ymax = b4$highError), fill = "darkorchid4") +
  geom_ribbon(aes(x=c4$step, ymin = c4$lowError, ymax = c4$highError), fill = "dodgerblue4") +
  geom_ribbon(aes(x=d4$step, ymin = d4$lowError, ymax = d4$highError), fill = "cadetblue4") +
  geom_ribbon(aes(x=e4$step, ymin = e4$lowError, ymax = e4$highError), fill = "mediumpurple4") +
  geom_line(data=a4, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=b4, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=c4, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=d4, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=e4, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plotMM6 = penny_plotMM6 + xlab("Monitor, PGG 2, Sanction Fine 6") + ylab("Pennies")
ppMM6 = penny_plotMM6 + coord_cartesian(xlim = c(-20, 150), ylim = c(-20, 150))

ppMM6

penny_plotMM6



## Here are the MM pennies
a1 <- summarySE(MM_10, measurevar="monitor_pennies", groupvars=c("step"))
a1$highError <- (a1$monitor_pennies + a1$sd)
a1$lowError <- (a1$monitor_pennies - a1$sd)

b1 <- summarySE(MM_10, measurevar="always_defect_pennies", groupvars=c("step"))
b1$highError <- (b1$always_defect_pennies + b1$sd)
b1$lowError <- (b1$always_defect_pennies - b1$sd)

c1 <- summarySE(MM_10, measurevar="cooperator_pennies", groupvars=c("step"))
c1$highError <- (c1$cooperator_pennies + c1$sd)
c1$lowError <- (c1$cooperator_pennies - c1$sd)

#reluctant_cooperator_pennies
d1 <- summarySE(MM_10, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
d1$highError <- (d1$reluctant_cooperator_pennies + d1$sd)
d1$lowError <- (d1$reluctant_cooperator_pennies - d1$sd)

#reluctant_defector_pennies
e1 <- summarySE(MM_10, measurevar="reluctant_defector_pennies", groupvars=c("step"))
e1$highError <- (e1$reluctant_defector_pennies + e1$sd)
e1$lowError <- (e1$reluctant_defector_pennies - e1$sd)



#Here is the first plot, which looks at a sanction fine of 10 for MM only  


penny_plot10 = ggplot()+
  geom_ribbon(aes(x=a1$step, ymin = a1$lowError, ymax = a1$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b1$step, ymin = b1$lowError, ymax = b1$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c1$step, ymin = c1$lowError, ymax = c1$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d1$step, ymin = d1$lowError, ymax = d1$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e1$step, ymin = e1$lowError, ymax = e1$highError), fill = "mediumpurple3") +
  geom_line(data=a1, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=b1, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=c1, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=d1, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=e1, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()

penp10 = penny_plot10 + xlab("Monitor, PGG 1.5, Sanction Fine 10") + ylab("Pennies")
pp10Z = penp10 + coord_cartesian(xlim = c(-20, 150), ylim = c(-20, 150))
pp10Z


png(file="Four_MM_pgg_2_ps04.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plotMM4, penny_plotMM5, penny_plotMM6, penp10, nrow=2, ncol=2,  labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()




png(file="PP_MM_1_5_zoomed.png", height=10, width=5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(ppMM4, ppMM5, ppMM6, pp10Z, nrow=4, ncol = 1, labels=c('\n a.', '\n b.', '\n c.', '\n d.'), label_size=10, align="v")
dev.off()
