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
setwd("/Users/stefanicrabtree/Documents/Manuscripts/Prosocial/April_30_2023_runs")




## Here we are making all the plots for MM

MM <- read.csv ("Prosocial_Sept_19_mm_only_PGG_1_5-table.csv", skip=6, header=T)
#MM <- read.csv ("../Model Outputs/Nov_28/Prosocial_MM_only-table.csv", skip=6, header=T, na.strings="<RuntimePrimitiveException>")
#MM_4 <-subset(MM, public_goods_game_multiplier=="1.5")
MM <- read.csv ("Prosocial_Sept_19_2022_mm_only_PGG_2-table.csv", skip=6, header=T)

## In MM we have varying sanction fines. We don't do that for the other types of plots
MM_4 <-subset(MM, sanction_fine=="4")
MM_5 <-subset(MM, sanction_fine=="5")
MM_6 <-subset(MM, sanction_fine=="6")
MM_10 <-subset(MM, sanction_fine=="10")


##First we will look at the MM pennies, and then the pops
## Change what comes after the underscore to get this correct

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


penny_plot = ggplot()+
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
  
penp = penny_plot + xlab("Monitor, PGG 2, Sanction Fine 10") + ylab("Pennies")
 
penp
 #myplot + theme_bw()
 
 #myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Here are the MM pops

##Plotting numbers of agents in MM scenarios

a <- summarySE(MM_10, measurevar="count_monitor", groupvars=c("step"))
a$highError <- (a$count_monitor + a$sd)
a$lowError <- (a$count_monitor - a$sd)

b <- summarySE(MM_10, measurevar="count_always_defect", groupvars=c("step"))
b$highError <- (b$count_always_defect + b$sd)
b$lowError <- (b$count_always_defect - b$sd)

c <- summarySE(MM_10, measurevar="count_cooperate", groupvars=c("step"))
c$highError <- (c$count_cooperate + c$sd)
c$lowError <- (c$count_cooperate - c$sd)

#reluctant_cooperator_pennies
d <- summarySE(MM_10, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d$highError <- (d$count_reluctant_cooperate + d$sd)
d$lowError <- (d$count_reluctant_cooperate - d$sd)

#reluctant_defector_pennies
e <- summarySE(MM_10, measurevar="count_reluctant_defect", groupvars=c("step"))
e$highError <- (e$count_reluctant_defect + e$sd)
e$lowError <- (e$count_reluctant_defect - e$sd)

## And here are the plots

pop_plot = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a$lowError, ymax = a$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b$step, ymin = b$lowError, ymax = b$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c$step, ymin = c$lowError, ymax = c$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d$step, ymin = d$lowError, ymax = d$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e$step, ymin = e$lowError, ymax = e$highError), fill = "mediumpurple3") +
  geom_line(data=a, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp = pop_plot + xlab("Step") + ylab("Population")
grid.arrange(pp, penp, ncol=2)

###
####
####
#####
#####
### Let's make some despot figures

#despot40 <- read.csv ("Prosocial_March_15_despot_04-table.csv", skip=6, header=T)
despot40 <- read.csv ("Prosocial_March_15_despot_ps_40-table.csv", skip=6, header=T)


###This is actually for copy defector here, since we are plotting most of the same things we do for despot


#despot40_no_NA <- na.omit(despot40)

aa <- summarySE(despot40, measurevar="monitor_pennies", groupvars=c("step"))
aa$highError <- (aa$monitor_pennies + aa$sd)
aa$lowError <- (aa$monitor_pennies - aa$sd)

bb <- summarySE(despot40, measurevar="always_defect_pennies", groupvars=c("step"))
bb$highError <- (bb$always_defect_pennies + bb$sd)
bb$lowError <- (bb$always_defect_pennies - bb$sd)

cc <- summarySE(despot40, measurevar="cooperator_pennies", groupvars=c("step"))
cc$highError <- (cc$cooperator_pennies + cc$sd)
cc$lowError <- (cc$cooperator_pennies - cc$sd)

#reluctant_cooperator_pennies
dd <- summarySE(despot40, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
dd$highError <- (dd$reluctant_cooperator_pennies + dd$sd)
dd$lowError <- (dd$reluctant_cooperator_pennies - dd$sd)

#reluctant_defector_pennies
ee <- summarySE(despot40, measurevar="reluctant_defector_pennies", groupvars=c("step"))
ee$highError <- (ee$reluctant_defector_pennies + ee$sd)
ee$lowError <- (ee$reluctant_defector_pennies - ee$sd)

penny_plot3 = ggplot()+
  geom_ribbon(aes(x=aa$step, ymin = aa$lowError, ymax = aa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bb$step, ymin = bb$lowError, ymax = bb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=cc$step, ymin = cc$lowError, ymax = cc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=dd$step, ymin = dd$lowError, ymax = dd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=ee$step, ymin = ee$lowError, ymax = ee$highError), fill = "mediumpurple3") +
  geom_line(data=aa, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=bb, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=cc, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=dd, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=ee, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()

penp_despot = penny_plot3 + xlab("Global Leadership, Probability Sanction 40%") + ylab("Pennies")
ppD = penp_despot + coord_cartesian(xlim = c(0, 500), ylim = c(0, 600))



despot90 <- read.csv ("Prosocial_March_15_despot_ps_90-table.csv", skip=6, header=T)
#despot90_no_NA <- na.omit(despot90)


ff <- summarySE(despot90, measurevar="monitor_pennies", groupvars=c("step"))
ff$highError <- (ff$monitor_pennies + ff$sd)
ff$lowError <- (ff$monitor_pennies - ff$sd)

gg <- summarySE(despot90, measurevar="always_defect_pennies", groupvars=c("step"))
gg$highError <- (gg$always_defect_pennies + gg$sd)
gg$lowError <- (gg$always_defect_pennies - gg$sd)

hh <- summarySE(despot90, measurevar="cooperator_pennies", groupvars=c("step"))
hh$highError <- (hh$cooperator_pennies + hh$sd)
hh$lowError <- (hh$cooperator_pennies - hh$sd)

#reluctant_cooperator_pennies
kk <- summarySE(despot90, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
kk$highError <- (kk$reluctant_cooperator_pennies + kk$sd)
kk$lowError <- (kk$reluctant_cooperator_pennies - kk$sd)

#reluctant_defector_pennies
mm <- summarySE(despot90, measurevar="reluctant_defector_pennies", groupvars=c("step"))
mm$highError <- (mm$reluctant_defector_pennies + mm$sd)
mm$lowError <- (mm$reluctant_defector_pennies - mm$sd)

penny_plot4 = ggplot()+
  geom_ribbon(aes(x=ff$step, ymin = ff$lowError, ymax = ff$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=gg$step, ymin = gg$lowError, ymax = gg$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=hh$step, ymin = hh$lowError, ymax = hh$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=kk$step, ymin = kk$lowError, ymax = kk$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=mm$step, ymin = mm$lowError, ymax = mm$highError), fill = "mediumpurple3") +
  geom_line(data=ff, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=gg, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=hh, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=kk, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=mm, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penp_despot2 = penny_plot4 + xlab("Global Leader, Probability Sanction 90%") + ylab("Pennies")
ppD2 = penp_despot2 + coord_cartesian(xlim = c(0, 500), ylim = c(0, 600))


png(file="Despot_April_8.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(ppD, ppD2, nrow=1, ncol = 2, labels=c('\nA.', '\nB.'), label_size=10, align="v")
dev.off()

damn <- plot(despot40$step, despot40$reluctant_defector_pennies, type = "l", lty = 1)

#plot(despot40$step, despot40$reluctant_defector_pennies)

#png(file="Colins_Stupid_Plot_2.png", height=8, width=10.5, units="in", res=600)
#par(mar=c(4.5,4.5,1.5,1.2))
#par(oma=c(0,0,0,0))
#plot(damn, xlim=c(0,500), ylim=c(0,600))
#dev.off()


##Plotting numbers of agents in despot scenarios

a <- summarySE(despot, measurevar="count_monitor", groupvars=c("step"))
a$highError <- (a$count_monitor + a$sd)
a$lowError <- (a$count_monitor - a$sd)

b <- summarySE(despot, measurevar="count_always_defect", groupvars=c("step"))
b$highError <- (b$count_always_defect + b$sd)
b$lowError <- (b$count_always_defect - b$sd)

c <- summarySE(despot, measurevar="count_cooperate", groupvars=c("step"))
c$highError <- (c$count_cooperate + c$sd)
c$lowError <- (c$count_cooperate - c$sd)

#reluctant_cooperator_pennies
d <- summarySE(despot, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d$highError <- (d$count_reluctant_cooperate + d$sd)
d$lowError <- (d$count_reluctant_cooperate - d$sd)

#reluctant_defector_pennies
e <- summarySE(despot, measurevar="count_reluctant_defect", groupvars=c("step"))
e$highError <- (e$count_reluctant_defect + e$sd)
e$lowError <- (e$count_reluctant_defect - e$sd)

## And here are the plots

pop_plot = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a$lowError, ymax = a$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b$step, ymin = b$lowError, ymax = b$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c$step, ymin = c$lowError, ymax = c$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d$step, ymin = d$lowError, ymax = d$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e$step, ymin = e$lowError, ymax = e$highError), fill = "mediumpurple3") +
  geom_line(data=a, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp = pop_plot + xlab("Step") + ylab("Population")

##
#####
#####
#####
##
#####
#####
#####
##
#####
#####
#####
##
#####
#####
#####
##
#####
#####
#####
##
#####
#####
#####
## Okay, now let's make some copy the leader plots

copy_leader <- read.csv ("Prosocial_April_30_2023_ps_09_copy_defector-table.csv", skip=6, header=T)

## Here are the copy the leader pennies
a <- summarySE(copy_leader, measurevar="monitor_pennies", groupvars=c("step"))
a$highError <- (a$monitor_pennies + a$sd)
a$lowError <- (a$monitor_pennies - a$sd)

b <- summarySE(copy_leader, measurevar="always_defect_pennies", groupvars=c("step"))
b$highError <- (b$always_defect_pennies + b$sd)
b$lowError <- (b$always_defect_pennies - b$sd)

c <- summarySE(copy_leader, measurevar="cooperator_pennies", groupvars=c("step"))
c$highError <- (c$cooperator_pennies + c$sd)
c$lowError <- (c$cooperator_pennies - c$sd)

#reluctant_cooperator_pennies
d <- summarySE(copy_leader, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
d$highError <- (d$reluctant_cooperator_pennies + d$sd)
d$lowError <- (d$reluctant_cooperator_pennies - d$sd)

#reluctant_defector_pennies
e <- summarySE(copy_leader, measurevar="reluctant_defector_pennies", groupvars=c("step"))
e$highError <- (e$reluctant_defector_pennies + e$sd)
e$lowError <- (e$reluctant_defector_pennies - e$sd)

penny_plot1 = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a$lowError, ymax = a$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b$step, ymin = b$lowError, ymax = b$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c$step, ymin = c$lowError, ymax = c$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d$step, ymin = d$lowError, ymax = d$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e$step, ymin = e$lowError, ymax = e$highError), fill = "mediumpurple3") +
  geom_line(data=a, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=b, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=c, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=d, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=e, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()

penp_copyL90 = penny_plot1 + xlab("Copy Defector, Probability Sanction 90%") + ylab("Pennies")
ppL90 = penp_copyL90 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 150))


copy_leader2 <- read.csv ("Prosocial_April_30_2023_ps_04_copy_defector-table.csv", skip=6, header=T)

f <- summarySE(copy_leader2, measurevar="monitor_pennies", groupvars=c("step"))
f$highError <- (f$monitor_pennies + f$sd)
f$lowError <- (f$monitor_pennies - f$sd)

g <- summarySE(copy_leader2, measurevar="always_defect_pennies", groupvars=c("step"))
g$highError <- (g$always_defect_pennies + g$sd)
g$lowError <- (g$always_defect_pennies - g$sd)

h <- summarySE(copy_leader2, measurevar="cooperator_pennies", groupvars=c("step"))
h$highError <- (h$cooperator_pennies + h$sd)
h$lowError <- (h$cooperator_pennies - h$sd)

#reluctant_cooperator_pennies
k <- summarySE(copy_leader2, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
k$highError <- (k$reluctant_cooperator_pennies + k$sd)
k$lowError <- (k$reluctant_cooperator_pennies - k$sd)

#reluctant_defector_pennies
m <- summarySE(copy_leader2, measurevar="reluctant_defector_pennies", groupvars=c("step"))
m$highError <- (m$reluctant_defector_pennies + m$sd)
m$lowError <- (m$reluctant_defector_pennies - m$sd)

penny_plot2 = ggplot()+
  geom_ribbon(aes(x=f$step, ymin = f$lowError, ymax = f$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=f$step, ymin = g$lowError, ymax = g$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=h$step, ymin = h$lowError, ymax = h$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=k$step, ymin = k$lowError, ymax = k$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=m$step, ymin = m$lowError, ymax = m$highError), fill = "mediumpurple3") +
  geom_line(data=f, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=g, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=h, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=k, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=m, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()

penp_copyL40 = penny_plot2 + xlab("Copy Defector, Probability Sanction 40%") + ylab("Pennies")
pppL40 = penp_copyL40 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 150))

grid.arrange(penp_copyL40, penp_copyL90, penp_despot, penp_despot2, nrow=2, ncol = 2)

png(file="Despot_to_Copy_Defector.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penp_despot, penp_despot2, penp_copyL40, penp_copyL90, nrow=2, ncol = 2, labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()

png(file="Despot_to_Copy_Defector_zoomed.png", height=8, width=10,5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(ppD, ppD2, nrow=2, ncol = 2, labels=c('\n A.', '\n B.'), label_size=10, align="v")
dev.off()

png(file="Four_MM_pgg_2_ps04_tax_0_50_2.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plotMM4, penny_plotMM5, penny_plotMM6, penp10, nrow=2, ncol=2,  labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()

##Plotting numbers of agents in copy leader scenarios

a <- summarySE(copy_leader, measurevar="count_monitor", groupvars=c("step"))
a$highError <- (a$count_monitor + a$sd)
a$lowError <- (a$count_monitor - a$sd)

b <- summarySE(copy_leader, measurevar="count_always_defect", groupvars=c("step"))
b$highError <- (b$count_always_defect + b$sd)
b$lowError <- (b$count_always_defect - b$sd)

c <- summarySE(copy_leader, measurevar="count_cooperate", groupvars=c("step"))
c$highError <- (c$count_cooperate + c$sd)
c$lowError <- (c$count_cooperate - c$sd)

#reluctant_cooperator_pennies
d <- summarySE(copy_leader, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d$highError <- (d$count_reluctant_cooperate + d$sd)
d$lowError <- (d$count_reluctant_cooperate - d$sd)

#reluctant_defector_pennies
e <- summarySE(copy_leader, measurevar="count_reluctant_defect", groupvars=c("step"))
e$highError <- (e$count_reluctant_defect + e$sd)
e$lowError <- (e$count_reluctant_defect - e$sd)

#colors <- c("count_monitor" = "forestgreen", "count_always_defect" = "darkorchid4", "count_cooperate" = "dodgerblue4", "count_reluctant_cooperate" = "cadetblue", "count_reluctant_defect" = "mediumpurple4")
pop_plot = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a$lowError, ymax = a$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b$step, ymin = b$lowError, ymax = b$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c$step, ymin = c$lowError, ymax = c$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d$step, ymin = d$lowError, ymax = d$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e$step, ymin = e$lowError, ymax = e$highError), fill = "mediumpurple3") +
  geom_line(data=a, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp = pop_plot + xlab("Step") + ylab("Population") 

png(file="Despot_to_Copy_Defector.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penp_despot, penp_copyL,nrow=2, labels=c('A', 'B'), label_size=12, align="v")
(legend(x="topleft", legend=c("reluctant cooperators","monitors","always defect","cooperate","reluctant defectors"),
        lty = c(1,1,1,1,1),
        col = c("red","blue","green","darkgoldenrod","deeppink4"),
        lwd = c(4,4,4,4,4)))

dev.off()



my_fill_colors <- c(a = "mediumseagreen", b = "darkorchid3", c = "dodgerblue2", d = "cadetblue3", e = "mediumpurple3")
my_line_colors <- c(a = "forestgreen", b = "darkorchid4", c = "dodgerblue4", d = "cadetblue", e = "mediumpurple4")

pop_plot = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a$lowError, ymax = a$highError, fill = "a")) +
  geom_ribbon(aes(x=b$step, ymin = b$lowError, ymax = b$highError, fill = "b")) +
  geom_ribbon(aes(x=c$step, ymin = c$lowError, ymax = c$highError, fill = "c")) +
  geom_ribbon(aes(x=d$step, ymin = d$lowError, ymax = d$highError, fill = "d")) +
  geom_ribbon(aes(x=e$step, ymin = e$lowError, ymax = e$highError, fill = "e")) +
  scale_fill_manual(values = my_fill_colors) +
  geom_line(data=a, aes(x=step, y=count_monitor,colour="a")) +
  geom_line(data=b, aes(x=step, y=count_always_defect,colour="b"))+
  geom_line(data=c, aes(x=step, y=count_cooperate,colour="c"))+
  geom_line(data=d, aes(x=step, y=count_reluctant_cooperate,colour="d"))+
  geom_line(data=e, aes(x=step, y=count_reluctant_defect,colour="e"))+
  scale_color_manual(values = my_line_colors) +
  theme_few() + 
  labs(x = "Step", y="Population", color = "Population") + 
  guides(fill="none")
pop_plot


### And now we do the Influencer pennies, which is what we came here for

inf <- read.csv ("Prosocial_Sept_19_2022 _influencer_6-table.csv", skip=6, header=T)

inf_05_5 <- subset(inf, local_probinfluence=="0.5" & local_sphereinfluence == "5")
inf_05_10 <- subset(inf, local_probinfluence=="0.5" & local_sphereinfluence == "10")
inf_75_5 <- subset(inf, local_probinfluence=="0.75" & local_sphereinfluence == "5")
inf_75_10 <- subset(inf, local_probinfluence=="0.75" & local_sphereinfluence == "10")
inf_09_5 <- subset(inf, local_probinfluence=="0.9" & local_sphereinfluence == "5")
inf_09_10 <- subset(inf, local_probinfluence=="0.9" & local_sphereinfluence == "10")

inf_5_sp <- subset(inf, local_sphereinfluence=="5")
inf_10_sp <- subset(inf, local_sphereinfluence=="10")