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

install.packages("ggthemes") # Install 

install.packages('cowplot')

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
library(ggthemes) # Load

##the folder I'm using
#I'm switching to relative paths so its easier to both work on the script
getwd()
setwd("/Users/stefanicrabtree/Documents/Prosocial/April_30_2023_runs")


inf_sanc_04 <- read.csv ("Prosocial_April_30_influencer_10_ps_04-table.csv", skip=6, header=T)
inf_05_5 <- subset(inf_sanc_04, local_probinfluence=="0.5" & local_sphereinfluence == "5")
inf_05_10 <- subset(inf_sanc_04, local_probinfluence=="0.5" & local_sphereinfluence == "10")
inf_75_5 <- subset(inf_sanc_04, local_probinfluence=="0.75" & local_sphereinfluence == "5")
inf_75_10 <- subset(inf_sanc_04, local_probinfluence=="0.75" & local_sphereinfluence == "10")
inf_09_5 <- subset(inf_sanc_04, local_probinfluence=="0.9" & local_sphereinfluence == "5")
inf_09_10 <- subset(inf_sanc_04, local_probinfluence=="0.9" & local_sphereinfluence == "10")



inf_sanc_09 <- read.csv ("Prosocial_April_30_influencer_10_ps_09-table.csv", skip=6, header=T)
inf_05_5 <- subset(inf_sanc_09, local_probinfluence=="0.5" & local_sphereinfluence == "5")
inf_05_10 <- subset(inf_sanc_09, local_probinfluence=="0.5" & local_sphereinfluence == "10")
inf_75_5 <- subset(inf_sanc_09, local_probinfluence=="0.75" & local_sphereinfluence == "5")
inf_75_10 <- subset(inf_sanc_09, local_probinfluence=="0.75" & local_sphereinfluence == "10")
inf_09_5 <- subset(inf_sanc_09, local_probinfluence=="0.9" & local_sphereinfluence == "5")
inf_09_10 <- subset(inf_sanc_09, local_probinfluence=="0.9" & local_sphereinfluence == "10")




a <- summarySE(inf_05_5, measurevar="count_monitor", groupvars=c("step"))
a$highError <- (a$count_monitor + a$sd)
a$lowError <- (a$count_monitor - a$sd)

b <- summarySE(inf_05_5, measurevar="count_always_defect", groupvars=c("step"))
b$highError <- (b$count_always_defect + b$sd)
b$lowError <- (b$count_always_defect - b$sd)

c <- summarySE(inf_05_5, measurevar="count_cooperate", groupvars=c("step"))
c$highError <- (c$count_cooperate + c$sd)
c$lowError <- (c$count_cooperate - c$sd)

#count_reluctant_cooperate
d <- summarySE(inf_05_5, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d$highError <- (d$count_reluctant_cooperate + d$sd)
d$lowError <- (d$count_reluctant_cooperate - d$sd)

#count_reluctant_defect
e <- summarySE(inf_05_5, measurevar="count_reluctant_defect", groupvars=c("step"))
e$highError <- (e$count_reluctant_defect + e$sd)
e$lowError <- (e$count_reluctant_defect - e$sd)

penny_plot1 = ggplot()+
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
penny_plot1 = penny_plot1 + xlab("Step") + ylab("population")
p1 = penny_plot1 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))

aa <- summarySE(inf_05_10, measurevar="count_monitor", groupvars=c("step"))
aa$highError <- (aa$count_monitor + aa$sd)
aa$lowError <- (aa$count_monitor - aa$sd)

bb <- summarySE(inf_05_10, measurevar="count_always_defect", groupvars=c("step"))
bb$highError <- (bb$count_always_defect + bb$sd)
bb$lowError <- (bb$count_always_defect - bb$sd)

cc <- summarySE(inf_05_10, measurevar="count_cooperate", groupvars=c("step"))
cc$highError <- (cc$count_cooperate + cc$sd)
cc$lowError <- (cc$count_cooperate - cc$sd)

#count_reluctant_cooperate
dd <- summarySE(inf_05_10, measurevar="count_reluctant_cooperate", groupvars=c("step"))
dd$highError <- (dd$count_reluctant_cooperate + dd$sd)
dd$lowError <- (dd$count_reluctant_cooperate - dd$sd)

#count_reluctant_defect
ee <- summarySE(inf_05_10, measurevar="count_reluctant_defect", groupvars=c("step"))
ee$highError <- (ee$count_reluctant_defect + ee$sd)
ee$lowError <- (ee$count_reluctant_defect - ee$sd)

penny_plot2 = ggplot()+
  geom_ribbon(aes(x=aa$step, ymin = aa$lowError, ymax = aa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bb$step, ymin = bb$lowError, ymax = bb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=cc$step, ymin = cc$lowError, ymax = cc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=dd$step, ymin = dd$lowError, ymax = dd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=ee$step, ymin = ee$lowError, ymax = ee$highError), fill = "mediumpurple3") +
  geom_line(data=aa, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=bb, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=cc, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=dd, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=ee, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
penny_plot2 = penny_plot2 + xlab("Step") + ylab("population")
p2 = penny_plot2 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))
###


aaa <- summarySE(inf_75_5, measurevar="count_monitor", groupvars=c("step"))
aaa$highError <- (aaa$count_monitor + aaa$sd)
aaa$lowError <- (aaa$count_monitor - aaa$sd)

bbb <- summarySE(inf_75_5, measurevar="count_always_defect", groupvars=c("step"))
bbb$highError <- (bbb$count_always_defect + bbb$sd)
bbb$lowError <- (bbb$count_always_defect - bbb$sd)

ccc <- summarySE(inf_75_5, measurevar="count_cooperate", groupvars=c("step"))
ccc$highError <- (ccc$count_cooperate + ccc$sd)
ccc$lowError <- (ccc$count_cooperate - ccc$sd)

#count_reluctant_cooperate
ddd <- summarySE(inf_75_5, measurevar="count_reluctant_cooperate", groupvars=c("step"))
ddd$highError <- (ddd$count_reluctant_cooperate + ddd$sd)
ddd$lowError <- (ddd$count_reluctant_cooperate - ddd$sd)

#count_reluctant_defect
eee <- summarySE(inf_75_5, measurevar="count_reluctant_defect", groupvars=c("step"))
eee$highError <- (eee$count_reluctant_defect + eee$sd)
eee$lowError <- (eee$count_reluctant_defect - eee$sd)


penny_plot3 = ggplot()+
  geom_ribbon(aes(x=aaa$step, ymin = aaa$lowError, ymax = aaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbb$step, ymin = bbb$lowError, ymax = bbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=ccc$step, ymin = ccc$lowError, ymax = ccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=ddd$step, ymin = ddd$lowError, ymax = ddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eee$step, ymin = eee$lowError, ymax = eee$highError), fill = "mediumpurple3") +
  geom_line(data=aaa, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=bbb, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=ccc, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=ddd, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=eee, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
penny_plot3 = penny_plot3 + xlab("Step") + ylab("population")
p3 = penny_plot3 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))


######


aaaa <- summarySE(inf_75_10, measurevar="count_monitor", groupvars=c("step"))
aaaa$highError <- (aaaa$count_monitor + aaaa$sd)
aaaa$lowError <- (aaaa$count_monitor - aaaa$sd)

bbbb <- summarySE(inf_75_10, measurevar="count_always_defect", groupvars=c("step"))
bbbb$highError <- (bbbb$count_always_defect + bbbb$sd)
bbbb$lowError <- (bbbb$count_always_defect - bbbb$sd)

cccc <- summarySE(inf_75_10, measurevar="count_cooperate", groupvars=c("step"))
cccc$highError <- (cccc$count_cooperate + cccc$sd)
cccc$lowError <- (cccc$count_cooperate - cccc$sd)

#count_reluctant_cooperate
dddd <- summarySE(inf_75_10, measurevar="count_reluctant_cooperate", groupvars=c("step"))
dddd$highError <- (dddd$count_reluctant_cooperate + dddd$sd)
dddd$lowError <- (dddd$count_reluctant_cooperate - dddd$sd)

#count_reluctant_defect
eeee <- summarySE(inf_75_10, measurevar="count_reluctant_defect", groupvars=c("step"))
eeee$highError <- (eeee$count_reluctant_defect + eeee$sd)
eeee$lowError <- (eeee$count_reluctant_defect - eeee$sd)

penny_plot4 = ggplot()+
  geom_ribbon(aes(x=aaaa$step, ymin = aaaa$lowError, ymax = aaaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbbb$step, ymin = bbbb$lowError, ymax = bbbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=cccc$step, ymin = cccc$lowError, ymax = cccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=dddd$step, ymin = dddd$lowError, ymax = dddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eeee$step, ymin = eeee$lowError, ymax = eeee$highError), fill = "mediumpurple3") +
  geom_line(data=aaaa, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=bbbb, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=cccc, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=dddd, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=eeee, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
penny_plot4 = penny_plot4 + xlab("Step") + ylab("population")
p4 = penny_plot4 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))


####




aaaaa <- summarySE(inf_09_5, measurevar="count_monitor", groupvars=c("step"))
aaaaa$highError <- (aaaaa$count_monitor + aaaaa$sd)
aaaaa$lowError <- (aaaaa$count_monitor - aaaaa$sd)

bbbbb <- summarySE(inf_09_5, measurevar="count_always_defect", groupvars=c("step"))
bbbbb$highError <- (bbbbb$count_always_defect + bbbbb$sd)
bbbbb$lowError <- (bbbbb$count_always_defect - bbbbb$sd)

ccccc <- summarySE(inf_09_5, measurevar="count_cooperate", groupvars=c("step"))
ccccc$highError <- (ccccc$count_cooperate + ccccc$sd)
ccccc$lowError <- (ccccc$count_cooperate - ccccc$sd)

#count_reluctant_cooperate
ddddd <- summarySE(inf_09_5, measurevar="count_reluctant_cooperate", groupvars=c("step"))
ddddd$highError <- (ddddd$count_reluctant_cooperate + ddddd$sd)
ddddd$lowError <- (ddddd$count_reluctant_cooperate - ddddd$sd)

#count_reluctant_defect
eeeee <- summarySE(inf_09_5, measurevar="count_reluctant_defect", groupvars=c("step"))
eeeee$highError <- (eeeee$count_reluctant_defect + eeeee$sd)
eeeee$lowError <- (eeeee$count_reluctant_defect - eeeee$sd)

penny_plot5 = ggplot()+
  geom_ribbon(aes(x=aaaaa$step, ymin = aaaaa$lowError, ymax = aaaaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbbbb$step, ymin = bbbbb$lowError, ymax = bbbbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=ccccc$step, ymin = ccccc$lowError, ymax = ccccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=ddddd$step, ymin = ddddd$lowError, ymax = ddddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eeeee$step, ymin = eeeee$lowError, ymax = eeeee$highError), fill = "mediumpurple3") +
  geom_line(data=aaaaa, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=bbbbb, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=ccccc, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=ddddd, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=eeeee, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
penny_plot5 = penny_plot5 + xlab("Step") + ylab("population")

p5 = penny_plot5 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))

######




aaaaaa <- summarySE(inf_09_10, measurevar="count_monitor", groupvars=c("step"))
aaaaaa$highError <- (aaaaaa$count_monitor + aaaaaa$sd)
aaaaaa$lowError <- (aaaaaa$count_monitor - aaaaaa$sd)

bbbbbb <- summarySE(inf_09_10, measurevar="count_always_defect", groupvars=c("step"))
bbbbbb$highError <- (bbbbbb$count_always_defect + bbbbbb$sd)
bbbbbb$lowError <- (bbbbbb$count_always_defect - bbbbbb$sd)

cccccc <- summarySE(inf_09_10, measurevar="count_cooperate", groupvars=c("step"))
cccccc$highError <- (cccccc$count_cooperate + cccccc$sd)
cccccc$lowError <- (cccccc$count_cooperate - cccccc$sd)

#count_reluctant_cooperate
dddddd <- summarySE(inf_09_10, measurevar="count_reluctant_cooperate", groupvars=c("step"))
dddddd$highError <- (dddddd$count_reluctant_cooperate + dddddd$sd)
dddddd$lowError <- (dddddd$count_reluctant_cooperate - dddddd$sd)

#count_reluctant_defect
eeeeee <- summarySE(inf_09_10, measurevar="count_reluctant_defect", groupvars=c("step"))
eeeeee$highError <- (eeeeee$count_reluctant_defect + eeeeee$sd)
eeeeee$lowError <- (eeeeee$count_reluctant_defect - eeeeee$sd)

penny_plot6 = ggplot()+
  geom_ribbon(aes(x=aaaaaa$step, ymin = aaaaaa$lowError, ymax = aaaaaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbbbbb$step, ymin = bbbbbb$lowError, ymax = bbbbbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=cccccc$step, ymin = cccccc$lowError, ymax = cccccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=dddddd$step, ymin = dddddd$lowError, ymax = dddddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eeeeee$step, ymin = eeeeee$lowError, ymax = eeeeee$highError), fill = "mediumpurple3") +
  geom_line(data=aaaaaa, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=bbbbbb, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=cccccc, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=dddddd, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=eeeeee, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
penny_plot6 = penny_plot6 + xlab("Step") + ylab("population")

p6 = penny_plot6 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))
p6


png(file="Influencer_sanc_09_July623_POP.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plot1, penny_plot2, penny_plot3, penny_plot4, penny_plot5, penny_plot6, nrow=2, ncol=3,  labels=c('\nA.', '\nB.', '\nC.', '\nD.', '\nE.', '\nF.'), label_size=10, align="v")
dev.off()


