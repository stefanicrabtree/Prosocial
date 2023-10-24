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
#setwd("/Users/stefanicrabtree/Documents/Prosocial/April_30_2023_runs")



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

a <- summarySE(inf_05_5, measurevar="monitor_pennies", groupvars=c("step"))
a$highError <- (a$monitor_pennies + a$sd)
a$lowError <- (a$monitor_pennies - a$sd)

b <- summarySE(inf_05_5, measurevar="always_defect_pennies", groupvars=c("step"))
b$highError <- (b$always_defect_pennies + b$sd)
b$lowError <- (b$always_defect_pennies - b$sd)

c <- summarySE(inf_05_5, measurevar="cooperator_pennies", groupvars=c("step"))
c$highError <- (c$cooperator_pennies + c$sd)
c$lowError <- (c$cooperator_pennies - c$sd)

#reluctant_cooperator_pennies
d <- summarySE(inf_05_5, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
d$highError <- (d$reluctant_cooperator_pennies + d$sd)
d$lowError <- (d$reluctant_cooperator_pennies - d$sd)

#reluctant_defector_pennies
e <- summarySE(inf_05_5, measurevar="reluctant_defector_pennies", groupvars=c("step"))
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
penny_plot1 = penny_plot1 + xlab("Step") + ylab("Pennies")
p1 = penny_plot1 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))

aa <- summarySE(inf_05_10, measurevar="monitor_pennies", groupvars=c("step"))
aa$highError <- (aa$monitor_pennies + aa$sd)
aa$lowError <- (aa$monitor_pennies - aa$sd)

bb <- summarySE(inf_05_10, measurevar="always_defect_pennies", groupvars=c("step"))
bb$highError <- (bb$always_defect_pennies + bb$sd)
bb$lowError <- (bb$always_defect_pennies - bb$sd)

cc <- summarySE(inf_05_10, measurevar="cooperator_pennies", groupvars=c("step"))
cc$highError <- (cc$cooperator_pennies + cc$sd)
cc$lowError <- (cc$cooperator_pennies - cc$sd)

#reluctant_cooperator_pennies
dd <- summarySE(inf_05_10, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
dd$highError <- (dd$reluctant_cooperator_pennies + dd$sd)
dd$lowError <- (dd$reluctant_cooperator_pennies - dd$sd)

#reluctant_defector_pennies
ee <- summarySE(inf_05_10, measurevar="reluctant_defector_pennies", groupvars=c("step"))
ee$highError <- (ee$reluctant_defector_pennies + ee$sd)
ee$lowError <- (ee$reluctant_defector_pennies - ee$sd)

penny_plot2 = ggplot()+
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
penny_plot2 = penny_plot2 + xlab("Step") + ylab("Pennies")
p2 = penny_plot2 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))
###


aaa <- summarySE(inf_75_5, measurevar="monitor_pennies", groupvars=c("step"))
aaa$highError <- (aaa$monitor_pennies + aaa$sd)
aaa$lowError <- (aaa$monitor_pennies - aaa$sd)

bbb <- summarySE(inf_75_5, measurevar="always_defect_pennies", groupvars=c("step"))
bbb$highError <- (bbb$always_defect_pennies + bbb$sd)
bbb$lowError <- (bbb$always_defect_pennies - bbb$sd)

ccc <- summarySE(inf_75_5, measurevar="cooperator_pennies", groupvars=c("step"))
ccc$highError <- (ccc$cooperator_pennies + ccc$sd)
ccc$lowError <- (ccc$cooperator_pennies - ccc$sd)

#reluctant_cooperator_pennies
ddd <- summarySE(inf_75_5, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
ddd$highError <- (ddd$reluctant_cooperator_pennies + ddd$sd)
ddd$lowError <- (ddd$reluctant_cooperator_pennies - ddd$sd)

#reluctant_defector_pennies
eee <- summarySE(inf_75_5, measurevar="reluctant_defector_pennies", groupvars=c("step"))
eee$highError <- (eee$reluctant_defector_pennies + eee$sd)
eee$lowError <- (eee$reluctant_defector_pennies - eee$sd)


penny_plot3 = ggplot()+
  geom_ribbon(aes(x=aaa$step, ymin = aaa$lowError, ymax = aaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbb$step, ymin = bbb$lowError, ymax = bbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=ccc$step, ymin = ccc$lowError, ymax = ccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=ddd$step, ymin = ddd$lowError, ymax = ddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eee$step, ymin = eee$lowError, ymax = eee$highError), fill = "mediumpurple3") +
  geom_line(data=aaa, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=bbb, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=ccc, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=ddd, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=eee, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plot3 = penny_plot3 + xlab("Step") + ylab("Pennies")
p3 = penny_plot3 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))


######


aaaa <- summarySE(inf_75_10, measurevar="monitor_pennies", groupvars=c("step"))
aaaa$highError <- (aaaa$monitor_pennies + aaaa$sd)
aaaa$lowError <- (aaaa$monitor_pennies - aaaa$sd)

bbbb <- summarySE(inf_75_10, measurevar="always_defect_pennies", groupvars=c("step"))
bbbb$highError <- (bbbb$always_defect_pennies + bbbb$sd)
bbbb$lowError <- (bbbb$always_defect_pennies - bbbb$sd)

cccc <- summarySE(inf_75_10, measurevar="cooperator_pennies", groupvars=c("step"))
cccc$highError <- (cccc$cooperator_pennies + cccc$sd)
cccc$lowError <- (cccc$cooperator_pennies - cccc$sd)

#reluctant_cooperator_pennies
dddd <- summarySE(inf_75_10, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
dddd$highError <- (dddd$reluctant_cooperator_pennies + dddd$sd)
dddd$lowError <- (dddd$reluctant_cooperator_pennies - dddd$sd)

#reluctant_defector_pennies
eeee <- summarySE(inf_75_10, measurevar="reluctant_defector_pennies", groupvars=c("step"))
eeee$highError <- (eeee$reluctant_defector_pennies + eeee$sd)
eeee$lowError <- (eeee$reluctant_defector_pennies - eeee$sd)

penny_plot4 = ggplot()+
  geom_ribbon(aes(x=aaaa$step, ymin = aaaa$lowError, ymax = aaaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbbb$step, ymin = bbbb$lowError, ymax = bbbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=cccc$step, ymin = cccc$lowError, ymax = cccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=dddd$step, ymin = dddd$lowError, ymax = dddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eeee$step, ymin = eeee$lowError, ymax = eeee$highError), fill = "mediumpurple3") +
  geom_line(data=aaaa, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=bbbb, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=cccc, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=dddd, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=eeee, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plot4 = penny_plot4 + xlab("Step") + ylab("Pennies")
p4 = penny_plot4 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))


####




aaaaa <- summarySE(inf_09_5, measurevar="monitor_pennies", groupvars=c("step"))
aaaaa$highError <- (aaaaa$monitor_pennies + aaaaa$sd)
aaaaa$lowError <- (aaaaa$monitor_pennies - aaaaa$sd)

bbbbb <- summarySE(inf_09_5, measurevar="always_defect_pennies", groupvars=c("step"))
bbbbb$highError <- (bbbbb$always_defect_pennies + bbbbb$sd)
bbbbb$lowError <- (bbbbb$always_defect_pennies - bbbbb$sd)

ccccc <- summarySE(inf_09_5, measurevar="cooperator_pennies", groupvars=c("step"))
ccccc$highError <- (ccccc$cooperator_pennies + ccccc$sd)
ccccc$lowError <- (ccccc$cooperator_pennies - ccccc$sd)

#reluctant_cooperator_pennies
ddddd <- summarySE(inf_09_5, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
ddddd$highError <- (ddddd$reluctant_cooperator_pennies + ddddd$sd)
ddddd$lowError <- (ddddd$reluctant_cooperator_pennies - ddddd$sd)

#reluctant_defector_pennies
eeeee <- summarySE(inf_09_5, measurevar="reluctant_defector_pennies", groupvars=c("step"))
eeeee$highError <- (eeeee$reluctant_defector_pennies + eeeee$sd)
eeeee$lowError <- (eeeee$reluctant_defector_pennies - eeeee$sd)

penny_plot5 = ggplot()+
  geom_ribbon(aes(x=aaaaa$step, ymin = aaaaa$lowError, ymax = aaaaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbbbb$step, ymin = bbbbb$lowError, ymax = bbbbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=ccccc$step, ymin = ccccc$lowError, ymax = ccccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=ddddd$step, ymin = ddddd$lowError, ymax = ddddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eeeee$step, ymin = eeeee$lowError, ymax = eeeee$highError), fill = "mediumpurple3") +
  geom_line(data=aaaaa, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=bbbbb, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=ccccc, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=ddddd, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=eeeee, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plot5 = penny_plot5 + xlab("Step") + ylab("Pennies")

p5 = penny_plot5 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))

######




aaaaaa <- summarySE(inf_09_10, measurevar="monitor_pennies", groupvars=c("step"))
aaaaaa$highError <- (aaaaaa$monitor_pennies + aaaaaa$sd)
aaaaaa$lowError <- (aaaaaa$monitor_pennies - aaaaaa$sd)

bbbbbb <- summarySE(inf_09_10, measurevar="always_defect_pennies", groupvars=c("step"))
bbbbbb$highError <- (bbbbbb$always_defect_pennies + bbbbbb$sd)
bbbbbb$lowError <- (bbbbbb$always_defect_pennies - bbbbbb$sd)

cccccc <- summarySE(inf_09_10, measurevar="cooperator_pennies", groupvars=c("step"))
cccccc$highError <- (cccccc$cooperator_pennies + cccccc$sd)
cccccc$lowError <- (cccccc$cooperator_pennies - cccccc$sd)

#reluctant_cooperator_pennies
dddddd <- summarySE(inf_09_10, measurevar="reluctant_cooperator_pennies", groupvars=c("step"))
dddddd$highError <- (dddddd$reluctant_cooperator_pennies + dddddd$sd)
dddddd$lowError <- (dddddd$reluctant_cooperator_pennies - dddddd$sd)

#reluctant_defector_pennies
eeeeee <- summarySE(inf_09_10, measurevar="reluctant_defector_pennies", groupvars=c("step"))
eeeeee$highError <- (eeeeee$reluctant_defector_pennies + eeeeee$sd)
eeeeee$lowError <- (eeeeee$reluctant_defector_pennies - eeeeee$sd)

penny_plot6 = ggplot()+
  geom_ribbon(aes(x=aaaaaa$step, ymin = aaaaaa$lowError, ymax = aaaaaa$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=bbbbbb$step, ymin = bbbbbb$lowError, ymax = bbbbbb$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=cccccc$step, ymin = cccccc$lowError, ymax = cccccc$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=dddddd$step, ymin = dddddd$lowError, ymax = dddddd$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=eeeeee$step, ymin = eeeeee$lowError, ymax = eeeeee$highError), fill = "mediumpurple3") +
  geom_line(data=aaaaaa, aes(x=step, y=monitor_pennies),colour="forestgreen") +
  geom_line(data=bbbbbb, aes(x=step, y=always_defect_pennies),colour="darkorchid4")+
  geom_line(data=cccccc, aes(x=step, y=cooperator_pennies),colour="dodgerblue4")+
  geom_line(data=dddddd, aes(x=step, y=reluctant_cooperator_pennies),colour="cadetblue")+
  geom_line(data=eeeeee, aes(x=step, y=reluctant_defector_pennies),colour="mediumpurple4")+
  theme_few()
penny_plot6 = penny_plot6 + xlab("Step") + ylab("Pennies")

p6 = penny_plot6 + coord_cartesian(xlim = c(-20, 200), ylim = c(-20, 200))
p6

png(file="Influencer_sanc_09_July623.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plot1, penny_plot2, penny_plot3, penny_plot4, penny_plot5, penny_plot6, nrow=2, ncol=3,  labels=c('\nA.', '\nB.', '\nC.', '\nD.', '\nE.', '\nF.'), label_size=10, align="v")
dev.off()

png(file="Influencer_sanc_09_zoomed_July623.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(p1, p2, p3, p4, p5, p6, nrow=2, ncol=3,  labels=c('\nA.', '\nB.', '\nC.', '\nD.', '\nE.', '\nF.'), label_size=10, align="v")
dev.off()

#### Colin Experimenting ####
inf_sanc_04 <- read.csv ("Prosocial_April_30_influencer_10_ps_04-table.csv", skip=6, header=T)
inf_sanc_09 <- read.csv ("Prosocial_April_30_influencer_10_ps_09-table.csv", skip=6, header=T)

inf_sanc <-
  bind_rows(inf_sanc_04,inf_sanc_09)

inf_sanc_ <- inf_sanc %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

inf_sanc_ %>%
  select(run_num, step, local_probinfluence, local_sphereinfluence, prob_sanction, coopall_pennies_frac) %>%
  mutate(local_probinfluence = as.factor(local_probinfluence)) %>%
  mutate(local_sphereinfluence = as.factor(local_sphereinfluence)) %>%
  mutate(prob_sanction = as.factor(prob_sanction)) %>%
  group_by(step, local_probinfluence, local_sphereinfluence, prob_sanction) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = factor(prob_sanction), linetype = local_sphereinfluence),
            linewidth = 1, position=position_dodge(width=10)) +
  labs(x= "Step", y= "Cooperator's proportion of total wealth", title = "Local influencer") +
  #scale_color_brewer("Prob sanction", palette = "Set2") +
  scale_color_manual("Sanction\nprobability", values = stef_colors) +
  scale_linetype_manual("Sphere of Influence", values = c("dotted","solid")) +
  theme_bw() +
  #guides(colour = "none") +
  geom_hline( yintercept = .5, size = .5, color = "black", linetype="dashed")
ggsave("infl_wealth_all.png", units = "in", width = 6, height = 4)
