


##### First let's do Mutual Monitor populations

MM <- read.csv ("Prosocial_Sept_19_mm_only_PGG_1_5-table.csv", skip=6, header=T)
MM <- read.csv ("Prosocial_Sept_19_2022_mm_only_PGG_2-table.csv", skip=6, header=T)
MM <- read.csv ("Prosocial_July_7_mm_only_PGG_1_5_Prob_4-table.csv", skip=6, header=T)
MM <- read.csv ("Prosocial_July_7_mm_only_PGG_2_Prob_4-table.csv", skip=6, header=T)
MM <- read.csv ("Prosocial_July_7_mm_only_PGG_2_Prob_9-table.csv", skip=6, header=T)

## In MM we have varying sanction fines. We don't do that for the other types of plots
MM_4 <-subset(MM, sanction_fine=="4")
MM_5 <-subset(MM, sanction_fine=="5")
MM_6 <-subset(MM, sanction_fine=="6")
MM_10 <-subset(MM, sanction_fine=="10")


##Plotting numbers of agents in MM scenarios

a1 <- summarySE(MM_4, measurevar="count_monitor", groupvars=c("step"))
a1$highError <- (a1$count_monitor + a1$sd)
a1$lowError <- (a1$count_monitor - a1$sd)

b1 <- summarySE(MM_4, measurevar="count_always_defect", groupvars=c("step"))
b1$highError <- (b1$count_always_defect + b1$sd)
b1$lowError <- (b1$count_always_defect - b1$sd)

c1 <- summarySE(MM_4, measurevar="count_cooperate", groupvars=c("step"))
c1$highError <- (c1$count_cooperate + c1$sd)
c1$lowError <- (c1$count_cooperate - c1$sd)

#reluctant_cooperator_pennies
d1 <- summarySE(MM_4, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d1$highError <- (d1$count_reluctant_cooperate + d1$sd)
d1$lowError <- (d1$count_reluctant_cooperate - d1$sd)

#reluctant_defector_pennies
e1 <- summarySE(MM_4, measurevar="count_reluctant_defect", groupvars=c("step"))
e1$highError <- (e1$count_reluctant_defect + e1$sd)
e1$lowError <- (e1$count_reluctant_defect - e1$sd)

## And here are the plots

pop_plot = ggplot()+
  geom_ribbon(aes(x=a1$step, ymin = a1$lowError, ymax = a1$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b1$step, ymin = b1$lowError, ymax = b1$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c1$step, ymin = c1$lowError, ymax = c1$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d1$step, ymin = d1$lowError, ymax = d1$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e1$step, ymin = e1$lowError, ymax = e1$highError), fill = "mediumpurple3") +
  geom_line(data=a1, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b1, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c1, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d1, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e1, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp1 = pop_plot + xlab("Step, Sanction Fine 4") + ylab("Population")


a2 <- summarySE(MM_5, measurevar="count_monitor", groupvars=c("step"))
a2$highError <- (a2$count_monitor + a2$sd)
a2$lowError <- (a2$count_monitor - a2$sd)

b2 <- summarySE(MM_5, measurevar="count_always_defect", groupvars=c("step"))
b2$highError <- (b2$count_always_defect + b2$sd)
b2$lowError <- (b2$count_always_defect - b2$sd)

c2 <- summarySE(MM_5, measurevar="count_cooperate", groupvars=c("step"))
c2$highError <- (c2$count_cooperate + c2$sd)
c2$lowError <- (c2$count_cooperate - c2$sd)

#reluctant_cooperator_pennies
d2 <- summarySE(MM_5, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d2$highError <- (d2$count_reluctant_cooperate + d2$sd)
d2$lowError <- (d2$count_reluctant_cooperate - d2$sd)

#reluctant_defector_pennies
e2 <- summarySE(MM_5, measurevar="count_reluctant_defect", groupvars=c("step"))
e2$highError <- (e2$count_reluctant_defect + e2$sd)
e2$lowError <- (e2$count_reluctant_defect - e2$sd)

## And here are the plots

pop_plot2 = ggplot()+
  geom_ribbon(aes(x=a2$step, ymin = a2$lowError, ymax = a2$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b2$step, ymin = b2$lowError, ymax = b2$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c2$step, ymin = c2$lowError, ymax = c2$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d2$step, ymin = d2$lowError, ymax = d2$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e2$step, ymin = e2$lowError, ymax = e2$highError), fill = "mediumpurple3") +
  geom_line(data=a2, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b2, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c2, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d2, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e2, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp2 = pop_plot2 + xlab("Step, Sanction Fine 5") + ylab("Population")


####

a3 <- summarySE(MM_6, measurevar="count_monitor", groupvars=c("step"))
a3$highError <- (a3$count_monitor + a3$sd)
a3$lowError <- (a3$count_monitor - a3$sd)

b3 <- summarySE(MM_6, measurevar="count_always_defect", groupvars=c("step"))
b3$highError <- (b3$count_always_defect + b3$sd)
b3$lowError <- (b3$count_always_defect - b3$sd)

c3 <- summarySE(MM_6, measurevar="count_cooperate", groupvars=c("step"))
c3$highError <- (c3$count_cooperate + c3$sd)
c3$lowError <- (c3$count_cooperate - c3$sd)

#reluctant_cooperator_pennies
d3 <- summarySE(MM_6, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d3$highError <- (d3$count_reluctant_cooperate + d3$sd)
d3$lowError <- (d3$count_reluctant_cooperate - d3$sd)

#reluctant_defector_pennies
e3 <- summarySE(MM_6, measurevar="count_reluctant_defect", groupvars=c("step"))
e3$highError <- (e3$count_reluctant_defect + e3$sd)
e3$lowError <- (e3$count_reluctant_defect - e3$sd)

## And here are the plots

pop_plot3 = ggplot()+
  geom_ribbon(aes(x=a3$step, ymin = a3$lowError, ymax = a3$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b3$step, ymin = b3$lowError, ymax = b3$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c3$step, ymin = c3$lowError, ymax = c3$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d3$step, ymin = d3$lowError, ymax = d3$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e3$step, ymin = e3$lowError, ymax = e3$highError), fill = "mediumpurple3") +
  geom_line(data=a3, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b3, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c3, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d3, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e3, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp3 = pop_plot3 + xlab("Step, Sanction Fine 6") + ylab("Population")

####

a4 <- summarySE(MM_10, measurevar="count_monitor", groupvars=c("step"))
a4$highError <- (a4$count_monitor + a4$sd)
a4$lowError <- (a4$count_monitor - a4$sd)

b4 <- summarySE(MM_10, measurevar="count_always_defect", groupvars=c("step"))
b4$highError <- (b4$count_always_defect + b4$sd)
b4$lowError <- (b4$count_always_defect - b4$sd)

c4 <- summarySE(MM_10, measurevar="count_cooperate", groupvars=c("step"))
c4$highError <- (c4$count_cooperate + c4$sd)
c4$lowError <- (c4$count_cooperate - c4$sd)

#reluctant_cooperator_pennies
d4 <- summarySE(MM_10, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d4$highError <- (d4$count_reluctant_cooperate + d4$sd)
d4$lowError <- (d4$count_reluctant_cooperate - d4$sd)

#reluctant_defector_pennies
e4 <- summarySE(MM_10, measurevar="count_reluctant_defect", groupvars=c("step"))
e4$highError <- (e4$count_reluctant_defect + e4$sd)
e4$lowError <- (e4$count_reluctant_defect - e4$sd)

## And here are the plots

pop_plot4 = ggplot()+
  geom_ribbon(aes(x=a4$step, ymin = a4$lowError, ymax = a4$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b4$step, ymin = b4$lowError, ymax = b4$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c4$step, ymin = c4$lowError, ymax = c4$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d4$step, ymin = d4$lowError, ymax = d4$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e4$step, ymin = e4$lowError, ymax = e4$highError), fill = "mediumpurple3") +
  geom_line(data=a4, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b4, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c4, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d4, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e4, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp4 = pop_plot4 + xlab("Step, Sanction Fine 10") + ylab("Population")


png(file="Populations_MM_2_Prob_9_July.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(pp1, pp2, pp3, pp4, nrow=2, ncol=2,  labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()




######


#pp5
#ppD = penp_despot + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 150))



despot90 <- read.csv ("Prosocial_April_30_despot_ps_9-table.csv", skip=6, header=T)

ff <- summarySE(despot90, measurevar="count_monitor", groupvars=c("step"))
ff$highError <- (ff$count_monitor + ff$sd)
ff$lowError <- (ff$count_monitor - ff$sd)

gg <- summarySE(despot90, measurevar="count_always_defect", groupvars=c("step"))
gg$highError <- (gg$count_always_defect + gg$sd)
gg$lowError <- (gg$count_always_defect - gg$sd)

hh <- summarySE(despot90, measurevar="count_cooperate", groupvars=c("step"))
hh$highError <- (hh$count_cooperate + hh$sd)
hh$lowError <- (hh$count_cooperate - hh$sd)

#reluctant_cooperator_pennies
kk <- summarySE(despot90, measurevar="count_reluctant_cooperate", groupvars=c("step"))
kk$highError <- (kk$count_reluctant_cooperate + kk$sd)
kk$lowError <- (kk$count_reluctant_cooperate - kk$sd)

#reluctant_defector_pennies
mm <- summarySE(despot90, measurevar="count_reluctant_defect", groupvars=c("step"))
mm$highError <- (mm$count_reluctant_defect + mm$sd)
mm$lowError <- (mm$count_reluctant_defect - mm$sd)

pop_plot6 = ggplot()+
  geom_ribbon(aes(x=ff$step, ymin = ff$lowError, ymax = ff$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=gg$step, ymin = gg$lowError, ymax = gg$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=hh$step, ymin = hh$lowError, ymax = hh$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=kk$step, ymin = kk$lowError, ymax = kk$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=mm$step, ymin = mm$lowError, ymax = mm$highError), fill = "mediumpurple3") +
  geom_line(data=ff, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=gg, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=hh, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=kk, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=mm, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
pp6 = pop_plot6 + xlab("Step, Global Leader, Probability Sanction 90%") + ylab("Population")
#ppD2 = penp_despot2 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 150))


despot40 <- read.csv ("Prosocial_April_30_despot_ps_4-table.csv", skip=6, header=T)



fff <- summarySE(despot40, measurevar="count_monitor", groupvars=c("step"))
fff$highError <- (fff$count_monitor + fff$sd)
fff$lowError <- (fff$count_monitor - fff$sd)

ggg <- summarySE(despot40, measurevar="count_always_defect", groupvars=c("step"))
ggg$highError <- (ggg$count_always_defect + ggg$sd)
ggg$lowError <- (ggg$count_always_defect - ggg$sd)

hhh <- summarySE(despot40, measurevar="count_cooperate", groupvars=c("step"))
hhh$highError <- (hhh$count_cooperate + hhh$sd)
hhh$lowError <- (hhh$count_cooperate - hhh$sd)

#reluctant_cooperator_pennies
kkk <- summarySE(despot40, measurevar="count_reluctant_cooperate", groupvars=c("step"))
kkk$highError <- (kkk$count_reluctant_cooperate + kkk$sd)
kkk$lowError <- (kkk$count_reluctant_cooperate - kkk$sd)

#reluctant_defector_pennies
mmm <- summarySE(despot40, measurevar="count_reluctant_defect", groupvars=c("step"))
mmm$highError <- (mmm$count_reluctant_defect + mmm$sd)
mmm$lowError <- (mmm$count_reluctant_defect - mmm$sd)

pop_plot7 = ggplot()+
  geom_ribbon(aes(x=ff$step, ymin = fff$lowError, ymax = fff$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=gg$step, ymin = ggg$lowError, ymax = ggg$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=hh$step, ymin = hhh$lowError, ymax = hhh$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=kk$step, ymin = kkk$lowError, ymax = kkk$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=mm$step, ymin = mmm$lowError, ymax = mmm$highError), fill = "mediumpurple3") +
  geom_line(data=fff, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=ggg, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=hhh, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=kkk, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=mmm, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()
pp7 = pop_plot7 + xlab("Step, Global Leader, Probability Sanction 40%") + ylab("Population")




copy_leader <- read.csv ("Prosocial_April_30_2023_ps_04_copy_defector-table.csv", skip=6, header=T)

a5 <- summarySE(copy_leader, measurevar="count_monitor", groupvars=c("step"))
a5$highError <- (a5$count_monitor + a5$sd)
a5$lowError <- (a5$count_monitor - a5$sd)

b5 <- summarySE(copy_leader, measurevar="count_always_defect", groupvars=c("step"))
b5$highError <- (b5$count_always_defect + b5$sd)
b5$lowError <- (b5$count_always_defect - b5$sd)

c5 <- summarySE(copy_leader, measurevar="count_cooperate", groupvars=c("step"))
c5$highError <- (c5$count_cooperate + c5$sd)
c5$lowError <- (c5$count_cooperate - c5$sd)

#reluctant_cooperator_pennies
d5 <- summarySE(copy_leader, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d5$highError <- (d5$count_reluctant_cooperate + d5$sd)
d5$lowError <- (d5$count_reluctant_cooperate - d5$sd)

#reluctant_defector_pennies
e5 <- summarySE(copy_leader, measurevar="count_reluctant_defect", groupvars=c("step"))
e5$highError <- (e5$count_reluctant_defect + e5$sd)
e5$lowError <- (e5$count_reluctant_defect - e5$sd)

#colors <- c("count_monitor" = "forestgreen", "count_always_defect" = "darkorchid4", "count_cooperate" = "dodgerblue4", "count_reluctant_cooperate" = "cadetblue", "count_reluctant_defect" = "mediumpurple4")
pop_plot8 = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a5$lowError, ymax = a5$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b$step, ymin = b5$lowError, ymax = b5$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c$step, ymin = c5$lowError, ymax = c5$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d$step, ymin = d5$lowError, ymax = d5$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e$step, ymin = e5$lowError, ymax = e5$highError), fill = "mediumpurple3") +
  geom_line(data=a5, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b5, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c5, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d5, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e5, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp8 = pop_plot8 + xlab("Step, Copy Defector, Probability Sanction 40%") + ylab("Population")


copy_leader2 <- read.csv ("Prosocial_April_30_2023_ps_09_copy_defector-table.csv", skip=6, header=T)

a6 <- summarySE(copy_leader2, measurevar="count_monitor", groupvars=c("step"))
a6$highError <- (a6$count_monitor + a6$sd)
a6$lowError <- (a6$count_monitor - a6$sd)

b6 <- summarySE(copy_leader2, measurevar="count_always_defect", groupvars=c("step"))
b6$highError <- (b6$count_always_defect + b6$sd)
b6$lowError <- (b6$count_always_defect - b6$sd)

c6 <- summarySE(copy_leader2, measurevar="count_cooperate", groupvars=c("step"))
c6$highError <- (c6$count_cooperate + c6$sd)
c6$lowError <- (c6$count_cooperate - c6$sd)

#reluctant_cooperator_pennies
d6 <- summarySE(copy_leader2, measurevar="count_reluctant_cooperate", groupvars=c("step"))
d6$highError <- (d6$count_reluctant_cooperate + d6$sd)
d6$lowError <- (d6$count_reluctant_cooperate - d6$sd)

#reluctant_defector_pennies
e6 <- summarySE(copy_leader2, measurevar="count_reluctant_defect", groupvars=c("step"))
e6$highError <- (e6$count_reluctant_defect + e6$sd)
e6$lowError <- (e6$count_reluctant_defect - e6$sd)

#colors <- c("count_monitor" = "forestgreen", "count_always_defect" = "darkorchid4", "count_cooperate" = "dodgerblue4", "count_reluctant_cooperate" = "cadetblue", "count_reluctant_defect" = "mediumpurple4")
pop_plot9 = ggplot()+
  geom_ribbon(aes(x=a$step, ymin = a6$lowError, ymax = a6$highError), fill = "mediumseagreen") +
  geom_ribbon(aes(x=b$step, ymin = b6$lowError, ymax = b6$highError), fill = "darkorchid3") +
  geom_ribbon(aes(x=c$step, ymin = c6$lowError, ymax = c6$highError), fill = "dodgerblue2") +
  geom_ribbon(aes(x=d$step, ymin = d6$lowError, ymax = d6$highError), fill = "cadetblue3") +
  geom_ribbon(aes(x=e$step, ymin = e6$lowError, ymax = e6$highError), fill = "mediumpurple3") +
  geom_line(data=a6, aes(x=step, y=count_monitor),colour="forestgreen") +
  geom_line(data=b6, aes(x=step, y=count_always_defect),colour="darkorchid4")+
  geom_line(data=c6, aes(x=step, y=count_cooperate),colour="dodgerblue4")+
  geom_line(data=d6, aes(x=step, y=count_reluctant_cooperate),colour="cadetblue")+
  geom_line(data=e6, aes(x=step, y=count_reluctant_defect),colour="mediumpurple4")+
  theme_few()

pp9 = pop_plot9 + xlab("Step, Copy Defector, Probability Sanction 90%") + ylab("Population")



png(file="Despot_to_Copy_Defector_Populations.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(pp7, pp6, pp8, pp9, nrow=2, ncol = 2, labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()

