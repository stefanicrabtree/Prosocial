## Summary Function for SE ####

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

#### Needed Libraries ####

#install.packages("ggthemes") # Use install lines like this for any missing packages below 
library(ggthemes)
library(dplyr)
library(reshape)
library(lattice)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

getwd()

#### Mutual Monitoring ####

MMa <- read.csv ("Prosocial_July_7_mm_only_PGG_2_Prob_4-table.csv", skip=6, header=T)
MMb <- read.csv("Prosocial_July_7_mm_only_PGG_1_5_Prob_4-table.csv", skip=6, header = T)
MM <- bind_rows(MMa,MMb)

## In MM we have varying sanction fines. We don't do that for the other types of plots
MM_4 <-subset(MM, sanction_fine=="4")
MM_5 <-subset(MM, sanction_fine=="5")
MM_6 <-subset(MM, sanction_fine=="6")
MM_10 <-subset(MM, sanction_fine=="10")

##First we will look at the MM pennies, and then the pops
## Change what comes after the underscore to get this correct

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
#

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

##### MM - Proportion plots  #####
MMa <- read.csv("Prosocial_April_9_mm_only_PGG_2_sanction_tax_0_50-table.csv", skip=6, header=T)
MMb <- read.csv("Prosocial_April_14_mm_1_5_prob_sanc_90_tax_05-table.csv", skip=6, header = T)
MM <- bind_rows(MMa,MMb) %>%
  mutate(Strategy = "MM") %>% 
  replace(is.na(.), 0)

stef_colors <- c("cadetblue3", "dodgerblue2", "mediumpurple3", "darkorchid3")

MM_ <- MM %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

MM_ %>%
  filter(public_goods_game_multiplier == 2) %>%
  select(run_num, public_goods_game_multiplier, step, sanction_fine, coopall_pennies_frac) %>%
  mutate(public_goods_game_multiplier = as.factor(public_goods_game_multiplier)) %>%
  group_by(step,sanction_fine, public_goods_game_multiplier) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = factor(sanction_fine)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Proportion", title = "Cooperators' proportion of total wealth", color = "Sanction fine") +
  scale_color_manual(values = stef_colors) +
  scale_linetype_manual("CPRm", values = c("dotted","solid")) +
  theme_few() +
  theme(legend.position = c(0.8, 0.2)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("mm_wealth_all.png", units = "in", width = 4, height = 4, scale = 1.5)

MM_ %>%
  select(run_num, public_goods_game_multiplier, step, sanction_fine, coopall_pennies_frac) %>%
  mutate(public_goods_game_multiplier = as.factor(public_goods_game_multiplier)) %>%
  group_by(step,sanction_fine, public_goods_game_multiplier) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = factor(sanction_fine), linetype = public_goods_game_multiplier),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperators' proportion of total wealth", color = "Sanction fine") +
  scale_color_manual(values = stef_colors) +
  scale_linetype_manual("CPRm", values = c("dotted","solid")) +
  theme_few() +
  theme(legend.position = c(0.8, 0.4)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("S_9_mm_wealth_all_CPRboth.png", units = "in", width = 4, height = 4, scale = 1.5)

##### MM - Sensitivity experiments #####
###### MM - More Defectors ######

old_def <- read.csv("Prosocial_April_9_mm_only_PGG_2_sanction_tax_0_50-table.csv", skip=6, header=T) %>%
  mutate(Label = "7%") %>%
  filter(sanction_fine == 6) %>% 
  replace(is.na(.), 0)

more_def1 <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_moredefect1-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>") %>%
  mutate(Label = "14%") %>% 
  replace(is.na(.), 0)

more_def2 <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_moredefect2-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>")  %>%
  mutate(Label = "28%") %>% 
  replace(is.na(.), 0)

more_def3 <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_moredefect3-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>")  %>%
  mutate(Label = "38%") %>% 
  replace(is.na(.), 0)

more_def <- bind_rows(old_def,more_def1,more_def2,more_def3) %>%
  mutate(Strategy = "MM") %>% 
  replace(is.na(.), 0)

def <- more_def %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

def %>%
  select(run_num, step, Label, coopall_pennies_frac) %>%
  group_by(step,Label) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = fct_relevel(Label,c("7%","14%","28%","38%"))),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", color = "Defector prop. (%)") +
  scale_color_manual(values = stef_colors) +
  theme_few() +
  theme(legend.position = c(0.85, 0.3)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("S_1_mm_more_defectors.png", units = "in", width = 6, height = 4)


###### MM - More Monitors ######
old_mon <- read.csv("Prosocial_April_9_mm_only_PGG_2_sanction_tax_0_50-table.csv", skip=6, header=T) %>%
  mutate(Label = "7%") %>%
  filter(sanction_fine == 6) %>% 
  replace(is.na(.), 0)

more_mon1 <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_moremonitor1-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>") %>%
  mutate(Label = "14%") %>% 
  replace(is.na(.), 0)

more_mon2 <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_moremonitor2-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>")  %>%
  mutate(Label = "28%") %>% 
  replace(is.na(.), 0)

more_mon3 <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_moremonitor3-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>")  %>%
  mutate(Label = "38%") %>% 
  replace(is.na(.), 0)

more_mon <- bind_rows(old_mon,more_mon1,more_mon2,more_mon3) %>%
  mutate(Strategy = "MM")

stef_colors <- c("cadetblue3", "dodgerblue2", "mediumpurple3", "darkorchid3")

mon <- more_mon %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

p_mon <- mon %>%
  select(run_num, step, Label, coopall_pennies_frac) %>%
  group_by(step,Label) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = fct_relevel(Label,c("7%","14%","28%","38%"))),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", color = "Monitor prop. (%)") +
  scale_color_manual(values = stef_colors) +
  theme_few() +
  theme(legend.position = c(0.75, 0.25)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
p_mon
ggsave("mm_more_monitors.png", units = "in", width = 6, height = 4)

###### MM - Pop Size Exp ######
pop_size <- read.csv("Prosocial_April_14_2024_For_Github Apr_14_agentpops_size-table.csv", skip=6, header=T, na.strings = "<RuntimePrimitiveException>") %>% 
  replace(is.na(.), 0) %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

p_popsize <- pop_size %>%
  select(run_num, step, pop_size, coopall_pennies_frac) %>%
  group_by(step,pop_size) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = factor(pop_size)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", color = "Pop. size") +
  scale_color_manual(values = stef_colors) +
  theme_few() +
  theme(legend.position = c(0.8, 0.25)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
p_popsize 
ggsave("mm_popsize_var.png", units = "in", width = 6, height = 4)

cowplot::plot_grid(p_mon, p_popsize, labels = "AUTO")
ggsave("S_8_supp_monitors_popsize.png", units = "in", width = 8, height = 4)

#### Despot aka Global Leadership ####
despot40 <- read.csv ("Prosocial_March_15_despot_ps_40-table.csv", skip=6, header=T)
despot90 <- read.csv ("Prosocial_March_15_despot_ps_90-table.csv", skip=6, header=T)

despot <- bind_rows(despot40,despot90) %>%
  mutate(Strategy = "Global") %>% 
  replace(is.na(.), 0)

despot_ <- despot %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

despot_ %>%
  select(run_num, step, prob_sanction, coopall_pennies_frac) %>%
  group_by(step, prob_sanction) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = factor(prob_sanction)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Proportion", title = "Cooperators' proportion of total wealth", color = "Sanction\nprobability") +
  scale_color_manual(values = stef_colors) +
  scale_linetype_manual("PGGm", values = c("dotted","solid")) +
  theme_few() +
  theme(legend.position = c(0.8, 0.2)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("despot_wealth_all.png", units = "in", width = 4, height = 4, scale = 1.5)

#### Influencer ####

inf_sanc_09 <- read.csv ("Prosocial_April_9_2024_influencer_09_sanct-table.csv", skip=6, header=T)

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
penny_plot1 = penny_plot1 + xlab("Sphere Influence 5, Prob Influence 50%") + ylab("Pennies")
penny_plot1 
p1 = penny_plot1 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 200))

coord_cartesian()


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
penny_plot2 = penny_plot2 + xlab("Sphere Influence 10, Prob Influence 50%") + ylab("Pennies")
penny_plot2
p2 = penny_plot2 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 200))
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
penny_plot3
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
penny_plot4
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
penny_plot5 = penny_plot5 + xlab("Sphere Influence 5, Prob Influence 90%") + ylab("Pennies")
penny_plot5
p5 = penny_plot5 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 150))

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
penny_plot6 = penny_plot6 + xlab("Sphere Influence 10, Prob Influence 90%") + ylab("Pennies")
penny_plot6
p6 = penny_plot6 + coord_cartesian(xlim = c(-20, 100), ylim = c(-20, 150))


png(file="Influencer_sanc_09_July_6.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plot1, penny_plot2, penny_plot3, penny_plot4, penny_plot5, penny_plot6, nrow=2, ncol=3,  labels=c('\nA.', '\nB.', '\nC.', '\nD.', '\nE.', '\nF.'), label_size=10, align="v")
dev.off()

png(file="Influencer_sanc_09_zoomed_July_6.png", height=10, width=5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(p1, p2, p5, p6,  ncol=1,  labels=c('\na.', '\nb.', '\nc.', '\nd.'), label_size=10, align="v")
dev.off()

png(file="Influencer_sanc_09_for_pub_July.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plot1, penny_plot2, penny_plot5, penny_plot6, nrow=2, ncol=2,  labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()

#p2, p1, p5, p6

png(file="All_Four_July.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot_grid(penny_plotMM6, penp_despot2, penp_copyL90, penny_plot6, nrow=2, ncol=2,  labels=c('\nA.', '\nB.', '\nC.', '\nD.'), label_size=10, align="v")
dev.off()



png(file="Legend.png", height=8, width=10.5, units="in", res=600)
par(mar=c(4.5,4.5,1.5,1.2))
par(oma=c(0,0,0,0))
plot(reluctant_defector_pennies ~ step, data=e, type="l", ylim=c(0,650), lty=1, lwd=2, col='deeppink4', xlab="",ylab="",main="Wealth with Influencer Leadership, Sphere 10, Probability 90%")
(legend(x="topleft", legend=c("monitors","always defect","always cooperate","reluctant cooperate","reluctant defect"),
        lty = c(1,1,1,1,1),
        col = c("forestgreen","darkorchid4","dodgerblue4","cadetblue","mediumpurple4"),
        lwd = c(4,4,4,4,4)))
dev.off()

##### Influencer Proportion plots  #####
inf_sanc_09 <- read.csv ("Prosocial_April_9_2024_influencer_09_sanct-table.csv", skip=6, header=T)
inf_sanc_09_ <- inf_sanc_09 %>% 
  replace(is.na(.), 0) %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies + infl_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies + infl_pennies) / total_pennies) %>%
  mutate(Strategy = "Influencer")

inf_sanc_09_ %>%
  filter(local_probinfluence == 0.9 | local_probinfluence == 0.5) %>%
  filter(local_sphereinfluence != 3) %>%
  select(run_num, step, local_probinfluence, local_sphereinfluence, coopall_pennies_frac) %>%
  mutate(local_probinfluence = as.factor(local_probinfluence)) %>%
  mutate(local_sphereinfluence = as.factor(local_sphereinfluence)) %>%
  group_by(step, local_probinfluence, local_sphereinfluence) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = factor(local_probinfluence), linetype = local_sphereinfluence),
            linewidth = 1, position=position_dodge(width=10)) +
  labs(x= "Step", y= "Cooperators' proportion of total wealth") +
  scale_linetype_manual("Sphere of Influence", values = c("dotted","solid")) +
  scale_color_manual("Prob. of Influence", values = stef_colors) +
  theme_few() +
  theme(legend.position = c(0.8, 0.2)) +
  geom_hline( yintercept = .5, size = 1, color = "black", linetype="dashed")
ggsave("Figure_3e.png", units = "in", height = 4, width = 4,scale = 1.5)

inf_sanc_40 <- read.csv ("Prosocial_influencer_04_sanct-table.csv", skip=6, header=T)
inf_sanc_40_ <- inf_sanc_40 %>% 
  replace(is.na(.), 0) %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies + infl_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies + infl_pennies) / total_pennies) %>%
  mutate(Strategy = "Influencer")

inf_sanc_40_ %>%
  select(run_num, step, local_probinfluence, local_sphereinfluence, coopall_pennies_frac) %>%
  mutate(local_probinfluence = as.factor(local_probinfluence)) %>%
  mutate(local_sphereinfluence = as.factor(local_sphereinfluence)) %>%
  group_by(step, local_probinfluence, local_sphereinfluence) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = factor(local_probinfluence), linetype = local_sphereinfluence),
            linewidth = 1, position=position_dodge(width=10)) +
  labs(x= "Step", y= "Cooperators' proportion of total wealth") +
  scale_linetype_manual("Sphere of Influence", values = c("dotted","dashed","solid")) +
  scale_color_manual("Prob. of Influence", values = stef_colors) +
  theme_few() +
  theme(legend.position = c(0.8, 0.3)) +
  geom_hline( yintercept = .5, size = 1, color = "black", linetype="dashed")
ggsave("S_19_infl_wealth_all_sanc40.png", units = "in", height = 4, width = 4,scale = 1.5)

#### Combined plot ####
all <- bind_rows(
  filter(MM_, public_goods_game_multiplier == 2 & sanction_fine == 6),
  filter(despot_, prob_sanction == 0.9),
  filter(inf_sanc_09_, local_sphereinfluence == 10 & local_probinfluence == 0.9)
)

all %>%
  select(run_num, step, Strategy, coopall_pennies_frac) %>%
  group_by(step, Strategy) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  geom_line(aes(step, avg_coop_pennies, color = Strategy),
            linewidth = 1, position=position_dodge(width=10)) +
  labs(x= "Step", y= "Proportion", title = "Leadership strategies") +
  scale_color_manual("Strategy", values = stef_colors) +
  theme_few() +
  theme(legend.position = c(0.9, 0.2)) +
  geom_hline( yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("all_wealth_all_apr14_2024.png", units = "in", width = 6, height = 4, scale = 1.5)

