library(ggplot2)
library(dplyr)
library(cowplot)
library(forcats)

old_def <- read.csv("Prosocial_July_7_mm_only_PGG_2_Prob_9-table.csv", skip=6, header=T) %>%
  mutate(Label = "7%") %>%
  filter(sanction_fine == 6)

more_def1 <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_moredefect1-table.csv", skip=6, header=T) %>%
  mutate(Label = "14%")

more_def2 <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_moredefect2-table.csv", skip=6, header=T)  %>%
  mutate(Label = "28%")

more_def3 <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_moredefect3-table.csv", skip=6, header=T)  %>%
  mutate(Label = "38%")

more_def <- bind_rows(old_def,more_def1,more_def2,more_def3) %>%
  mutate(Strategy = "MM")

stef_colors <- c("cadetblue3", "dodgerblue2", "mediumpurple3", "darkorchid3")

def <- more_def %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

def %>%
  select(run_num, step, Label, coopall_pennies_frac) %>%
  group_by(step,Label) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = fct_relevel(Label,c("7%","14%","28%","38%"))),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", color = "Defector prop. (%)") +
  scale_color_manual(values = stef_colors) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.2)) +
  #guides(colour = "none", linetype = "none") +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("mm_more_defectors.png", units = "in", width = 6, height = 4)


#### More Monitors ####
old_mon <- read.csv("Prosocial_July_7_mm_only_PGG_2_Prob_9-table.csv", skip=6, header=T) %>%
  mutate(Label = "7%") %>%
  filter(sanction_fine == 6)

more_mon1 <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_moremonitor1-table.csv", skip=6, header=T) %>%
  mutate(Label = "14%")

more_mon2 <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_moremonitor2-table.csv", skip=6, header=T)  %>%
  mutate(Label = "28%")

more_mon3 <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_moremonitor3-table.csv", skip=6, header=T)  %>%
  mutate(Label = "38%")

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
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = fct_relevel(Label,c("7%","14%","28%","38%"))),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", color = "Monitor prop. (%)") +
  scale_color_manual(values = stef_colors) +
  theme_bw() +
  theme(legend.position = c(0.75, 0.25)) +
  coord_cartesian(ylim = c(0,1)) +
  #guides(colour = "none", linetype = "none") +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
p_mon
ggsave("mm_more_monitors.png", units = "in", width = 6, height = 4)

#### Pop Size Exp ####
pop_size <- read.csv("Prosocial_July_30_2023_For_Github Nov_7_agentpops_size-table.csv", skip=6, header=T) %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

p_popsize <- pop_size %>%
  select(run_num, step, pop_size, coopall_pennies_frac) %>%
  group_by(step,pop_size) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = factor(pop_size)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", color = "Pop. size") +
  scale_color_manual(values = stef_colors) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.25)) +
  coord_cartesian(ylim = c(0,1)) +
  #guides(colour = "none", linetype = "none") +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
p_popsize 
ggsave("mm_popsize_var.png", units = "in", width = 6, height = 4)

plot_grid(p_mon, p_popsize, labels = "AUTO")
ggsave("supp_monitors_popsize.png", units = "in", width = 8, height = 4)

