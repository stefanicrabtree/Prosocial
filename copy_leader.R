library(ggplot2)
library(dplyr)

copy_leader40 <- read.csv ("Prosocial_April_30_2023_ps_04_copy_defector-table.csv", skip=6, header=T)
copy_leader90 <- read.csv ("Prosocial_April_30_2023_ps_09_copy_defector-table.csv", skip=6, header=T)

copy_leader <- bind_rows(copy_leader40,copy_leader90) %>%
  mutate(Strategy = "Copy defector")

copy_leader_ <- copy_leader %>%
  mutate(total_pennies = monitor_pennies + always_defect_pennies + cooperator_pennies + reluctant_cooperator_pennies + reluctant_defector_pennies) %>%
  mutate(coopall_pennies_frac = (monitor_pennies + cooperator_pennies + reluctant_cooperator_pennies) / total_pennies)

copy_leader_ %>%
  select(run_num, step, prob_sanction, coopall_pennies_frac) %>%
  group_by(step, prob_sanction) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = factor(prob_sanction)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", title = "Copy defector") +
  scale_color_manual("Sanction\nprobability", values = stef_colors) +
  scale_linetype_manual("PGGm", values = c("dotted","solid")) +
  theme_bw() +
  #guides(colour = "none") +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("copy_wealth_all.png", units = "in", width = 6, height = 4)

despot__ <- despot_ %>%
  mutate(Strategy = "Global")
copy_leader__ <- copy_leader_ %>%
  mutate(Strategy = "Copy defector")
despot_copy <- bind_rows(despot__,copy_leader__)

despot_copy %>%
  select(run_num, step, prob_sanction, Strategy, coopall_pennies_frac) %>%
  group_by(step, prob_sanction, Strategy) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = factor(prob_sanction), linetype = factor(Strategy)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Cooperator's proportion of total wealth", title = "Copy defector vs. global leadership") +
  scale_color_manual("Sanction\nprobability", values = stef_colors) +
  scale_linetype_manual("Leadership\nstrategy", values = c("dotted","solid")) +
  theme_bw() +
  #guides(colour = "none") +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("despot+copy_wealth_all.png", units = "in", width = 6, height = 4)
