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
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = factor(prob_sanction)),
            linewidth = 1, position=position_dodge(width=2)) +
  labs(x= "Time step", y= "Proportion", title = "Cooperators' proportion of total wealth", color = "Sanction\nprobability") +
  scale_color_manual(values = stef_colors) +
  scale_linetype_manual("PGGm", values = c("dotted","solid")) +
  theme_few() +
  #guides(colour = "none") +
  theme(legend.position = c(0.8, 0.2)) +
  geom_hline(yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("despot_wealth_all.png", units = "in", width = 4, height = 4, scale = 1.5)
