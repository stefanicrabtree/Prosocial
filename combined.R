all <- bind_rows(
  filter(MM_, public_goods_game_multiplier == 2 & sanction_fine == 6),
  filter(despot_, prob_sanction == 0.9),
  #filter(copy_leader_, prob_sanction == 0.9),
  #filter(despot_copy, prob_sanction == 0.9),
  filter(inf_sanc_09_, local_sphereinfluence == 5 & local_probinfluence == 0.9)
  )

all %>%
  select(run_num, step, Strategy, coopall_pennies_frac) %>%
  group_by(step, Strategy) %>%
  dplyr::summarise(avg_coop_pennies = mean(coopall_pennies_frac)) %>%
  ggplot() +
  #geom_line(aes(step, coopall_pennies_frac, color = factor(run_num)))
  geom_line(aes(step, avg_coop_pennies, color = Strategy),
            linewidth = 1, position=position_dodge(width=10)) +
  labs(x= "Step", y= "Proportion", title = "Leadership strategies") +
  #scale_color_brewer("Prob sanction", palette = "Set2") +
  scale_color_manual("Strategy", values = stef_colors) +
  #scale_linetype_manual("Sphere of Influence", values = c("dotted","solid")) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2)) +
  #guides(colour = "none") +
  geom_hline( yintercept = .5, size = .5, color = "black", linetype="dashed") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
ggsave("all_wealth_all_nov13.png", units = "in", width = 4, height = 4, scale = 2)
