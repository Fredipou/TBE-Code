### Trying the Bayesian approach on my own
# scaling date_pose.c

date_scale_center <- mean(data_nopupe$date_pose.c, na.rm = TRUE)
date_scale_sd     <- sd(data_nopupe$date_pose.c,   na.rm = TRUE)

data_nopupe$date_pose_sc <- scale(data_nopupe$date_pose.c)[, 1]

# Setting prior

prior_ptoid <- c(
  prior(normal(0, 2.5), class = "b"),         # fixed effects
  prior(normal(0, 2.5), class = "Intercept"), # intercept
  prior(exponential(1), class = "sd")         # random effect SD
)

# Mod 1: same as GAM
bayes_ptoid_1 <- brm(
  pres_ptoid ~ feuillus + date_pose_sc + (1 | parcelle),
  family   = bernoulli(link = "logit"),
  data     = data_nopupe,
  prior    = prior_ptoid,
  chains   = 4,
  iter     = 4000,
  warmup   = 1000,
  cores    = 4,
  seed     = 17,
  file     = "bayes_ptoid_gam"  
)

summary(bayes_ptoid_1)
pp_check(bayes_ptoid_1)

# Trying with the stade and date interaction 

#bayes_ptoid_2 <- brm(
#  pres_ptoid ~ feuillus + date_pose_sc * stade + (1 | parcelle),
#  family   = bernoulli(link = "logit"),
#  data     = data_nopupe,
#  prior    = prior_ptoid,
#  chains   = 4,
#  iter     = 4000,
#  warmup   = 1000,
#  cores    = 4,
#  seed     = 123,
#  file     = "bayes_ptoid_cache"  
#)

#summary(bayes_ptoid_2)
#pp_check(bayes_ptoid_2) 

# Predicting ptoid presence/moth survival

# Picking dates

dates_original <- c(141, 148, 164,   # early
                    150, 155, 171,   # peak  
                    157, 162, 177)   # late

dates_scaled <- (dates_original - date_scale_center) / date_scale_sd

emmeans(bayes_ptoid_1, ~ date_pose_sc,
        at   = list(date_pose_sc = dates_scaled,
                    feuillus     = mean(data_nopupe$feuillus)),
        type = "response")

# Label each date with its intended stage and period

date_stage_map <- data.frame(
  date_pose_sc = dates_scaled,
  stade        = c("L4", "L5", "L6",   
                   "L4", "L5", "L6",   
                   "L4", "L5", "L6"),   
  period       = rep(c("Early", "Peak", "Late"), each = 3),
  feuillus     = mean(data_nopupe$feuillus),
  parcelle     = NA
)

# Get posterior draws — only keep the matching stage for each date

stage_draws <- date_stage_map |>
  add_epred_draws(bayes_ptoid_1, re_formula = NA, ndraws = 2000) |>
  group_by(period, stade) |>
  median_qi(.epred, .width = 0.95) |>
  mutate(period = factor(period, levels = c("Early", "Peak", "Late")),
         stade  = factor(stade,  levels = c("L4", "L5", "L6")))

escape_draws <- date_stage_map |>
  add_epred_draws(bayes_ptoid_1, re_formula = NA, ndraws = 2000) |>
  group_by(period, .draw) |>
  summarise(escape = prod(1 - .epred), .groups = "drop") |>
  group_by(period) |>
  median_qi(escape, .width = 0.95) |>
  mutate(period = factor(period, levels = c("Early", "Peak", "Late")))

ggplot(stage_draws,
       aes(x = period, y = .epred, colour = stade, group = stade)) +
  geom_line(linewidth = 0.8, linetype = "dashed", alpha = 0.5) +
  geom_pointrange(aes(ymin = .lower, ymax = .upper),
                  position = position_dodge(width = 0.3),
                  linewidth = 0.8, size = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_colour_manual(values = c(L4 = "#2C7BB6",
                                 L5 = "#F07D28",
                                 L6 = "#1A9641")) +
  labs(x      = "Season period",
       y      = "Probability of parasitoid presence",
       colour = "Larval stage",
       title  = "Parasitism probability by stage and season",
       subtitle = "Median ± 95% posterior credible interval") +
  theme_classic(base_size = 13) +
  theme(legend.position    = "top",
        plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(colour = "grey40", size = 10),
        panel.grid.major.y = element_line(colour = "grey90"))

ggsave("parasitism_by_stage_period.png", width = 7, height = 5, dpi = 300)

# Get the full draws (not just summary)
escape_draws_full <- date_stage_map |>
  add_epred_draws(bayes_ptoid_1, re_formula = NA, ndraws = 2000) |>
  group_by(period, .draw) |>
  summarise(escape = prod(1 - .epred), .groups = "drop") |>
  mutate(period = factor(period, levels = c("Early", "Peak", "Late")))

ggplot(escape_draws_full, aes(x = period, y = escape)) +
  # Individual draws as dots
  stat_dots(quantiles = 100, color = NA,
            fill = "#4B8FA6", alpha = 0.4, dotsize = 0.8) +
  # Median + 95% CI on top
  stat_pointinterval(.width = c(0.50, 0.95),
                     colour = "grey20", linewidth = 1.2,
                     point_size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(x        = "Season period",
       y        = "Cumulative escape probability",
       title    = "Probability of escaping parasitism across all stages",
       subtitle = "100 posterior quantile dots · line = 50% and 95% credible interval") +
  theme_classic(base_size = 13) +
  theme(plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(colour = "grey40", size = 10),
        panel.grid.major.y = element_line(colour = "grey90"))

ggsave("escape_posterior_dots.png", width = 6, height = 5, dpi = 300)

ggplot(escape_draws_full, aes(x = period, y = escape)) +
  stat_halfeye(fill = "#4B8FA6", alpha = 0.75,
               .width = c(0.50, 0.95),
               point_colour = "grey20",
               interval_colour = "grey20",
               linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(x        = "Season period",
       y        = "Cumulative escape probability",
       title    = "Probability of escaping parasitism across all stages",
       subtitle = "Posterior distribution · thick line = 50% CI · thin line = 95% CI") +
  theme_classic(base_size = 13) +
  theme(plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(colour = "grey40", size = 10),
        panel.grid.major.y = element_line(colour = "grey90"))

ggsave("escape_posterior_halfeye.png", width = 6, height = 5, dpi = 300)

### Fitness from these ----

fitness_draws <- date_stage_map |>
  add_epred_draws(bayes_ptoid_1, re_formula = NA, ndraws = 2000) |>
  rename(p_parasitism = .epred) |>
  select(period, stade, .draw, p_parasitism) |>
  # Compute per-stage survival probability (1 - parasitism)
  mutate(stage_survival = 1 - p_parasitism) |>
  # Mean survival per stage across periods (the reference)
  group_by(stade, .draw) |>
  mutate(mean_survival = mean(stage_survival)) |>
  ungroup() |>
  # Relative fitness per stage × period
  mutate(w_rel = stage_survival / mean_survival)

# Summarise

fitness_summary <- fitness_draws |>
  group_by(stade, period) |>
  median_qi(stage_survival, w_rel, .width = 0.95)

fitness_summary

# Plot

ggplot(fitness_summary,
       aes(x = period, y = stade, fill = w_rel)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  # Median w value
  geom_text(aes(label = sprintf("w = %.2f", w_rel)),
            size = 4.5, fontface = "bold", colour = "grey10") +
  # 95% CI below
  geom_text(aes(label = sprintf("[%.2f, %.2f]", w_rel.lower, w_rel.upper)),
            size = 3, colour = "grey30", vjust = 3) +
  scale_fill_gradient2(low      = "#C1392B",   # below average
                       mid      = "white",
                       high     = "#2C7BB6",   # above average
                       midpoint = 1,
                       limits   = c(0.3, 1.6),
                       name     = "Relative\nfitness (w)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x        = "Season period",
       y        = "Larval stage",
       title    = "Relative fitness by larval stage and phenology",
       subtitle = "w > 1 = above average survival · w < 1 = below average\nMedian with 95% posterior credible interval") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(colour = "grey40", size = 10),
        axis.line       = element_blank(),
        axis.ticks      = element_blank())

ggsave("relative_fitness_heatmap.png", width = 7, height = 5, dpi = 300)

## Adding feuillus into the mix

# Define low and high feuillus values (e.g. 10th and 90th percentiles)
feuillus_low  <- quantile(data_nopupe$feuillus, 0.25)
feuillus_high <- quantile(data_nopupe$feuillus, 0.75)

# Build prediction grid for both forest types
date_stage_feuillus <- bind_rows(
  date_stage_map |> mutate(feuillus = feuillus_low,  forest = "Conifer-dominated"),
  date_stage_map |> mutate(feuillus = feuillus_high, forest = "Deciduous-dominated")
)

# Posterior draws
fitness_feuillus <- date_stage_feuillus |>
  add_epred_draws(bayes_ptoid_1, re_formula = NA, ndraws = 2000) |>
  rename(p_parasitism = .epred) |>
  ungroup() |>
  select(forest, period, stade, .draw, p_parasitism) |>
  mutate(stage_survival = 1 - p_parasitism) |>
  group_by(forest, stade, .draw) |>
  mutate(mean_survival = mean(stage_survival)) |>
  ungroup() |>
  mutate(w_rel = stage_survival / mean_survival)

# Summarise
fitness_feuillus_summary <- fitness_feuillus |>
  group_by(forest, stade, period) |>
  median_qi(stage_survival, w_rel, .width = 0.95)

fitness_feuillus_summary

ggplot(fitness_feuillus_summary,
       aes(x = period, y = stade, fill = w_rel)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("w = %.2f", w_rel)),
            size = 4, fontface = "bold", colour = "grey10") +
  geom_text(aes(label = sprintf("[%.2f, %.2f]", w_rel.lower, w_rel.upper)),
            size = 2.8, colour = "grey30", vjust = 3) +
  scale_fill_gradient2(low      = "#C1392B",
                       mid      = "white",
                       high     = "#2C7BB6",
                       midpoint = 1,
                       limits   = c(0.3, 1.6),
                       name     = "Relative\nfitness (w)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ forest) +
  labs(x        = "Season period",
       y        = "Larval stage",
       title    = "Relative fitness by forest type",
       subtitle = "Feuillus at 10th vs 90th percentile · w > 1 = above average · w < 1 = below average") +
  theme_classic(base_size = 13) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(colour = "grey40", size = 10),
        axis.line       = element_blank(),
        axis.ticks      = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = NA),
        strip.text       = element_text(face = "bold"))

ggsave("relative_fitness_by_forest.png", width = 11, height = 5, dpi = 300)

###
# Continuous grid: feuillus × date_pose_sc, one plot per stage

feuillus_seq <- seq(min(data_nopupe$feuillus, na.rm = TRUE),
                    max(data_nopupe$feuillus, na.rm = TRUE),
                    length.out = 40)

dates_seq_scaled <- seq(min(dates_scaled), max(dates_scaled), length.out = 40)

# All combinations per stage

landscape_grid <- expand.grid(
  date_pose_sc = dates_seq_scaled,
  feuillus     = feuillus_seq,
  stade        = unique(data_nopupe$stade),
  parcelle     = NA
) |>
  mutate(
    date_original = date_pose_sc * date_scale_sd + date_scale_center
  )

# Posterior median predictions (use ndraws = 500 for speed on a large grid)
landscape_pred <- landscape_grid |>
  add_epred_draws(bayes_ptoid_1, re_formula = NA, ndraws = 500) |>
  group_by(date_original, feuillus, stade) |>
  summarise(
    p_parasitism   = median(.epred),
    stage_survival = median(1 - .epred),
    .groups = "drop"
  ) |>
  # Relative fitness: divide by mean survival per stage
  group_by(stade) |>
  mutate(w_rel = stage_survival / mean(stage_survival)) |>
  ungroup() |>
  mutate(stade = factor(stade, levels = c("L4", "L5", "L6")))
###

# Build base ggplot for one stage
p_L4 <- landscape_pred |>
  filter(stade == "L4") |>
  ggplot(aes(x = date_original, y = feuillus, fill = w_rel)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient2(low = "#C1392B", mid = "white",
                       high = "#2C7BB6", midpoint = 1) +
  labs(x = "Date", y = "Feuillus", fill = "w") +
  theme_classic()

# Convert to 3D
plot_gg(p_L4,
        width        = 5,
        height       = 5,
        scale        = 300,
        multicore    = TRUE,
        windowsize   = c(1000, 800),
        zoom         = 0.65,
        phi          = 30,
        theta        = 45)

render_snapshot("fitness_landscape_L4_3d.png")
