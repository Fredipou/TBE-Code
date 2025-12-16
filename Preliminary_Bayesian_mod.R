## Trying Bayesian modeling for cumulative survival probability and selection
## First model with all variables
# Explore Marginal effects

mod_bayes <- brm(
  survie_clean ~ pheno.c * stade.c * feuillus +(1|parcelle) + (1|parcelle:stade.c),
  data = data_nopupe,
  family = bernoulli(link = "logit"),
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95),
  file= "mod_bayes"
)
summary(mod_bayes)
posteriors_1 = as_draws_df(mod_bayes)

## Selection models

## multivariate, not sure how to make it work

#data_multi <- data_nopupe %>%
 # select(parcelle, stade.c, pheno.c, survie_clean) %>%
 # tidyr::pivot_wider(names_from = stade.c, values_from = survie_clean,
                  #   names_prefix = "surv_stage_")

## Stage as factor

mod_bayes_1 <- brm(
  survie_clean ~ pheno.c * stade.c + I(pheno.c^2) * stade.c,
  data = data_nopupe,
  family = bernoulli(link = "logit"),
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95),
  file = "Mod_bayes_1"
)

summary(mod_bayes_1)

## Extracting posteriors and credible intervals

posterior <- as_draws_df(mod_bayes_1)
posterior
posterior <- posterior %>%
  mutate(
    beta_L4  = b_pheno.c + `b_pheno.c:stade.c` * 4,
    beta_L5  = b_pheno.c + `b_pheno.c:stade.c` * 5,
    beta_L6  = b_pheno.c + `b_pheno.c:stade.c` * 6,
    gamma_L4 = b_Ipheno.cE2 + `b_stade.c:Ipheno.cE2` * 4,
    gamma_L5 = b_Ipheno.cE2 + `b_stade.c:Ipheno.cE2` * 5,
    gamma_L6 = b_Ipheno.cE2 + `b_stade.c:Ipheno.cE2` * 6
  )

posterior %>%
  summarise(
    across(starts_with("beta_"), 
           list(mean = mean, l95 = ~quantile(.x, 0.025), u95 = ~quantile(.x, 0.975))),
    across(starts_with("gamma_"), 
           list(mean = mean, l95 = ~quantile(.x, 0.025), u95 = ~quantile(.x, 0.975)))
  )
view(posterior)
##Visualisation

posterior_long <- posterior %>%
  select(starts_with("beta_"), starts_with("gamma_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("type", "stage"),
    names_sep = "_",
    values_to = "value"
  )

posterior_summary <- posterior_long %>%
  group_by(type, stage) %>%
  summarise(
    mean = mean(value),
    l95 = quantile(value, 0.025),
    u95 = quantile(value, 0.975),
    .groups = "drop"
  )

ggplot(posterior_summary, aes(x = stage, y = mean, color = type, group = type)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = l95, ymax = u95), width = 0.2) +
  labs(
    x = "Larval Stage",
    y = "Selection Gradient",
    color = "Type"
  ) +
  theme_minimal(base_size = 14)

## Don't like the look of these but still might be useful.

new_data <- expand.grid(
  pheno.c = seq(min(data_nopupe$pheno.c), max(data_nopupe$pheno.c), length.out = 100),
  stade.c = unique(data_nopupe$stade.c)
)

# Posterior predictions on the logit scale

fitted_vals <- fitted(mod_bayes_1, newdata = new_data, re_formula = NA, summary = FALSE)

posterior_pred <- new_data %>%
  mutate(
    mean = apply(fitted_vals, 2, mean),
    l95  = apply(fitted_vals, 2, quantile, 0.025),
    u95  = apply(fitted_vals, 2, quantile, 0.975)
  )

ggplot(posterior_pred, aes(x = pheno.c, y = mean, color = factor(stade.c))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = l95, ymax = u95, fill = factor(stade.c)),
              alpha = 0.2, color = NA) +
  labs(
    x = "Phenology (centered)",
    y = "Predicted survival probability",
    color = "Larval stage",
    fill = "Larval stage"
  ) +
  theme_minimal(base_size = 14)

