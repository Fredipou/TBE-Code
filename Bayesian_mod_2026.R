## Trying Bayesien model from the main GAMM ----

source("Data_cleaning.R")

data_nopupe$parcelle <- factor(data_nopupe$parcelle)
data_nopupe_2025$parcelle = factor(data_nopupe_2025$parcelle)
data_nopupe_2026$parcelle = factor(data_nopupe_2026$parcelle)

#First model, without rain and biomass index. ----

#priors
priors <- c(
  prior(normal(0, 2), class = "b"),                 # feuillus, annee_factor2026
  prior(student_t(3, 0, 2.5), class = "Intercept"),  # garder le défaut
  prior(student_t(3, 0, 2.5), class = "sds", lb = 0) # garder le défaut
)
names(data_nopupe)
gamm_ptoid_main_bayes <- brm(
  pres_ptoid ~ feuillus +
    annee +
    s(date_pose.c, by = annee, k = 12) + 
    (1|parcelle), 
  family = bernoulli(link = "logit"),  
  data = data_nopupe,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15) #adapt_delta 0.99 car divergent transitions
)

#summary(gamm_ptoid_main_bayes)
#saveRDS(gamm_ptoid_main_bayes, file = "gamm_ptoid_main_bayes.rds")
gamm_ptoid_main_bayes <- readRDS("gamm_ptoid_main_bayes.rds")

# Effets fixes négatifs et signif de année2026, feuillus. Grande incertitude pour l'année 2026. 
# Effet aléatoire presque signif

plot(gamm_ptoid_main_bayes)
pp_check(gamm_ptoid_main_bayes) #looks good

## Extraire prédictions

#1) Bases de données pour prédictions

# 2025

prediction_data_2025 <- expand.grid(
  date_pose.c = c(141, 148, 164,   # early
                  150, 155, 171,   # peak
                  157, 162, 177)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(141, 148, 164) ~ "early",
      date_pose.c %in% c(150, 155, 171) ~ "peak",
      date_pose.c %in% c(157, 162, 177) ~ "late"
    ),
    stage_date = case_when(
      date_pose.c %in% c(141, 150, 157) ~ "L4",
      date_pose.c %in% c(148, 155, 162) ~ "L5",
      date_pose.c %in% c(164, 171, 177) ~ "L6"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee    = factor("2025", levels = levels(data_nopupe$annee))
  )

# Grille 2026

prediction_data_2026 <- expand.grid(
  date_pose.c = c(147, 156, 168,   # early
                  154, 164, 170,   # peak
                  161, 175, 182)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(147, 156, 168) ~ "early",
      date_pose.c %in% c(154, 164, 170) ~ "peak",
      date_pose.c %in% c(161, 175, 182) ~ "late"
    ),
    stage_date = case_when(
      date_pose.c %in% c(147, 154, 161) ~ "L4",
      date_pose.c %in% c(156, 164, 175) ~ "L5",
      date_pose.c %in% c(168, 170, 182) ~ "L6"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee    = factor("2026", levels = levels(data_nopupe$annee))
  )

#2) utiliser predictions de marginal effetcs 

prediction_data_2025 <- prediction_data_2025 %>%
  mutate(parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))) #à revérifier avec synthaxe lme4

prediction_data_2026 <- prediction_data_2026 %>%
  mutate(parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle)))

# Prédictions 2025

preds_bayes_2025 <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2025,
                                re_formula = NA, type = "response") 
preds_draws_2025 <- get_draws(preds_bayes_2025)

survie_bayes_2025 <- preds_draws_2025 %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  group_by(pheno.c) %>%
  summarise(survie = mean(surv_prod),
            survie_lwr = quantile(surv_prod, 0.025),
            survie_upr = quantile(surv_prod, 0.975), .groups = "drop")

survie_bayes_2025

# Prédictions 2026

preds_bayes_2026 <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2026,
                                re_formula = NA, type = "response")
preds_draws_2026 <- get_draws(preds_bayes_2026)

survie_bayes_2026 <- preds_draws_2026 %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  group_by(pheno.c) %>%
  summarise(survie = mean(surv_prod),
            survie_lwr = quantile(surv_prod, 0.025),
            survie_upr = quantile(surv_prod, 0.975), .groups = "drop")

survie_bayes_2026 

#Cool, avec Bayes la tendance générale est la même pour les deux années
# mais beaucoup plus incertaine en 2026. A réasseyer avec les "vraies" dates de BIOSIM

# Figure log(survie)/pheno.c en spaghetti plot

log_surv_draws_2025 <- preds_draws_2025 %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2025")

log_surv_draws_2026 <- preds_draws_2026 %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2026")

log_surv_draws_all <- bind_rows(log_surv_draws_2025, log_surv_draws_2026)
 
ggplot(log_surv_draws_all, aes(x = pheno.c, y = log_surv, group = drawid)) +
  geom_line(alpha = 0.02, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black", size = 2) +
  facet_wrap(~ annee) +
  labs(
    x = "Scénario phénologique",
    y = "log(survie)",
    title = "Survie face au parasitisme selon la phénologie"
  ) +
  theme_bw()

# Dates intermédiaires pour courbes plus smooth

# Fonction d'interpolation linéaire entre les 3 scénarios connus, par stade

interpolate_dates <- function(pheno_anchors, date_anchors, pheno_seq) {
  approx(x = pheno_anchors, y = date_anchors, xout = pheno_seq)$y
}

pheno_seq <- seq(-1, 1, by = 0.25)  # 9 points : -1, -0.75, ..., 0.75, 1

# 2025 : dates ancrées par stade (L4, L5, L6) aux 3 scénarios (-1, 0, 1)
dates_2025 <- list(
  L4 = c(early = 141, peak = 150, late = 157),
  L5 = c(early = 148, peak = 155, late = 162),
  L6 = c(early = 164, peak = 171, late = 177)
)

prediction_data_2025_interp <- purrr::map_dfr(names(dates_2025), function(stage) {
  data.frame(
    stage_date = stage,
    pheno_num = pheno_seq,
    date_pose.c = interpolate_dates(c(-1, 0, 1), dates_2025[[stage]], pheno_seq)
  )
}) %>%
  mutate(
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee = factor("2025", levels = levels(data_nopupe$annee)),
    parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
  )

# 2026 : même logique
dates_2026 <- list(
  L4 = c(early = 147, peak = 154, late = 161),
  L5 = c(early = 156, peak = 164, late = 175),
  L6 = c(early = 168, peak = 170, late = 182)
)

prediction_data_2026_interp <- purrr::map_dfr(names(dates_2026), function(stage) {
  data.frame(
    stage_date = stage,
    pheno_num = pheno_seq,
    date_pose.c = interpolate_dates(c(-1, 0, 1), dates_2026[[stage]], pheno_seq)
  )
}) %>%
  mutate(
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee = factor("2026", levels = levels(data_nopupe$annee)),
    parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
  )

preds_bayes_2025_interp <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2025_interp,
                                       re_formula = NA, type = "response")
preds_draws_2025_interp <- get_draws(preds_bayes_2025_interp)

log_surv_2025_interp <- preds_draws_2025_interp %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno_num, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2025")

preds_bayes_2026_interp <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2026_interp,
                                       re_formula = NA, type = "response")
preds_draws_2026_interp <- get_draws(preds_bayes_2026_interp)

log_surv_2026_interp <- preds_draws_2026_interp %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno_num, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2026")

log_surv_all_interp <- bind_rows(log_surv_2025_interp, log_surv_2026_interp)
surv_all_interp <- log_surv_all_interp %>%
  mutate(surv_prod = exp(log_surv))

set.seed(123)
sample_draws <- sample(unique(log_surv_all_interp$drawid), 300)

log_surv_subset <- log_surv_all_interp %>%
  filter(drawid %in% sample_draws)

ggplot(log_surv_subset, aes(x = pheno_num, y = log_surv, group = drawid, color = annee)) +
  geom_line(alpha = 0.1) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +
  scale_color_viridis_d(end = 0.8, guide = "none") +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("Early", "Peak", "Late")) +
  facet_wrap(~ annee) +
  labs(x = "Scénario phénologique", y = "log(survie)") +
  theme_cowplot() +
  panel_border()

## Mod linéaire pour calculer sélection directionnel 2025

selection_gradients_2025 <- preds_draws_2025 %>%
  mutate(
    surv = 1 - draw,
    pheno_num = case_when(
      pheno.c == "early" ~ -1,
      pheno.c == "peak"  ~ 0, # reconvertir en num car pred_draws est en catégorie
      pheno.c == "late"  ~ 1
    )
  ) %>%
  group_by(drawid, pheno_num) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%  # groupe par pheno et fait le produit des 3
  mutate(log_surv = log(surv_prod)) %>% #change en log
  group_by(drawid) %>%
  summarise(
    beta = coef(lm(log_surv ~ pheno_num))[2],  # Reg linéaire pour chaque trio + pente  
    .groups = "drop"
  )

beta_summary_2025 <- selection_gradients_2025 %>%
  summarise(
    beta_mean = mean(beta),
    beta_lwr  = quantile(beta, 0.025), #moy des pentes et I.C
    beta_upr  = quantile(beta, 0.975)
  )

beta_summary_2025 # = gradient directionnel 

# Intervalle exclut 0 donc sélection directionnelle significative pour phénologie hâtive!

selection_gradients_quad_2025 <- preds_draws_2025 %>%
  mutate(
    surv = 1 - draw,
    pheno_num = case_when(
      pheno.c == "early" ~ -1, 
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 1
    )
  ) %>%
  group_by(drawid, pheno_num) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>% 
  mutate(log_surv = log(surv_prod)) %>% 
  group_by(drawid) %>%
  summarise(
    gamma_raw = coef(lm(log_surv ~ pheno_num + I(pheno_num^2)))[3], 
    gamma = 2 * gamma_raw,   # correction Stinchcombe et al. 2008 pour quadratique
    .groups = "drop"
  )

gamma_summary_2025 <- selection_gradients_quad_2025 %>%
  summarise(
    gamma_mean = mean(gamma),
    gamma_lwr  = quantile(gamma, 0.025),
    gamma_upr  = quantile(gamma, 0.975)
  )

gamma_summary_2025

# Intervalle inclut 0 donc pas de preuve de sélection stabilisante mais légèrement négatif 
# = tendance vers sélection stabilisante

# Mod linéaire pour sélection 2026

selection_gradients_2026 <- preds_draws_2026 %>%
  mutate(
    surv = 1 - draw,
    pheno_num = case_when(
      pheno.c == "early" ~ -1,
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 1
    )
  ) %>%
  group_by(drawid, pheno_num) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod)) %>%
  group_by(drawid) %>%
  summarise(
    beta = coef(lm(log_surv ~ pheno_num))[2],
    .groups = "drop"
  )

beta_summary_2026 <- selection_gradients_2026 %>%
  summarise(
    beta_mean = mean(beta),
    beta_lwr  = quantile(beta, 0.025),
    beta_upr  = quantile(beta, 0.975)
  )

beta_summary_2026 # = gradient directionnel 

#Ici l'intervalle inclut 0 mais de très proche! Donc même tendance qu.en 2025 mais pas signif
# Ampleur pus faible et incertitude plus grande

selection_gradients_quad_2026 <- preds_draws_2026 %>%
  mutate(
    surv = 1 - draw,
    pheno_num = case_when(
      pheno.c == "early" ~ -1,
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 1 
    )
  ) %>%
  group_by(drawid, pheno_num) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") 
  mutate(log_surv = log(surv_prod)) 
  group_by(drawid) %>%
  summarise(
    gamma_raw = coef(lm(log_surv ~ pheno_num + I(pheno_num^2)))[3],
    gamma = 2 * gamma_raw,  # correction article 2008
    .groups = "drop"
  )

gamma_summary_2026 <- selection_gradients_quad_2026 %>%
  summarise(
    gamma_mean = mean(gamma),
    gamma_lwr  = quantile(gamma, 0.025),
    gamma_upr  = quantile(gamma, 0.975)
  )

gamma_summary_2026

# Intervalle inclut 0 aussi et très large, signe positif mais trop incertain pour en conclure
# quelque chose?

### Essai de quelques visualisations ----
#1

gradient_summary <- bind_rows(
  beta_summary_2025 %>% mutate(annee = "2025", gradient = "β (directionnel)", 
                               mean = beta_mean, lwr = beta_lwr, upr = beta_upr),
  beta_summary_2026 %>% mutate(annee = "2026", gradient = "β (directionnel)", 
                               mean = beta_mean, lwr = beta_lwr, upr = beta_upr),
  gamma_summary_2025 %>% mutate(annee = "2025", gradient = "γ (quadratique)", 
                                mean = gamma_mean, lwr = gamma_lwr, upr = gamma_upr),
  gamma_summary_2026 %>% mutate(annee = "2026", gradient = "γ (quadratique)", 
                                mean = gamma_mean, lwr = gamma_lwr, upr = gamma_upr)
) %>%
  select(annee, gradient, mean, lwr, upr)

ggplot(gradient_summary, aes(x = mean, y = interaction(gradient, annee), color = annee)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), linewidth = 0.8, size = 0.8) +
  scale_color_manual(values = c("2025" = "#2C5F7C", "2026" = "#D97B3F")) +
  labs(x = "Estimé du gradient de sélection", y = NULL, color = "Année") +
  theme_cowplot()

#2 ggplot spagh  ----

fit_lines <- function(preds_draws, annee_label) {
  preds_draws %>%
    mutate(
      surv = 1 - draw,
      pheno_num = case_when(
        pheno.c == "early" ~ -1,
        pheno.c == "peak"  ~ 0,
        pheno.c == "late"  ~ 1
      )
    ) %>%
    group_by(drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod)) %>%
    group_by(drawid) %>%
    summarise(
      intercept = coef(lm(log_surv ~ pheno_num))[1],
      beta      = coef(lm(log_surv ~ pheno_num))[2],
      .groups = "drop"
    ) %>%
    mutate(annee = annee_label)
}

lines_2025 <- fit_lines(preds_draws_2025, "2025")
lines_2026 <- fit_lines(preds_draws_2026, "2026")

lines_all <- bind_rows(lines_2025, lines_2026)

set.seed(123)
n_lines_to_show <- 300  # ajuste selon la densité visuelle souhaitée

sampled_lines <- lines_all %>%
  group_by(annee) %>%
  slice_sample(n = n_lines_to_show) %>%
  ungroup()

pheno_grid <- seq(-1, 1, by = 0.1)

lines_expanded <- sampled_lines %>%
  rowwise() %>%
  mutate(pheno_num = list(pheno_grid)) %>%
  unnest(pheno_num) %>%
  mutate(fitted = intercept + beta * pheno_num)

mean_lines <- lines_all %>%
  group_by(annee) %>%
  summarise(intercept_mean = mean(intercept), beta_mean = mean(beta), .groups = "drop") %>%
  rowwise() %>%
  mutate(pheno_num = list(pheno_grid)) %>%
  unnest(pheno_num) %>%
  mutate(fitted = intercept_mean + beta_mean * pheno_num)

ggplot() +
  geom_line(data = lines_expanded, aes(x = pheno_num, y = fitted, group = drawid), 
            alpha = 0.08, color = "steelblue") +
  geom_line(data = mean_lines, aes(x = pheno_num, y = fitted), 
            color = "black", linewidth = 1.2) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("Early", "Peak", "Late")) +
  facet_wrap(~ annee) +
  labs(
    x = "Scénario phénologique",
    y = "log(survie relative) ajustée",
    title = "Gradient de sélection directionnel : pentes individuelles vs moyenne"
  ) +
  theme_cowplot() +
  panel_border()

## Prochaine étape
## Eassyer avec les années séparés en brms pour isoler l'effet annuelle sur l'environnement, prendre les extrèmes
# Essayer model avec interaction feuillus date.pose.c
## Essayer avec pluie dans autre modèle
## Essayer avec les vrais dates BIOSIM!!
## Changer pheno.c de -1 à -2 et 2 ou essayer (ou -1.96/1.96) dans lm gradient de sélection
## Pour avoir 84 parcelles utiliser re_formula = NULL, avec une valeur propre pour chaque parcelle

## Mod Bayesian pour chaque année ----

ptoid_main_2025 <- brm(
  pres_ptoid ~ feuillus +
    s(date_pose.c, k = 6) + 
    (1|parcelle),
  family = bernoulli(link = "logit"),  
  data = data_nopupe_2025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15) #adapt_delta 0.99 car divergent transitions
)
#summary(ptoid_main_2025)
#saveRDS(ptoid_main_2025, file = "ptoid_main_2025.rds")
ptoid_main_2025 <- readRDS("ptoid_main_2025.rds")
#plot(ptoid_main_2025)
#pp_check(ptoid_main_2025)

ptoid_main_2026 <- brm(
  pres_ptoid ~ feuillus +
    s(date_pose.c, k = 6) + 
    (1|parcelle),
  family = bernoulli(link = "logit"),  
  data = data_nopupe_2026,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15) #adapt_delta 0.99 car divergent transitions
)

#summary(ptoid_main_2026)
#saveRDS(ptoid_main_2026, file = "ptoid_main_2026.rds")
ptoid_main_2026 <- readRDS("ptoid_main_2026.rds")
#plot(ptoid_main_2026)
#pp_check(ptoid_main_2026)

# Dernier Modèle avec intéraction année x feuillus ----

ptoid_main_interaction <- brm(
  pres_ptoid ~ feuillus * annee +
    s(date_pose.c, by = annee, k = 6) +
    (1 | parcelle),
  family = bernoulli(link = "logit"),
  data = data_nopupe,
  chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

#summary(ptoid_main_interaction)
saveRDS(ptoid_main_interaction, file = "ptoid_main_interaction.rds")
#ptoid_main_interaction <- readRDS("ptoid_main_interaction.rds")
#plot(ptoid_main_interaction)
#pp_check(ptoid_main_interaction)


#semble qu'il y ait une différence de l'effet de feuillus entre les années mais de peu.

## Essayer de recalculer les pressions de sélections avec les modèles séparés ----

# ---- Valeurs de feuillus, séparées par année (les parcelles diffèrent un peu) ----

feuillus_values_2025 <- data_nopupe_2025 %>%
  distinct(parcelle, feuillus) %>%
  pull(feuillus) %>%
  unique() %>%
  sort()

feuillus_values_2026 <- data_nopupe_2026 %>%
  distinct(parcelle, feuillus) %>%
  pull(feuillus) %>%
  unique() %>%
  sort()

length(feuillus_values_2025)
length(feuillus_values_2026)

# ---- Grilles de dates par année (sans "annee", puisque chaque modèle est déjà spécifique) ----

grille_dates_2025 <- expand.grid(
  date_pose.c = c(141, 148, 164,   # early
                  150, 155, 171,   # peak
                  157, 162, 177)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(141, 148, 164) ~ "early",
      date_pose.c %in% c(150, 155, 171) ~ "peak",
      date_pose.c %in% c(157, 162, 177) ~ "late"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    parcelle = factor(levels(data_nopupe_2025$parcelle)[1], levels = levels(data_nopupe_2025$parcelle))
  )

grille_dates_2026 <- expand.grid(
  date_pose.c = c(147, 156, 168,   # early
                  154, 164, 170,   # peak
                  161, 175, 182)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(147, 156, 168) ~ "early",
      date_pose.c %in% c(154, 164, 170) ~ "peak",
      date_pose.c %in% c(161, 175, 182) ~ "late"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    parcelle = factor(levels(data_nopupe_2026$parcelle)[1], levels = levels(data_nopupe_2026$parcelle))
  )

# ---- Fonction : beta (gradient linéaire) pour chaque valeur de feuillus, un modèle donné ----
compute_beta_par_feuillus <- function(grille_dates, feuillus_values, modele) {
  
  prediction_data <- feuillus_values %>%
    map_df(function(f) grille_dates %>% mutate(feuillus = f))
  
  preds_bayes <- predictions(modele, newdata = prediction_data,
                             re_formula = NA, type = "response")
  preds_draws <- get_draws(preds_bayes)
  
  preds_draws %>%
    mutate(
      surv = 1 - draw,
      pheno_num = case_when(
        pheno.c == "early" ~ -1,
        pheno.c == "peak"  ~ 0,
        pheno.c == "late"  ~ 1
      )
    ) %>%
    group_by(feuillus, drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod)) %>%
    group_by(feuillus, drawid) %>%
    summarise(beta = coef(lm(log_surv ~ pheno_num))[2], .groups = "drop") %>%
    group_by(feuillus) %>%
    summarise(
      beta_mean = mean(beta),
      beta_lwr  = quantile(beta, 0.025),
      beta_upr  = quantile(beta, 0.975),
      .groups = "drop"
    )
}

# ---- Application aux deux modèles séparés ----

beta_par_feuillus_2025 <- compute_beta_par_feuillus(grille_dates_2025, feuillus_values_2025, ptoid_main_2025)
beta_par_feuillus_2026 <- compute_beta_par_feuillus(grille_dates_2026, feuillus_values_2026, ptoid_main_2026)

print(beta_par_feuillus_2025)
print(beta_par_feuillus_2026)

## Visu --

beta_combined <- bind_rows(
  beta_par_feuillus_2025 %>% mutate(annee = "2025"),
  beta_par_feuillus_2026 %>% mutate(annee = "2026")
)

ggplot(beta_combined, aes(x = feuillus, y = beta_mean, color = annee, fill = annee)) +
  geom_ribbon(aes(ymin = beta_lwr, ymax = beta_upr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "% Feuillus",
    y = "Gradient de sélection (beta) sur la phénologie",
    color = "Année", fill = "Année",
    title = "Variation du gradient de sélection phénologique selon le couvert feuillu, par année"
  ) +
  theme_minimal()

## Refaire ces analyses avec les vrais dates de BIOSIM pour 2025 et 2026 ----

# 1) Date sur le terrain

 # 2025
#(141, 150, 157) ~ "L4"
#(148, 155, 162) ~ "L5"
#(164, 171, 177) ~ "L6"

 # 2026
#(147, 154, 161) ~ "L4"
#(156, 164, 175) ~ "L5"
#(168, 170, 182) ~ "L6"


# 2) Date BIOSIM (après saison)

 # 2025
#(147, 153, 160) ~ "L4"
#(153, 159, 168) ~ "L5"
#(162, 171, 182) ~ "L6"

 # 2026
#(145, 155, 166) ~ "L4"
#(151, 162, 174) ~ "L5"
#(163, 176, 190) ~ "L6"

## ---- Dates BIOSIM (par stade, ordre early/peak/late)

dates_2025_biosim <- list(
  L4 = c(early = 147, peak = 153, late = 160),
  L5 = c(early = 153, peak = 159, late = 168),
  L6 = c(early = 162, peak = 171, late = 182)
)

dates_2026_biosim <- list(
  L4 = c(early = 145, peak = 155, late = 166),
  L5 = c(early = 151, peak = 162, late = 174),
  L6 = c(early = 163, peak = 176, late = 190)
)

# Grille data

grille_biosim <- function(dates_list, annee_label) {
  purrr::map_dfr(names(dates_list), function(stage) {
    data.frame(
      stage_date = stage,
      pheno.c = names(dates_list[[stage]]),
      date_pose.c = unname(dates_list[[stage]])
    )
  }) %>%
    mutate(
      pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
      feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
      annee    = factor(annee_label, levels = levels(data_nopupe$annee)),
      parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
    )
}

prediction_data_2025_biosim <- grille_biosim(dates_2025_biosim, "2025")
prediction_data_2026_biosim <- grille_biosim(dates_2026_biosim, "2026")

# Prédiction et survie cumulative

preds_bayes_2025_biosim <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2025_biosim,
                                       re_formula = NA, type = "response")
preds_draws_2025_biosim <- get_draws(preds_bayes_2025_biosim)

survie_bayes_2025_biosim <- preds_draws_2025_biosim %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  group_by(pheno.c) %>%
  summarise(survie = mean(surv_prod),
            survie_lwr = quantile(surv_prod, 0.025),
            survie_upr = quantile(surv_prod, 0.975), .groups = "drop")

survie_bayes_2025_biosim

preds_bayes_2026_biosim <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2026_biosim,
                                       re_formula = NA, type = "response")
preds_draws_2026_biosim <- get_draws(preds_bayes_2026_biosim)

survie_bayes_2026_biosim <- preds_draws_2026_biosim %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  group_by(pheno.c) %>%
  summarise(survie = mean(surv_prod),
            survie_lwr = quantile(surv_prod, 0.025),
            survie_upr = quantile(surv_prod, 0.975), .groups = "drop")

survie_bayes_2026_biosim

calculer_gradients <- function(preds_draws) {
  base <- preds_draws %>%
    mutate(
      surv = 1 - draw,
      pheno_num = case_when(
        pheno.c == "early" ~ -1,
        pheno.c == "peak"  ~ 0,
        pheno.c == "late"  ~ 1
      )
    ) %>%
    group_by(drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod))
  
  beta_df <- base %>%
    group_by(drawid) %>%
    summarise(beta = coef(lm(log_surv ~ pheno_num))[2], .groups = "drop")
  
  gamma_df <- base %>%
    group_by(drawid) %>%
    summarise(
      gamma_raw = coef(lm(log_surv ~ pheno_num + I(pheno_num^2)))[3],
      gamma = 2 * gamma_raw,
      .groups = "drop"
    )
  
  list(beta = beta_df, gamma = gamma_df)
}

gradients_2025_biosim <- calculer_gradients(preds_draws_2025_biosim)
gradients_2026_biosim <- calculer_gradients(preds_draws_2026_biosim)

beta_summary_2025_biosim <- gradients_2025_biosim$beta %>%
  summarise(beta_mean = mean(beta), beta_lwr = quantile(beta, 0.025), beta_upr = quantile(beta, 0.975))
beta_summary_2025_biosim

beta_summary_2026_biosim <- gradients_2026_biosim$beta %>%
  summarise(beta_mean = mean(beta), beta_lwr = quantile(beta, 0.025), beta_upr = quantile(beta, 0.975))
beta_summary_2026_biosim

gamma_summary_2025_biosim <- gradients_2025_biosim$gamma %>%
  summarise(gamma_mean = mean(gamma), gamma_lwr = quantile(gamma, 0.025), gamma_upr = quantile(gamma, 0.975))
gamma_summary_2025_biosim

gamma_summary_2026_biosim <- gradients_2026_biosim$gamma %>%
  summarise(gamma_mean = mean(gamma), gamma_lwr = quantile(gamma, 0.025), gamma_upr = quantile(gamma, 0.975))
gamma_summary_2026_biosim

## Figures avec dates BIOSIM

#1

log_surv_draws_2025_biosim <- preds_draws_2025_biosim %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2025")

log_surv_draws_2026_biosim <- preds_draws_2026_biosim %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno.c, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2026")

log_surv_draws_all_biosim <- bind_rows(log_surv_draws_2025_biosim, log_surv_draws_2026_biosim)

ggplot(log_surv_draws_all_biosim, aes(x = pheno.c, y = log_surv, group = drawid)) +
  geom_line(alpha = 0.02, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black", size = 2) +
  facet_wrap(~ annee) +
  labs(
    x = "Scénario phénologique",
    y = "log(survie)",
    title = "Survie face au parasitisme selon la phénologie (dates BIOSIM)"
  ) +
  theme_bw()

# 2

interpolate_dates <- function(pheno_anchors, date_anchors, pheno_seq) {
  approx(x = pheno_anchors, y = date_anchors, xout = pheno_seq)$y
}

pheno_seq <- seq(-1, 1, by = 0.25)

prediction_data_2025_interp_biosim <- purrr::map_dfr(names(dates_2025_biosim), function(stage) {
  data.frame(
    stage_date = stage,
    pheno_num = pheno_seq,
    date_pose.c = interpolate_dates(c(-1, 0, 1), dates_2025_biosim[[stage]], pheno_seq)
  )
}) %>%
  mutate(
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee = factor("2025", levels = levels(data_nopupe$annee)),
    parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
  )

prediction_data_2026_interp_biosim <- purrr::map_dfr(names(dates_2026_biosim), function(stage) {
  data.frame(
    stage_date = stage,
    pheno_num = pheno_seq,
    date_pose.c = interpolate_dates(c(-1, 0, 1), dates_2026_biosim[[stage]], pheno_seq)
  )
}) %>%
  mutate(
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee = factor("2026", levels = levels(data_nopupe$annee)),
    parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
  )

preds_bayes_2025_interp_biosim <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2025_interp_biosim,
                                              re_formula = NA, type = "response")
preds_draws_2025_interp_biosim <- get_draws(preds_bayes_2025_interp_biosim)

log_surv_2025_interp_biosim <- preds_draws_2025_interp_biosim %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno_num, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2025")

preds_bayes_2026_interp_biosim <- predictions(gamm_ptoid_main_bayes, newdata = prediction_data_2026_interp_biosim,
                                              re_formula = NA, type = "response")
preds_draws_2026_interp_biosim <- get_draws(preds_bayes_2026_interp_biosim)

log_surv_2026_interp_biosim <- preds_draws_2026_interp_biosim %>%
  mutate(surv = 1 - draw) %>%
  group_by(pheno_num, drawid) %>%
  summarise(surv_prod = prod(surv), .groups = "drop") %>%
  mutate(log_surv = log(surv_prod), annee = "2026")

log_surv_all_interp_biosim <- bind_rows(log_surv_2025_interp_biosim, log_surv_2026_interp_biosim)

set.seed(123)
sample_draws_biosim <- sample(unique(log_surv_all_interp_biosim$drawid), 300)

log_surv_subset_biosim <- log_surv_all_interp_biosim %>%
  filter(drawid %in% sample_draws_biosim)

ggplot(log_surv_subset_biosim, aes(x = pheno_num, y = log_surv, group = drawid, color = annee)) +
  geom_line(alpha = 0.1) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +
  scale_color_viridis_d(end = 0.8, guide = "none") +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("Early", "Peak", "Late")) +
  facet_wrap(~ annee) +
  labs(x = "Scénario phénologique", y = "log(survie)", title = "Dates BIOSIM") +
  theme_cowplot() +
  panel_border()

# 3

gradient_summary_biosim <- bind_rows(
  beta_summary_2025_biosim %>% mutate(annee = "2025", gradient = "β (directionnel)", 
                                      mean = beta_mean, lwr = beta_lwr, upr = beta_upr),
  beta_summary_2026_biosim %>% mutate(annee = "2026", gradient = "β (directionnel)", 
                                      mean = beta_mean, lwr = beta_lwr, upr = beta_upr),
  gamma_summary_2025_biosim %>% mutate(annee = "2025", gradient = "γ (quadratique)", 
                                       mean = gamma_mean, lwr = gamma_lwr, upr = gamma_upr),
  gamma_summary_2026_biosim %>% mutate(annee = "2026", gradient = "γ (quadratique)", 
                                       mean = gamma_mean, lwr = gamma_lwr, upr = gamma_upr)
) %>%
  select(annee, gradient, mean, lwr, upr)

ggplot(gradient_summary_biosim, aes(x = mean, y = interaction(gradient, annee), color = annee)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), linewidth = 0.8, size = 0.8) +
  scale_color_manual(values = c("2025" = "#2C5F7C", "2026" = "#D97B3F")) +
  labs(x = "Estimé du gradient de sélection", y = NULL, color = "Année",
       title = "Gradients de sélection (dates BIOSIM)") +
  theme_cowplot()

#4

fit_lines <- function(preds_draws, annee_label) {
  preds_draws %>%
    mutate(
      surv = 1 - draw,
      pheno_num = case_when(
        pheno.c == "early" ~ -1,
        pheno.c == "peak"  ~ 0,
        pheno.c == "late"  ~ 1
      )
    ) %>%
    group_by(drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod)) %>%
    group_by(drawid) %>%
    summarise(
      intercept = coef(lm(log_surv ~ pheno_num))[1],
      beta      = coef(lm(log_surv ~ pheno_num))[2],
      .groups = "drop"
    ) %>%
    mutate(annee = annee_label)
}

lines_2025_biosim <- fit_lines(preds_draws_2025_biosim, "2025")
lines_2026_biosim <- fit_lines(preds_draws_2026_biosim, "2026")

lines_all_biosim <- bind_rows(lines_2025_biosim, lines_2026_biosim)

set.seed(123)
sampled_lines_biosim <- lines_all_biosim %>%
  group_by(annee) %>%
  slice_sample(n = 300) %>%
  ungroup()

pheno_grid <- seq(-1, 1, by = 0.1)

lines_expanded_biosim <- sampled_lines_biosim %>%
  rowwise() %>%
  mutate(pheno_num = list(pheno_grid)) %>%
  unnest(pheno_num) %>%
  mutate(fitted = intercept + beta * pheno_num)

mean_lines_biosim <- lines_all_biosim %>%
  group_by(annee) %>%
  summarise(intercept_mean = mean(intercept), beta_mean = mean(beta), .groups = "drop") %>%
  rowwise() %>%
  mutate(pheno_num = list(pheno_grid)) %>%
  unnest(pheno_num) %>%
  mutate(fitted = intercept_mean + beta_mean * pheno_num)

ggplot() +
  geom_line(data = lines_expanded_biosim, aes(x = pheno_num, y = fitted, group = drawid), 
            alpha = 0.08, color = "steelblue") +
  geom_line(data = mean_lines_biosim, aes(x = pheno_num, y = fitted), 
            color = "black", linewidth = 1.2) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c("Early", "Peak", "Late")) +
  facet_wrap(~ annee) +
  labs(
    x = "Scénario phénologique",
    y = "log(survie relative) ajustée",
    title = "Gradient de sélection directionnel (dates BIOSIM)"
  ) +
  theme_cowplot() +
  panel_border()

#5
ptoid_main_2025 <- readRDS("ptoid_main_2025.rds")
ptoid_main_2026 <- readRDS("ptoid_main_2026.rds")
  
  feuillus_values_2025 <- data_nopupe_2025 %>%
  distinct(parcelle, feuillus) %>%
  pull(feuillus) %>%
  unique() %>%
  sort()

feuillus_values_2026 <- data_nopupe_2026 %>%
  distinct(parcelle, feuillus) %>%
  pull(feuillus) %>%
  unique() %>%
  sort()

length(feuillus_values_2025)
length(feuillus_values_2026)

# ---- Grilles de dates BIOSIM par année ----

grille_dates_2025_biosim <- expand.grid(
  date_pose.c = c(147, 153, 162,   # early
                  153, 159, 171,   # peak
                  160, 168, 182)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(147, 153, 162) ~ "early",
      date_pose.c %in% c(153, 159, 171) ~ "peak",
      date_pose.c %in% c(160, 168, 182) ~ "late"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    parcelle = factor(levels(data_nopupe_2025$parcelle)[1], levels = levels(data_nopupe_2025$parcelle))
  )

grille_dates_2026_biosim <- expand.grid(
  date_pose.c = c(145, 151, 163,   # early
                  155, 162, 176,   # peak
                  166, 174, 190)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(145, 151, 163) ~ "early",
      date_pose.c %in% c(155, 162, 176) ~ "peak",
      date_pose.c %in% c(166, 174, 190) ~ "late"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    parcelle = factor(levels(data_nopupe_2026$parcelle)[1], levels = levels(data_nopupe_2026$parcelle))
  )

# ---- Fonction (inchangée) ----

compute_beta_par_feuillus <- function(grille_dates, feuillus_values, modele) {
  
  prediction_data <- feuillus_values %>%
    map_df(function(f) grille_dates %>% mutate(feuillus = f))
  
  preds_bayes <- predictions(modele, newdata = prediction_data,
                             re_formula = NA, type = "response")
  preds_draws <- get_draws(preds_bayes)
  
  preds_draws %>%
    mutate(
      surv = 1 - draw,
      pheno_num = case_when(
        pheno.c == "early" ~ -1,
        pheno.c == "peak"  ~ 0,
        pheno.c == "late"  ~ 1
      )
    ) %>%
    group_by(feuillus, drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod)) %>%
    group_by(feuillus, drawid) %>%
    summarise(beta = coef(lm(log_surv ~ pheno_num))[2], .groups = "drop") %>%
    group_by(feuillus) %>%
    summarise(
      beta_mean = mean(beta),
      beta_lwr  = quantile(beta, 0.025),
      beta_upr  = quantile(beta, 0.975),
      .groups = "drop"
    )
}

# ---- Application aux deux modèles séparés, avec dates BIOSIM ----

beta_par_feuillus_2025_biosim <- compute_beta_par_feuillus(grille_dates_2025_biosim, feuillus_values_2025, ptoid_main_2025)
beta_par_feuillus_2026_biosim <- compute_beta_par_feuillus(grille_dates_2026_biosim, feuillus_values_2026, ptoid_main_2026)

print(beta_par_feuillus_2025_biosim)
print(beta_par_feuillus_2026_biosim)

## ---- Visualisation ----

beta_combined_biosim <- bind_rows(
  beta_par_feuillus_2025_biosim %>% mutate(annee = "2025"),
  beta_par_feuillus_2026_biosim %>% mutate(annee = "2026")
)

ggplot(beta_combined_biosim, aes(x = feuillus, y = beta_mean, color = annee, fill = annee)) +
  geom_ribbon(aes(ymin = beta_lwr, ymax = beta_upr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "% Feuillus",
    y = "Gradient de sélection (beta) sur la phénologie",
    color = "Année", fill = "Année",
    title = "Variation du gradient de sélection phénologique selon le couvert feuillu, par année (dates BIOSIM)"
  ) +
  theme_minimal()
