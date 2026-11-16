## Trying Bayesien model from the main GAMM ----

source("Data_cleaning.R")

data_nopupe$parcelle <- factor(data_nopupe$parcelle)
data_nopupe_2025$parcelle = factor(data_nopupe_2025$parcelle)
data_nopupe_2026$parcelle = factor(data_nopupe_2026$parcelle)


# Change la valeur de pheno.c à -2, 0 et 2 pour mieux représenter l'écart avec le trait

data_nopupe <- data_nopupe %>%
  mutate(pheno.c = ifelse(pheno == "E", -2,
                          ifelse(pheno == "P",  0,
                                 ifelse(pheno == "L", 2, NA))))
data_nopupe_2025 <- data_nopupe_2025 %>%
  mutate(pheno.c = ifelse(pheno == "E", -2,
                          ifelse(pheno == "P",  0,
                                 ifelse(pheno == "L", 2, NA))))

data_nopupe_2026 <- data_nopupe_2026 %>%
  mutate(pheno.c = ifelse(pheno == "E", -2,
                          ifelse(pheno == "P",  0,
                                 ifelse(pheno == "L", 2, NA))))

# First model, without rain and biomass index ----

priors <- c(
  prior(normal(0, 2), class = "b"),                 # feuillus, annee_factor2026
  prior(student_t(3, 0, 2.5), class = "Intercept"),  
  prior(student_t(3, 0, 2.5), class = "sds", lb = 0) 
)

#Saved model

gamm_ptoid_main_bayes <- readRDS("gamm_ptoid_main_bayes.rds")

gamm_ptoid_main_bayes <- brm(
  pres_ptoid ~ feuillus +
    annee +
    feuillus+
    s(date_pose.c, by = annee, k = 12) + 
    (1|parcelle), 
  family = bernoulli(link = "logit"),  
  data = data_nopupe,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15), #adapt_delta 0.99 car divergent transitions
)

#summary(gamm_ptoid_main_bayes)
#saveRDS(gamm_ptoid_main_bayes, file = "gamm_ptoid_main_bayes.rds")
#get_prior(gamm_ptoid_main_bayes)
#gamm_ptoid_main_bayes <- readRDS("gamm_ptoid_main_bayes.rds")

# Effets fixes négatifs et signif de année 2026, feuillus. Grande incertitude pour l'année 2026. 
# Effet aléatoire presque signif

#plot(gamm_ptoid_main_bayes)
#pp_check(priors) #looks good
#epred_draws(gamm_ptoid_main_bayes)

# Modèle avec intéraction année x feuillus

ptoid_main_interaction <- readRDS("ptoid_main_interaction.rds")

ptoid_main_interaction <- brm(
  pres_ptoid ~ feuillus * annee +
    s(date_pose.c, by = annee, k = 12) +
    (1 | parcelle),
  family = bernoulli(link = "logit"),
  data = data_nopupe,
  chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
#hypothesis(ptoid_main_interaction, "feuillus:annee2026 > 0")
#summary(ptoid_main_interaction)
#saveRDS(ptoid_main_interaction, file = "ptoid_main_interaction.rds")

#ptoid_main_interaction <- readRDS("ptoid_main_interaction.rds")
#plot(ptoid_main_interaction)
#pp_check(ptoid_main_interaction)
#hypothesis(ptoid_main_interaction, "feuillus:annee2026 > 0")
#Le couvert feuillu réduit le parasitisme en 2025, mais cet effet disparaît en 2026 (−0.02 + 0.02 ≈ 0). 
#La différence entre les années est soutenue. Avec les données complètes 2026, je crois que ce
# modèle devrait être prioriser

## Vérification entre les deux modèles

#ptoid_main_interaction <- add_criterion(ptoid_main_interaction, "loo")
#gamm_ptoid_main_bayes  <- add_criterion(gamm_ptoid_main_bayes, "loo")
#loo_compare(ptoid_main_interaction, gamm_ptoid_main_bayes)

## Modele avec une autre intéraction aléatoire parcelle:annee

ptoid_main_interaction_2 <- brm(
  pres_ptoid ~ feuillus * annee +
    s(date_pose.c, by = annee, k = 12) +
    (1 | parcelle) +
    (1 | parcelle:annee),
  family = bernoulli(link = "logit"),
  data = data_nopupe,
  chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
summary(ptoid_main_interaction_2)
saveRDS(ptoid_main_interaction_2, "ptoid_main_interaction_2.rds")

ptoid_main_interaction   <- add_criterion(ptoid_main_interaction, "loo")
ptoid_main_interaction_2 <- add_criterion(ptoid_main_interaction_2, "loo")
loo_compare(ptoid_main_interaction, ptoid_main_interaction_2)

# Sans premiere interaction et compare les trois
ptoid_main_interaction_3 = readRDS("ptoid_main_interaction_3.RDS")

ptoid_main_interaction_3 <- brm(
  pres_ptoid ~ feuillus * annee +
    s(date_pose.c, by = annee, k = 12) +
    (1 | parcelle:annee),
  family = bernoulli(link = "logit"),
  data = data_nopupe,
  chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
#saveRDS(ptoid_main_interaction_3, "ptoid_main_interaction_3.rds")
#summary(ptoid_main_interaction_3)
#ptoid_main_interaction_3 <- add_criterion(ptoid_main_interaction_3, "loo")
#loo_compare(ptoid_main_interaction, ptoid_main_interaction_2, ptoid_main_interaction_3)
#loo(ptoid_main_interaction_3)



## Survie cumulative 2025 & 2026 ----

#1) Bases de données pour prédictions avec dates TERRAIN

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
                  154, 163, 175,   # peak
                  161, 170, 182)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(147, 156, 168) ~ "early",
      date_pose.c %in% c(154, 163, 175) ~ "peak",
      date_pose.c %in% c(161, 170, 182) ~ "late"
    ),
    stage_date = case_when(
      date_pose.c %in% c(147, 154, 161) ~ "L4",
      date_pose.c %in% c(156, 163, 170) ~ "L5",
      date_pose.c %in% c(168, 175, 182) ~ "L6"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE), #couvert feuillus moyen 33%
    annee    = factor("2026", levels = levels(data_nopupe$annee))
  )


#2) utiliser predictions de marginal effects Parcelle = 33% fixe pour effet au niveau
# population

prediction_data_2025 <- prediction_data_2025 %>%
  mutate(parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))) #à besoin d'une parcelle

prediction_data_2026 <- prediction_data_2026 %>%
  mutate(parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle)))

# Prédictions 2025

preds_bayes_2025 <- predictions(ptoid_main_interaction, newdata = prediction_data_2025,
                                re_formula = NA, type = "response") #on veut chiffre entre 0 et 1
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

preds_bayes_2026 <- predictions(ptoid_main_interaction, newdata = prediction_data_2026,
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

## Vérifications pour 2026

#prediction_data_2026 %>% count(pheno.c)
#prediction_data_2026 %>% count(pheno.c, stage_date)

#Cool, avec Bayes la tendance générale est la même pour les deux années
# mais beaucoup plus incertaine en 2026. A réasseyer avec les "vraies" dates de BIOSIM

## Figure Real data VS Pred ----

range_2025 <- range(data_nopupe_2025$date_pose.c, na.rm = TRUE)
range_2026 <- range(data_nopupe_2026$date_pose.c, na.rm = TRUE)

grille_continue_2025 <- data.frame(
  date_pose.c = seq(range_2025[1], range_2025[2], length.out = 100)
) %>%
  mutate(
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee = factor("2025", levels = levels(data_nopupe$annee)),
    parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
  )

grille_continue_2026 <- data.frame(
  date_pose.c = seq(range_2026[1], range_2026[2], length.out = 100)
) %>%
  mutate(
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee = factor("2026", levels = levels(data_nopupe$annee)),
    parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))
  )

preds_continue_2025 <- predictions(ptoid_main_interaction, newdata = grille_continue_2025,
                                   re_formula = NA, type = "response") %>%
  mutate(survie = 1 - estimate, survie_lwr = 1 - conf.high, survie_upr = 1 - conf.low, annee = "2025")

preds_continue_2026 <- predictions(ptoid_main_interaction, newdata = grille_continue_2026,
                                   re_formula = NA, type = "response") %>%
  mutate(survie = 1 - estimate, survie_lwr = 1 - conf.high, survie_upr = 1 - conf.low, annee = "2026")

preds_continue_all <- bind_rows(preds_continue_2025, preds_continue_2026)

## 1. Regrouper les vrais données observées en groupe de date, par année

data_obs_2025 <- data_nopupe_2025 %>%
  filter(!is.na(pres_ptoid)) %>%
  group_by(date_pose.c) %>%
  summarise(
    n = n(),
    n_parasite = sum(pres_ptoid),
    prop_parasite = mean(pres_ptoid),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(binom.test(n_parasite, n)$conf.int),
    survie_obs = 1 - prop_parasite,
    survie_obs_lwr = 1 - ci[[2]],
    survie_obs_upr = 1 - ci[[1]]
  ) %>%
  ungroup() %>%
  select(-ci) %>%
  mutate(annee = "2025")

# Même chose pour 2026

data_obs_2026 <- data_nopupe_2026 %>%
  filter(!is.na(pres_ptoid)) %>%
  group_by(date_pose.c) %>%
  summarise(
    n = n(),
    n_parasite = sum(pres_ptoid),
    prop_parasite = mean(pres_ptoid),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(binom.test(n_parasite, n)$conf.int),
    survie_obs = 1 - prop_parasite,
    survie_obs_lwr = 1 - ci[[2]],
    survie_obs_upr = 1 - ci[[1]]
  ) %>%
  ungroup() %>%
  select(-ci) %>%
  mutate(annee = "2026")

data_obs_all <- bind_rows(data_obs_2025, data_obs_2026)

## 2. Graphique

ggplot() +
  geom_ribbon(data = preds_continue_all, aes(x = date_pose.c, ymin = survie_lwr, ymax = survie_upr),
              alpha = 0.15, fill = "steelblue") +
  geom_line(data = preds_continue_all, aes(x = date_pose.c, y = survie),
            color = "steelblue", linewidth = 1) +
  geom_pointrange(data = data_obs_all,
                  aes(x = date_pose.c, y = survie_obs, ymin = survie_obs_lwr, ymax = survie_obs_upr,
                      size = n),
                  color = "black", alpha = 0.7) +
  scale_size_continuous(range = c(0.3, 1.2), name = "n larves") +
  facet_wrap(~ annee) +
  labs(
    x = "Date de déploiement (jour julien)",
    y = "Probabilité de survie",
    title = "Validation du modèle : survie prédite (courbe) vs observée (par date réelle)"
  ) +
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )


## D'accord, on peut voir que le modèle prédit beaucoup mieux l'année 2025 que 2026
## C'est relativement normal puisque nous avons beaucoup moins de données pour 2026
## (avant test d'ADN). À voir si c'est la même chose quand les années sont séparés.

## Figure Survie cumulative : log(survie)/pheno.c en spaghetti plot ----

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

surv_all_num <- log_surv_draws_all %>%
  mutate(pheno_num = case_when(pheno.c == "early" ~ -2,
                               pheno.c == "peak"  ~  0,
                               pheno.c == "late"  ~  2))

set.seed(123)
sample_draws_200 <- surv_all_num %>%
  distinct(annee, drawid) %>%
  group_by(annee) %>%
  slice_sample(n = 200) %>%
  ungroup()

surv_subset_200 <- surv_all_num %>%
  semi_join(sample_draws_200, by = c("annee", "drawid"))

##  Parabole lisse par tirage, passant par les trois scénarios

coefs_parabole_par_tirage <- surv_all_num %>%
  select(annee, drawid, pheno.c, log_surv) %>%
  pivot_wider(names_from = pheno.c, values_from = log_surv) %>%     # colonnes early, peak, late
  mutate(b0 = peak,
         b1 = (late - early) / 4,
         b2 = (late + early - 2 * peak) / 8)

pheno_num_seq <- seq(-2, 2, by = 0.05)

courbes_lisses_par_tirage <- coefs_parabole_par_tirage %>%
  crossing(pheno_num = pheno_num_seq) %>%
  mutate(log_surv = b0 + b1 * pheno_num + b2 * pheno_num^2,
         surv_prod = exp(log_surv))

courbes_lisses_200 <- courbes_lisses_par_tirage %>%
  semi_join(sample_draws_200, by = c("annee", "drawid"))

courbe_moyenne_et_bande <- courbes_lisses_par_tirage %>%
  group_by(annee, pheno_num) %>%
  summarise(surv_moy = exp(mean(log_surv)),
            surv_lwr = exp(quantile(log_surv, 0.025)),
            surv_upr = exp(quantile(log_surv, 0.975)), .groups = "drop")

## Figure

cols_annee <- c("2025" = "#0072B2", "2026" = "#D55E00")

ggplot() +
  geom_ribbon(data = courbe_moyenne_et_bande,
              aes(x = pheno_num, ymin = surv_lwr, ymax = surv_upr, fill = annee), alpha = 0.15) +
  geom_line(data = courbes_lisses_200,
            aes(x = pheno_num, y = surv_prod, group = interaction(annee, drawid), color = annee),
            alpha = 0.08) +
  geom_line(data = courbe_moyenne_et_bande,
            aes(x = pheno_num, y = surv_moy, color = annee), linewidth = 1.3) +
  scale_color_manual(values = cols_annee) +
  scale_fill_manual(values = cols_annee) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2),
                     labels = c("-2\nEarly", "-1", "0\nPeak", "1", "2\nLate")) +
  scale_y_continuous(trans = "log", breaks = scales::log_breaks(n = 5),
                     labels = scales::label_number(accuracy = 0.01)) +
  annotation_logticks(sides = "l") +
  labs(x = "Scénario phénologique", y = "Survie cumulative moyenne",
       color = "Année", fill = "Année") +
  theme_bw() +
  theme(panel.grid = element_blank())


## Sélection directionnel 2025 ----

selection_gradients_2025 <- preds_draws_2025 %>%
  mutate(
    surv = 1 - draw,
    pheno_num = case_when(
      pheno.c == "early" ~ -2,
      pheno.c == "peak"  ~ 0, # reconvertir en num car pred_draws est en catégorie
      pheno.c == "late"  ~ 2
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
      pheno.c == "early" ~ -2, 
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 2
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
      pheno.c == "early" ~ -2,
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 2
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
      pheno.c == "early" ~ -2,
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 2 
    )
  ) %>%
  group_by(drawid, pheno_num) %>%
  summarise(surv_prod = prod(surv), .groups = "drop")%>%
  mutate(log_surv = log(surv_prod)) %>%
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

# Intervalle inclut 0 aussi et très large

### Essai de quelques visualisations

#Figure: Gradient de sélection Beta et gamme selon l'année ----

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
  scale_color_manual(values = cols_annee) +
  labs(x = "Estimé du gradient de sélection", y = NULL, color = "Année") +
  theme_cowplot()

ggplot(gradient_summary, aes(x = mean, y = gradient, color = annee)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lwr, xmax = upr),
                  position = position_dodge(width = 0.5),
                  linewidth = 0.8, size = 0.8) +
  scale_color_manual(values = cols_annee) +
  labs(x = "Estimé du gradient de sélection", y = NULL, color = "Année") +
  theme_cowplot()


## Prochaine étape
## Eassyer avec les années séparés en brms pour isoler l'effet annuelle sur l'environnement, prendre les extrèmes
# Essayer model avec interaction feuillus date.pose.c
## Essayer avec pluie dans autre modèle
## Essayer avec les vrais dates BIOSIM!!
## Changer pheno.c de -1 à -2 et 2 ou essayer (ou -1.96/1.96) dans lm gradient de sélection
## Pour avoir 84 parcelles utiliser re_formula = NULL, avec une valeur propre pour chaque parcelle

## Autres Modèles Bayesiens ----

## Modèles séparés par années

#Plus utilisé maintenant car année prise en compte dans le modèle
#Modèle 2025
ptoid_main_2025 <- readRDS("ptoid_main_2025.rds")

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
#plot(ptoid_main_2025)
#pp_check(ptoid_main_2025)

#Modele 2026

#Plus utilisé maintenant car année prise en compte dans le modèle

#ptoid_main_2026 <- readRDS("ptoid_main_2026.rds")

ptoid_main_2026 <- readRDS("ptoid_main_2026.rds")
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
#plot(ptoid_main_2026)
#pp_check(ptoid_main_2026)


## Selection Gradient/Feuillus ----

# Code saved cause it's long: 

gradients_combined_Feuillus <- readRDS("gradients_combined_Feuillus.rds")

#Just need to run the figure after this.

grille_dates_2025 <- expand.grid(
  date_pose.c = c(141, 148, 164,   # early (L4, L5, L6)
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
    parcelle = as.character(na.omit(data_nopupe$parcelle)[1])   # sans effet avec re_formula = NA
  )

grille_dates_2026 <- expand.grid(
  date_pose.c = c(147, 156, 168,   # early (L4, L5, L6)
                  154, 163, 175,   # peak
                  161, 170, 182)   # late
) %>%
  mutate(
    pheno.c = case_when(
      date_pose.c %in% c(147, 156, 168) ~ "early",
      date_pose.c %in% c(154, 163, 175) ~ "peak",
      date_pose.c %in% c(161, 170, 182) ~ "late"
    ),
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    parcelle = as.character(na.omit(data_nopupe$parcelle)[1])
  )

# Fonction : beta ET gamma, séquence continue de feuillus, re_formula = NA

compute_gradients_NA_simple <- function(grille_dates, range_feuillus, modele, annee_label,
                                        n_points = 30, ndraws = 2000) {
  
  feuillus_grid <- seq(range_feuillus[1], range_feuillus[2], length.out = n_points)
  
  prediction_data <- map_dfr(feuillus_grid, function(f) grille_dates %>% mutate(feuillus = f)) %>%
    mutate(
      annee    = factor(annee_label, levels = levels(data_nopupe$annee)),
      parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))  # sans effet avec re_formula = NA
    )
  
  preds_bayes <- predictions(modele, newdata = prediction_data,
                             re_formula = NA, type = "response", ndraws = ndraws)
  preds_draws <- get_draws(preds_bayes)
  
  base <- preds_draws %>%
    mutate(
      surv = 1 - draw,
      pheno_num = case_when(
        pheno.c == "early" ~ -2,
        pheno.c == "peak"  ~ 0,
        pheno.c == "late"  ~ 2
      )
    ) %>%
    group_by(feuillus, drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod))
  
  beta_df <- base %>%
    group_by(feuillus, drawid) %>%
    summarise(beta = coef(lm(log_surv ~ pheno_num))[2], .groups = "drop") %>%
    group_by(feuillus) %>%
    summarise(mean = mean(beta), lwr = quantile(beta, 0.025), upr = quantile(beta, 0.975), .groups = "drop") %>%
    mutate(gradient = "beta")
  
  gamma_df <- base %>%
    group_by(feuillus, drawid) %>%
    summarise(
      gamma_raw = coef(lm(log_surv ~ pheno_num + I(pheno_num^2)))[3],
      gamma = 2 * gamma_raw,
      .groups = "drop"
    ) %>%
    group_by(feuillus) %>%
    summarise(mean = mean(gamma), lwr = quantile(gamma, 0.025), upr = quantile(gamma, 0.975), .groups = "drop") %>%
    mutate(gradient = "gamma")
  
  bind_rows(beta_df, gamma_df) %>% mutate(annee = annee_label)
}
## Refaire figure avec Modèle Intéraction

# Même fonction, avec le modèle d'interaction

gradients_int_2025 <- compute_gradients_NA_simple(
  grille_dates_2025, range(data_nopupe_2025$feuillus, na.rm = TRUE),
  ptoid_main_interaction, annee_label = "2025"
)

gradients_int_2026 <- compute_gradients_NA_simple(
  grille_dates_2026, range(data_nopupe_2026$feuillus, na.rm = TRUE),
  ptoid_main_interaction, annee_label = "2026"
)

gradients_interaction <- bind_rows(gradients_int_2025, gradients_int_2026) %>%
  mutate(gradient = factor(gradient, levels = c("beta", "gamma"),
                           labels = c("β (directionnel)", "γ (quadratique)")))

#saveRDS(gradients_interaction, "gradients_interaction.rds")
gradients_interaction = readRDS("gradients_interaction.rds")


# Application avec modèles séparés

#range_feuillus_2025 <- range(data_nopupe_2025$feuillus, na.rm = TRUE)
#range_feuillus_2026 <- range(data_nopupe_2026$feuillus, na.rm = TRUE)

#gradients_2025 <- compute_gradients_NA_simple(grille_dates_2025, range_feuillus_2025, ptoid_main_interaction) %>%
 # mutate(annee = "2025")

#gradients_2026 <- compute_gradients_NA_simple(grille_dates_2026, range_feuillus_2026, ptoid_main_interaction) %>%
 # mutate(annee = "2026")

#gradients_combined_Feuillus <- bind_rows(gradients_2025, gradients_2026) %>%
 # mutate(gradient = factor(gradient, levels = c("beta", "gamma"),
  #                         labels = c("β (directionnel)", "γ (quadratique)")))

#saveRDS(gradients_combined_Feuillus, file = "gradients_combined_Feuillus.rds")

## Visualisation Feuillus/Selection ----

  ggplot(gradients_interaction, aes(x = feuillus, y = mean, color = annee, fill = annee)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = cols_annee) +
    scale_fill_manual(values = cols_annee) +
    facet_wrap(~ gradient, scales = "free_y") +
  labs(
    x = "Proportion de feuillus (%)",
    y = "Gradient de sélection",
    color = "Année", fill = "Année"
  ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    )

## Figure avec B seulement
  
  ggplot(gradients_interaction %>% filter(gradient == "β (directionnel)"),
         aes(x = feuillus, y = mean, color = annee, fill = annee)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = cols_annee) +
    scale_fill_manual(values = cols_annee) +
    labs(
      x = "Proportion de feuillus (%)",
      y = "Gradient de sélection directionnel (β)",
      color = "Année", fill = "Année"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    )
  
