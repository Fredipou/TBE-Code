## Selection gradient by mean fitness ----
## This use re_formula = NULL
# Code is saved cause running the function is too long.

gradients_fitness_combined <- readRDS("gradients_fitness_combined.rds")

#Just need to run the figure after.

# 2026 needs a new df cause it doesn't have 84 parcels, which was causing trouble
# in the function

parcelles_modele_2025 <- as.character(unique(ptoid_main_2025$data$parcelle))
parcelles_modele_2026 <- as.character(unique(ptoid_main_2026$data$parcelle))

feuillus_parcelle_2025 <- data_nopupe_2025 %>%
  distinct(parcelle, feuillus) %>%
  mutate(parcelle = as.character(parcelle)) %>%
  filter(parcelle %in% parcelles_modele_2025)

feuillus_parcelle_2026 <- data_nopupe_2026 %>%
  distinct(parcelle, feuillus) %>%
  mutate(parcelle = as.character(parcelle)) %>%
  filter(parcelle %in% parcelles_modele_2026)

## Function to calcul mean fitness by parcels, allowing individual parcel variation 
# with re_formula = NULL

compute_gradients_et_fitness_parcelle <- function(grille_dates, feuillus_parcelle_table, modele, parcelle_levels, ndraws = 1000) {
  
  prediction_data <- map_dfr(seq_len(nrow(feuillus_parcelle_table)), function(i) {
    grille_dates %>%
      mutate(
        feuillus = feuillus_parcelle_table$feuillus[i],
        parcelle = factor(feuillus_parcelle_table$parcelle[i], levels = parcelle_levels)
      )
  })
  
  preds_bayes <- predictions(modele, newdata = prediction_data,
                             re_formula = NULL, type = "response", ndraws = ndraws)
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
    group_by(parcelle, drawid, pheno_num) %>%
    summarise(surv_prod = prod(surv), .groups = "drop") %>%
    mutate(log_surv = log(surv_prod))
  
  # Gradients par parcelle
  beta_df <- base %>%
    group_by(parcelle, drawid) %>%
    summarise(beta = coef(lm(log_surv ~ pheno_num))[2], .groups = "drop") %>%
    group_by(parcelle) %>%
    summarise(beta_mean = mean(beta), .groups = "drop")
  
  gamma_df <- base %>%
    group_by(parcelle, drawid) %>%
    summarise(
      gamma_raw = coef(lm(log_surv ~ pheno_num + I(pheno_num^2)))[3],
      gamma = 2 * gamma_raw, .groups = "drop"
    ) %>%
    group_by(parcelle) %>%
    summarise(gamma_mean = mean(gamma), .groups = "drop")
  
  # Fitness moyenne par parcelle (survie moyenne à travers early/peak/late)
  fitness_df <- base %>%
    group_by(parcelle, drawid) %>%
    summarise(fitness_draw = mean(surv_prod), .groups = "drop") %>%
    group_by(parcelle) %>%
    summarise(fitness_mean = mean(fitness_draw), .groups = "drop")
  
  beta_df %>%
    left_join(gamma_df, by = "parcelle") %>%
    left_join(fitness_df, by = "parcelle")
}

gradients_fitness_2025 <- compute_gradients_et_fitness_parcelle(
  grille_dates_2025, feuillus_parcelle_2025, ptoid_main_2025, parcelles_modele_2025
) %>% mutate(annee = "2025")

gradients_fitness_2026 <- compute_gradients_et_fitness_parcelle(
  grille_dates_2026, feuillus_parcelle_2026, ptoid_main_2026, parcelles_modele_2026
) %>% mutate(annee = "2026")

gradients_fitness_combined <- bind_rows(gradients_fitness_2025, gradients_fitness_2026) %>%
  pivot_longer(cols = c(beta_mean, gamma_mean), names_to = "gradient", values_to = "valeur") %>%
  mutate(gradient = factor(gradient, levels = c("beta_mean", "gamma_mean"),
                           labels = c("β (directionnel)", "γ (quadratique)")))

#saveRDS(gradients_fitness_combined, file = "gradients_fitness_combined.rds")


ggplot(gradients_fitness_combined, aes(x = fitness_mean, y = valeur, color = annee, fill = annee)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(end = 0.8) +
  scale_fill_viridis_d(end = 0.8) +
  facet_wrap(~ gradient, scales = "free_y") +
  labs(
    x = "Mean fitness (survival)",
    y = "Selection gradient",
    color = "Year", fill = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

##Intéressant mais semble un peu trop parfait, could be an artefact form the model? transformation logit problématique?


## vérification ----
## Test parcelles avec seulement des niveaux de base différents, mais même effet de la phénologie partout
# et compare de la courbes simulés vs observés


## Résultat de la fonction saved car très long.
artefact_results <- read_rds("artefact_results.rds")
# Juste à run la figure après

# 1. Prédicteur linéaire (logit) pour les 9 points, feuillus moyen, SANS effet aléatoire

grille_test <- grille_dates_2025 %>%
  mutate(feuillus = mean(data_nopupe_2025$feuillus, na.rm = TRUE))

eta_draws <- posterior_linpred(ptoid_main_2025, newdata = grille_test, re_formula = NA) #Re_formula = NA donc pas de variation entre parcelles.
# eta_draws : matrice [n_draws x 9]

# 2. Grille de décalages artificiels (simule une gamme réaliste de variation entre parcelles,
#    basée sur sd(Intercept) ~ 0.66 observé dans le modèle)

delta_grid <- seq(-2, 2, length.out = 20)

pheno_num_vec <- case_when(
  grille_test$pheno.c == "early" ~ -2,
  grille_test$pheno.c == "peak"  ~ 0,
  grille_test$pheno.c == "late"  ~ 2
)

# 3. Pour chaque delta, calculer beta et fitness moyenne

artefact_results <- purrr::map_dfr(delta_grid, function(d) {
  prob <- plogis(eta_draws + d)
  surv <- 1 - prob
  
  n_draws <- nrow(surv)
  beta_vec <- numeric(n_draws)
  fitness_vec <- numeric(n_draws)
  
  for (i in seq_len(n_draws)) {
    df_i <- data.frame(pheno_num = pheno_num_vec, surv = surv[i, ])
    surv_prod <- df_i %>% group_by(pheno_num) %>% summarise(sp = prod(surv)) 
    log_surv <- log(surv_prod$sp)
    beta_vec[i] <- coef(lm(log_surv ~ surv_prod$pheno_num))[2]
    fitness_vec[i] <- mean(surv_prod$sp)
  }
  
  data.frame(delta = d, beta_mean = mean(beta_vec), fitness_mean = mean(fitness_vec))
})

saveRDS(artefact_results, file = "artefact_results.rds")
#print(artefact_results)

ggplot() +
  geom_point(data = gradients_fitness_combined %>% filter(gradient == "β (directionnel)", annee == "2025"),
             aes(x = fitness_mean, y = valeur), color = "purple", alpha = 0.5) +
  geom_line(data = artefact_results, aes(x = fitness_mean, y = beta_mean), 
            color = "black", linewidth = 1.2, linetype = "dashed") +
  labs(
    x = "Mean fitness ",
    y = "Beta",
  ) +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

## Artefact et points sont presque pareils..

# Est-ce qu'il reste un signal quand on retire "l'artefact du logit" ?

# 1. Créer une fonction d'interpolation à partir de la courbe artefact

predict_beta_artefact <- approxfun(x = artefact_results$fitness_mean, y = artefact_results$beta_mean)

# 2. Calculer le résidu pour chaque parcelle réelle (2025)
beta_residus_2025 <- gradients_fitness_combined %>%
  filter(gradient == "β (directionnel)", annee == "2025") %>%
  mutate(
    beta_predit_artefact = predict_beta_artefact(fitness_mean),
    residu = valeur - beta_predit_artefact
  )

# 3. Vérifier si les résidus montrent encore une tendance selon fitness

ggplot(beta_residus_2025, aes(x = fitness_mean, y = residu)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    x = "Mean fitness",
    y = "Residuals (Beta obs - Beta pred)",
  ) +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )
## Reste un signal mais beaucoup moins fort que ce que l'on voyait avant.. (0.03 à 0.01)