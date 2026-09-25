#Path analysis
# Version large (avant pivot_longer), pour chaque année
path_data_2025 <- gradients_fitness_2025 %>%
  mutate(parcelle = as.character(parcelle)) %>%
  left_join(feuillus_parcelle_2025, by = "parcelle")

path_data_2026 <- gradients_fitness_2026 %>%
  mutate(parcelle = as.character(parcelle)) %>%
  left_join(feuillus_parcelle_2026, by = "parcelle")

# Vérification
head(path_data_2025)
sum(is.na(path_data_2025$feuillus))  # devrait être 0

# install.packages("lavaan")  # si pas déjà installé
library(lavaan)

path_model <- '
  # Feuillus explique la fitness moyenne
  fitness_mean ~ a * feuillus
  
  # Feuillus ET fitness moyenne expliquent le gradient de sélection
  beta_mean ~ b * fitness_mean + c * feuillus
  
  # Effet indirect (feuillus -> fitness -> beta) et effet total
  indirect := a * b
  total := c + (a * b)
'

fit_2025 <- sem(path_model, data = path_data_2025)
summary(fit_2025, standardized = TRUE, fit.measures = TRUE)

fit_2026 <- sem(path_model, data = path_data_2026)
summary(fit_2026, standardized = TRUE, fit.measures = TRUE)

standardizedSolution(fit_2025) %>%
  filter(op %in% c(":=", "~")) %>%
  select(lhs, op, rhs, est.std, se, pvalue, ci.lower, ci.upper)

## Interprétation
# 2025: feuillus -> fitness_mean, a= 0.726 et p=0.001. effet positif et fort, plus de feuillus = plus de survie
# fitness_mean -> beta_mean b = 0.971 et p = 0.001, effet très fort... artefact?
# feuillus -> beta_mean, c = 0.010 et p= 0.772, quand on tient compte du fitness, % de feuillus n'a plus d'effet
# direct sur gradient de sélection
# 2026: problème de modèle a réassayer avec NA corrigé.

