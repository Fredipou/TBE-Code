#### Model Testing 2025-12-16

## Testing Linear models with full dataset (including pupae)

# Models from earlier

model_glmer_1 <- glmer(pres_ptoid_clean ~ feuillus + pheno.c*stade.c + (1 | parcelle), 
                     family = binomial, data = Recolte_foret)
summary(model_glmer_1)

# AIC of 586 and Feuillus + stade.c < 0.05, but no interaction of stade and pheno

model_glmer_2 <- glmer(pres_ptoid_clean ~ feuillus + pheno.c + stade.c + (1 | parcelle), 
                       family = binomial, data = Recolte_foret)
summary(model_glmer_2)

#AIC of 584.5, all variables signif 

## Sans les pupes

model_glmer_1.1 <- glmer(pres_ptoid_clean ~ feuillus + pheno.c*stade.c + (1 | parcelle), 
                       family = binomial, data = data_nopupe)
summary(model_glmer_1.1)

# AIC 442.2, maintenant feuillus, pheno.c et interaction pheno*stade < 0.05. 
# Confirme que les pupes semblent masquées l'effet de la pheno, pas d'effet de stade

model_glmer_2.1 <- glmer(pres_ptoid_clean ~ feuillus + pheno.c + stade.c + (1 | parcelle), 
                       family = binomial, data = data_nopupe)
summary(model_glmer_2.1)

#AIC = 459.3, toutes variables < 0.05

#### Testing non-linar models (GAM & GAMM) with full dataset and date.pose.c

# Intéraction non linéaire entre pheno et stade


Recolte_foret$parcelle <- factor(Recolte_foret$parcelle)
data_nopupe$parcelle <- factor(data_nopupe$parcelle)

model_gam_date <- gam(
  pres_ptoid_clean ~ 
    feuillus +
    s(date_pose.c, k = 6) +
    s(parcelle, bs = "re"),
  family = binomial,
  data = Recolte_foret,
  method = "REML"
)

gam.check(model_gam_date)
summary(model_gam_date)
plot(model_gam_date, shade = TRUE, pages = 1)
draw(model_gam_date)

## Sans les pupes

model_gam_date2 <- gam(
  pres_ptoid_clean ~ 
    feuillus +
    s(date_pose.c, k = 6) +
    s(parcelle, bs = "re"),
  family = binomial,
  data = data_nopupe,
  method = "REML"
)

gam.check(model_gam_date2)
summary(model_gam_date2)
plot(model_gam_date2, shade = TRUE, pages = 1)
draw(model_gam_date2)

AIC(model_glmer_1.1, model_gam_date2)

# Fonctionne toujours, date_pose.c signif et clairment non-linéaire (EDF = 3.26)
# Explique 22.3% de variance! AIC = 424 donc meilleur que glmer

#### Essai de transférer ce modèle en Bayesien

# Priors

updated_priors <- c(
  set_prior("normal(0, 0.5)", class = "b"),        # Fixed effects
  set_prior("student_t(3, 0, 2)", class = "sd"),   # Random effects
  set_prior("normal(0, 1)", class = "sds")         # Smooth standard deviation
)

# Updated Bayesian GAM

updated_bayes_model <- brm(
  formula = pres_ptoid_clean ~ feuillus + s(date_pose.c, k = 6, bs = "tp") + (1 | parcelle),
  family = bernoulli(),       
  data = data_nopupe,
  prior = updated_priors,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

summary(updated_bayes_model)  
pp_check(updated_bayes_model)
