### Testing model for survival analysis ----

source("Data_cleaning.R")

### First option (GLMER) ----
### Mod with pupae ----

model_glmer_ptoid <- glmer(pres_ptoid ~ feuillus + pheno.c*stade + (1 | parcelle), 
                       family = binomial, data = Recolte_foret)

summary(model_glmer_ptoid)

model_glmer_surv <- glmer(survie_larve ~ feuillus + pheno.c*stade + (1 | parcelle), 
                       family = binomial, data = Recolte_foret)

summary(model_glmer_surv)

# For 2025 let's exclude pupae since they seem really to not respond at all
# to either Feuillus, pheno.c and have really low base parasitism. From the first year,
# we gather that pupae are biologically different from the rest of the stages.
# Let's try a model with only pupae to confirm:
#
#mod_just_pupe = glmer(pres_ptoid ~ feuillus + pheno.c + (1 | parcelle), 
#                      family = binomial, data = data_justpupe)
#summary(mod_just_pupe)
#
#ggplot(data_justpupe, aes(x = feuillus, y = survie_larve)) +
#  geom_point(alpha = 0.3, position = position_jitter(height = 0.05)) +
#  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
#              se = TRUE, color = "#0072B2", size = 1.2) +
#  labs(
#    title = "Effet du % de feuillus sur la probabilité de survie",
#    x = "% de feuillus",
#    y = "Probabilité de parasitisme"
#  ) +
#  theme_minimal()

### Mod without pupae ----

model_glmer_ptoid_2 <- glmer(pres_ptoid ~ feuillus + pheno.c*stade + (1 | parcelle), 
                           family = binomial, data = data_nopupe)
summary(model_glmer_ptoid_2)

model_glmer_surv_2 <- glmer(survie_larve ~ feuillus + pheno.c*stade + (1 | parcelle), 
                          family = binomial, data = data_nopupe)
summary(model_glmer_surv_2)

r.squaredGLMM(model_glmer_surv)
r.squaredGLMM(model_glmer_surv_2)

r.squaredGLMM(model_glmer_ptoid)
r.squaredGLMM(model_glmer_ptoid_2)

### Extracting survival probabilities with emmeans from glmer ----

emmeans(model_glmer_ptoid_2, ~ stade * pheno.c,
        at = list(pheno.c = c(-1, 0, 1),
                  feuillus = mean(data_nopupe$feuillus)),
        type = "response")

Ptoid_early = (1 - 0.054) * (1 - 0.497) * (1 - 0.718)
Ptoid_peak = (1 - 0.259) * (1 - 0.579) * (1 - 0.378)
Ptoid_late = (1 - 0.683) * (1 - 0.656) * (1 - 0.127)

# Early = 0.13, Peak = 0.19, late = 0.09

### Same thing but with survival instead of pres_ptoid

emmeans(model_glmer_surv_2, ~ stade * pheno.c,
        at = list(pheno.c = c(-1, 0, 1),
                  feuillus = mean(data_nopupe$feuillus)),
        type = "response")

Surv_early = 0.172 * 0.0309 * 0.231
Surv_peak = 0.205*0.245*0.443
Surv_late = 0.242*0.190*0.679

# Early = 0.001, Peak = 0.02, Late = 0.03.
# Even though, in term of mortality from ptoid earlier larvae seemed to escape
# a bit more their mortality is higher than all other. Could be affected by
# problem in rearing from L4.

### Second option (GAM with date_pose.c) ----
## Gam since it seems that date_pose.c isn't linear

Recolte_foret$parcelle <- as.factor(Recolte_foret$parcelle)
data_nopupe$stade <- as.factor(data_nopupe$stade)
data_nopupe$parcelle <- as.factor(data_nopupe$parcelle)

gamm_ptoid_simple <- gam(
  pres_ptoid ~ feuillus +
    s(date_pose.c, k = 7) +
    s(parcelle, bs = "re"),
  family = binomial, data = data_nopupe, method = "REML"
)
summary(gamm_ptoid_simple)
gam.check(gamm_ptoid_simple)

# Not sure how to analyse this since s(parcelle) is now NA. Seems k is still under 1
# Temporal auto-correlation?

# Plot
draw(gamm_ptoid_simple)
plot(gamm_ptoid_simple, select = 1, shade = TRUE, 
     xlab = "Date pose (centered)", 
     ylab = "Effect on parasitoid presence (log-odds)")

#Clearly seeing the non-linear pattern

#Trying to add stade as a variable

gamm_ptoid_stade <- gam(
  pres_ptoid ~ feuillus +
    s(date_pose.c, by = stade, k = 7) +
    stade +
    s(parcelle, bs = "re"),
  family = binomial, data = data_nopupe, method = "REML"
)
summary(gamm_ptoid_stade)
gam.check(gamm_ptoid_stade)

draw(gamm_ptoid_stade)
plot(gamm_ptoid_stade, select = 1, shade = TRUE, 
     xlab = "Date pose (centered)", 
     ylab = "Effect on parasitoid presence (log-odds)")

#Interesting, edf for L4 and L6 of 1 suggesting linearity for these stade
# but nor for L5, also REML better in this model. 

# Compare

AIC(gamm_ptoid_simple, gamm_ptoid_stade)

# Less AIC is gamm_ptoid_stade but it is really close. Maybe prioritize the lower
# df model. Not enough of a difference probably

#gamm_ptoid_stade_2 <- gam(
#  pres_ptoid ~ feuillus +
#    s(date_pose.c, k = 7) +
#    stade +
#    s(parcelle, bs = "re"),
#  family = binomial, data = Recolte_foret, method = "REML"
#)
#summary(gamm_ptoid_stade_2)
#gam.check(gamm_ptoid_stade_2)
#AIC(gamm_ptoid_simple_pupae, gamm_ptoid_stade_2)

## GAM with survival ----
# Let's re-do these but with survival instead.

gamm_surv_simple <- gam(
  survie_larve ~ feuillus +
    s(date_pose.c, k = 7) +
    s(parcelle, bs = "re"),
  family = binomial, data = data_nopupe, method = "REML"
)
summary(gamm_surv_simple)
gam.check(gamm_surv_simple)

# k a bit better, feuillus no longer signif.

# Plot

draw(gamm_surv_simple)
plot(gamm_surv_simple, select = 1, shade = TRUE, 
     xlab = "Date pose (centered)", 
     ylab = "Effect on larval survival (log-odds)")

#Clearly seeing the non-linear pattern again

#Trying to add stade as a variable

gamm_surv_stade <- gam(
  survie_larve ~ feuillus +
    s(date_pose.c, by = stade, k = 7) +
    stade +
    s(parcelle, bs = "re"),
  family = binomial, data = data_nopupe, method = "REML"
)
summary(gamm_surv_stade)
gam.check(gamm_surv_stade)

draw(gamm_surv_stade)
plot(gamm_surv_stade, select = 1, shade = TRUE, 
     xlab = "Date pose (centered)", 
     ylab = "Effect on larval survival (log-odds)")

# Interesting, edf for L4 and L5 of 1.5 suggesting, weak non-linear effect?
# Plot is hard to analyse.

# Compare

AIC(gamm_surv_simple, gamm_surv_stade)

#Here it seems that the model simple is clearly better than the other

## Extracting prediction from GAM model ----

# 1) Create df for early, peak, late with each corresponding dates

pred_data <- expand.grid(
  date_pose.c = c(141, 148, 164,   # early
                  150, 155, 171,   # peak
                  157, 162, 177),  # late
  parcelle    = levels(data_nopupe$parcelle)
) %>%
  mutate(
    pheno = case_when(
      date_pose.c %in% c(141, 148, 164) ~ "early",
      date_pose.c %in% c(150, 155, 171) ~ "peak",
      date_pose.c %in% c(157, 162, 177) ~ "late"
    ),
    stage_date = case_when(
      date_pose.c %in% c(141, 150, 157) ~ "L4",
      date_pose.c %in% c(148, 155, 162) ~ "L5",
      date_pose.c %in% c(164, 171, 177) ~ "L6"
    ),
    pheno        = factor(pheno, levels = c("early", "peak", "late")),
    feuillus    = mean(data_nopupe$feuillus, na.rm = TRUE)
  )

# Predict

preds <- predict(gamm_ptoid_simple,
                 newdata = pred_data,
                 type    = "link",
                 se.fit  = TRUE)
# include Id to have prediction for all parcels

# Add confidence interval

pred_data$fit        <- preds$fit
pred_data$lower      <- preds$fit - 1.96 * preds$se.fit
pred_data$upper      <- preds$fit + 1.96 * preds$se.fit
pred_data$prob       <- plogis(pred_data$fit)
pred_data$prob_lower <- plogis(pred_data$lower)
pred_data$prob_upper <- plogis(pred_data$upper)

view(pred_data)

# Extract mean prob 

pred_data %>%
  group_by(pheno, stage_date) %>%
  summarise(
    mean_prob       = mean(prob),
    mean_prob_lower = mean(prob_lower),
    mean_prob_upper = mean(prob_upper),
    .groups = "drop"
  )

#Checking manually

Ptoid_early_gam = (1 - 0.0424) * (1 - 0.205) * (1 - 0.651)
Ptoid_peak_gam = (1 - 0.315) * (1 - 0.604) * (1 - 0.387)
Ptoid_Late_gam = (1 - 0.661) * (1 - 0.680) * (1 - 0.152)

#Early = 0.26, Peak = 0.16, Late = 0.09. Now early is clearly favored.

# Extract mean prob et calcul automatique du Ptoid

ptoid_results <- pred_data %>%
  group_by(pheno, stage_date) %>%
  summarise(
    mean_prob       = mean(prob),
    mean_prob_lower = mean(prob_lower),
    mean_prob_upper = mean(prob_upper),
    .groups = "drop"
  ) %>%
  group_by(pheno) %>%
  summarise(
    Ptoid     = prod(1 - mean_prob),
    Ptoid_lower = prod(1 - mean_prob_upper),
    Ptoid_upper = prod(1 - mean_prob_lower),
    .groups = "drop"
  )

view(ptoid_results)


#3. Plot
pred_data$path        <- factor(pred_data$path,        levels = c("early", "peak", "late"))
pred_data$stage_label <- factor(pred_data$stage_label, levels = c("L4", "L5", "L6"))

ggplot(pred_data, aes(x = date_pose.c, y = prob, colour = path, group = path)) +
  geom_ribbon(aes(ymin = prob_lower, ymax = prob_upper, fill = path),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = stage_label), size = 3) +
  scale_colour_manual(values = c("early" = "#2166ac",
                                 "peak"  = "#d6604d",
                                 "late"  = "#4dac26")) +
  scale_fill_manual(values   = c("early" = "#2166ac",
                                 "peak"  = "#d6604d",
                                 "late"  = "#4dac26")) +
  scale_shape_manual(values  = c("L4" = 15, "L5" = 16, "L6" = 17)) +
  scale_y_continuous(limits  = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks  = c(141, 148, 150, 155, 157, 162, 164, 171, 177)) +
  labs(x        = "Date pose (day of year)",
       y        = "Predicted P(parasitoid presence)",
       colour   = "Phenological path",
       fill     = "Phenological path",
       shape    = "Larval stage",
       title    = "Expected parasitoid presence across larval life",
       subtitle = "Population-level predictions at mean feuillus (parcelle effect excluded)") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


