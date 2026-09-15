# 1. Transformer carac_normal de format large à format long
abondance_long <- Carac_normal %>%
  pivot_longer(
    cols = -parcelle,
    names_to = "species",
    values_to = "abondance_rel"
  )

# 2. Calculer le QMD par parcelle/espèce (comme avant)
DHP_qmd_espece <- Biomasse %>%
  mutate(DHP = as.numeric(DHP)) %>%
  group_by(parcelle, species) %>%
  summarise(
    DHP_qmd = sqrt(mean(DHP^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(DHP_qmd = ifelse(is.nan(DHP_qmd), NA_real_, DHP_qmd))

# 3. Joindre QMD et abondance relative par parcelle/espèce
biomasse_pondere <- DHP_qmd_espece %>%
  left_join(abondance_long, by = c("parcelle", "species")) %>%
  filter(!is.na(DHP_qmd), !is.na(abondance_rel))

# 4. Calculer l'indice pondéré par parcelle
biomasse_index_pondere <- biomasse_pondere %>%
  group_by(parcelle) %>%
  summarise(
    biomasse_index_weighted = weighted.mean(
      DHP_qmd,
      w = abondance_rel,
      na.rm = TRUE
    ),
    .groups = "drop"
  )
(biomasse_index_pondere)

Recolte_foret <- Recolte_foret %>%
  left_join(biomasse_index_pondere, by = "parcelle")
###

# 1. Modèle sans biomasse
gamm_ptoid_nobiomasse <- gam(
  pres_ptoid ~ feuillus +
    annee_factor +
    s(date_pose.c, by = annee_factor, k = 12) +
    s(parcelle, bs = "re"),
  family = binomial, data = data_gamm, method = "REML"
)

# 2. Modèle avec biomasse non pondérée (déjà fait, mais À REFITTER sur data_gamm actuel)
gamm_ptoid_biomasse_simple <- gam(
  pres_ptoid ~ feuillus +
    biomasse_index +
    annee_factor +
    s(date_pose.c, by = annee_factor, k = 12) +
    s(parcelle, bs = "re"),
  family = binomial, data = data_gamm, method = "REML"
)

# 3. Modèle avec biomasse pondérée (déjà fait)
# gamm_ptoid_biomasse_pondere -- déjà disponible
cor.test(Recolte_foret$biomasse_index_weighted, Recolte_foret$feuillus)
AIC(gamm_ptoid_nobiomasse, gamm_ptoid_biomasse_simple, gamm_ptoid_biomasse_pondere)

ggplot(Recolte_foret, aes(x = feuillus, y = biomasse_index_weighted)) +
  geom_point() +
  geom_smooth(method = "lm")
