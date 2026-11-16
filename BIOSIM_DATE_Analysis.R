## Testing main analyses with BIOSIM date of after 2026 field season ----

source("Data_cleaning.R")

## Getting a mean date for every larval stage and year

prep <- function(df, scenario) {
  df %>%
    transmute(parcelle = Name, annee = Year, scenario = scenario,
              L4 = yday(L4), L5 = yday(L5), L6 = yday(L6), Pupae = yday(Pupae))
}

biosim <- bind_rows(prep(BIOSIM_EARLY, "early"),
                    prep(BIOSIM_PEAK,  "peak"),
                    prep(BIOSIM_LATE,  "late")) %>%
  pivot_longer(c(L4, L5, L6, Pupae), names_to = "stade", values_to = "jour_julien") %>%
  mutate(stade = factor(stade, levels = c("L4", "L5", "L6", "Pupae")),
         scenario = factor(scenario, levels = c("early", "peak", "late")))

# 1. Une ligne par parcelle, année, stade et scénario

biosim_parcelle <- biosim

# 2. Moyenne par année, stade et scénario (toutes parcelles)

biosim_moy <- biosim %>%
  group_by(annee, stade, scenario) %>%
  summarise(jour_moy = mean(jour_julien, na.rm = TRUE),
            sd = sd(jour_julien, na.rm = TRUE),
            min = min(jour_julien), max = max(jour_julien),
            n_parcelles = n(), .groups = "drop")

biosim_moy

## Vérifications

# Doublons éventuels (plusieurs lignes par parcelle, année, stade, scénario)
biosim %>% count(parcelle, annee, stade, scenario) %>% filter(n > 1)

# Ordre early <= peak <= late
biosim %>% pivot_wider(names_from = scenario, values_from = jour_julien) %>%
  filter(!(early <= peak & peak <= late))

# Parcelles communes avec data_nopupe

setdiff(unique(biosim$parcelle), levels(data_nopupe$parcelle))
setdiff(unique(biosim$parcelle), unique(as.character(data_nopupe$parcelle)))
setdiff(unique(as.character(data_nopupe$parcelle)), unique(biosim$parcelle))

biosim <- biosim %>% mutate(parcelle = recode(parcelle, "C59" = "C49"))
# Une parcelle mal identifiée dans BIOSIM

biosim

## Écarts entre dates utilisées et date BIOSIM

pose <- tibble::tribble(
  ~annee, ~stade.c, ~pheno.c, ~pose,
  2025, 4, -2, 141, 2025, 5, -2, 148, 2025, 6, -2, 164,
  2025, 4,  0, 150, 2025, 5,  0, 155, 2025, 6,  0, 171,
  2025, 4,  2, 157, 2025, 5,  2, 162, 2025, 6,  2, 177,
  2026, 4, -2, 147, 2026, 5, -2, 156, 2026, 6, -2, 168,
  2026, 4,  0, 154, 2026, 5,  0, 163, 2026, 6,  0, 175,
  2026, 4,  2, 161, 2026, 5,  2, 170, 2026, 6,  2, 182
)

bio <- biosim_moy %>%
  filter(stade != "Pupae") %>%
  mutate(stade.c = as.numeric(sub("L", "", stade)),
         pheno.c = c(early = -2, peak = 0, late = 2)[as.character(scenario)]) %>%
  select(annee, stade.c, pheno.c, biosim = jour_moy)

decalage <- pose %>%
  left_join(bio, by = c("annee", "stade.c", "pheno.c")) %>%
  mutate(decalage = pose - biosim) %>%
  arrange(annee, stade.c, pheno.c)

print(decalage, n = 18)

## Comme attendue, il y a un décalage entre les dates de pose et de BIOSIM. Cependant, les larves
## passaient 7 jours sur le terrain.. Peut-être qu'une question plus importante est: est-ce que
## la date de pose prédite par BIOSIM est comprise entre date_pose et date_récolte?

decalage %>%
  mutate(biosim_j     = round(biosim),
         fin          = pose + 6,
         dans_fenetre = biosim_j >= pose & biosim_j <= fin) %>%
  select(annee, stade.c, pheno.c, pose, fin, biosim, biosim_j, dans_fenetre) %>%
  print(n = 18)

## D'accord donc: En 2025, l'intervalle L6 early (BIOSIM 2 jours avant) et L6 peak (BIOSIM 0.7 jour avant)
# ne comprennent pas la date BIOSIM
## En 2026, l'intervalle L5 peak (BIOSIM 3 jours avant), L5 late (BIOSIM 7 jours avant) et L6 early (4 jours avant)
## Résumé: décalage surtout L5 late 2026, L5 peak 2026 et L6 early 2026. 
#
## Refaire les analyses avec les dates BIOSIM

# Deux grilles au même format : dates de pose (base) et dates BIOSIM

mk_grille <- function(df, col) {
  df %>% transmute(annee, stade_date = paste0("L", stade.c),
                   pheno.c = factor(c(`-2` = "early", `0` = "peak", `2` = "late")[as.character(pheno.c)],
                                    levels = c("early", "peak", "late")),
                   date_pose.c = .data[[col]])
}
grille_pose <- mk_grille(decalage, "pose")
grille_bio  <- mk_grille(decalage, "biosim")      # moyennes BIOSIM par année, stade et scénario (non arrondies)

# Contrôle : les dates BIOSIM restent-elles dans la plage observée ?

range(data_nopupe_2025$date_pose.c, na.rm = TRUE); range(data_nopupe_2026$date_pose.c, na.rm = TRUE)
grille_bio %>% group_by(annee) %>% summarise(min = min(date_pose.c), max = max(date_pose.c))

# Survie cumulée, β, γ et rapports de survie pour une grille et une année

calc_sel <- function(grille, annee_lab, modele = ptoid_main_interaction) {
  nd <- grille %>% filter(annee == annee_lab) %>%
    mutate(feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
           annee    = factor(as.character(annee_lab), levels = levels(data_nopupe$annee)),
           parcelle = as.character(na.omit(data_nopupe$parcelle)[1]))   # sans effet avec re_formula = NA
  surv <- 1 - posterior_epred(modele, newdata = nd, re_formula = NA)
  logW <- sapply(c("early", "peak", "late"),
                 function(s) rowSums(log(surv[, nd$pheno.c == s, drop = FALSE])))
  out <- tibble(beta  = (logW[, "late"] - logW[, "early"]) / 4,
                gamma = (logW[, "late"] + logW[, "early"] - 2 * logW[, "peak"]) / 4,
                rr_early = exp(logW[, "early"] - logW[, "peak"]),
                rr_late  = exp(logW[, "late"]  - logW[, "peak"]))
  out %>% pivot_longer(everything(), names_to = "quantite") %>%
    group_by(quantite) %>%
    summarise(mean = mean(value), lwr = quantile(value, 0.025), upr = quantile(value, 0.975), .groups = "drop")
}

res <- expand_grid(source = c("Pose", "BIOSIM"), annee = c(2025, 2026)) %>%
  pmap_dfr(function(source, annee) {
    g <- if (source == "Pose") grille_pose else grille_bio
    calc_sel(g, annee) %>% mutate(source = source, annee = annee)
  })

res %>% arrange(quantite, annee, source) %>% print(n = 32)

# 1. Grille BIOSIM limitée à la plage observée (2025 : max 177 ; 2026 : max 182)
grille_bio_cap <- grille_bio %>%
  mutate(date_pose.c = case_when(annee == 2025 ~ pmin(date_pose.c, 177),
                                 annee == 2026 ~ pmin(date_pose.c, 182),
                                 TRUE ~ date_pose.c))

# Vérifier quelles dates ont été limitées (devrait être L6 tardif 2025)
grille_bio %>% filter(date_pose.c > ifelse(annee == 2025, 177, 182))

# 2. Relancer les trois sources

res <- expand_grid(source = c("Pose", "BIOSIM", "BIOSIM_cap"), annee = c(2025, 2026)) %>%
  pmap_dfr(function(source, annee) {
    g <- switch(source, Pose = grille_pose, BIOSIM = grille_bio, BIOSIM_cap = grille_bio_cap)
    calc_sel(g, annee) %>% mutate(source = source, annee = annee)
  })

res %>% arrange(quantite, annee, source) %>% print(n = 48)

# Date changée seulement si la date BIOSIM n'était pas comprise dans l'intervalle

# 1. Dates modifiées seulement pour les combinaisons hors fenêtre
decalage_mix <- decalage %>%
  mutate(biosim_j = round(biosim),
         dans     = biosim_j >= pose & biosim_j <= pose + 6,
         date_mix = ifelse(dans, pose, biosim_j))

decalage_mix %>% filter(!dans) %>% select(annee, stade.c, pheno.c, pose, biosim_j, date_mix)
# attendu : 2025 L6 hâtif, et 2026 L5 pic, L5 tardif, L6 hâtif

grille_mix <- mk_grille(decalage_mix, "date_mix")

# 2. Même calcul que pour les autres sources
res_mix <- map_dfr(c(2025, 2026), function(a) {
  calc_sel(grille_mix, a) %>% mutate(source = "Mixte", annee = a)
})

bind_rows(res, res_mix) %>%
  filter(source %in% c("Pose", "Mixte", "BIOSIM")) %>%
  arrange(quantite, annee, source) %>% print(n = 40)

#saveRDS(bind_rows(res, res_mix), "selection_pose_biosim_mixte.rds")

# Figure Période d'exposition vs BIOSIM dates ----

cols <- c("Fenêtre d'exposition (pose + 6 j)" = "#0072B2", "BIOSIM" = "#D55E00")

# The names must match the labels used in aes(color = "...") below
cols <- c("Exposure window (placement + 6 d)" = "#0072B2", "BIOSIM" = "#D55E00")

bio <- biosim_moy %>%
  filter(stade != "Pupae") %>%
  mutate(stade.c = as.numeric(sub("L", "", stade)),
         pheno.c = c(early = -2, peak = 0, late = 2)[as.character(scenario)]) %>%
  select(annee, stade.c, pheno.c, biosim = jour_moy, bmin = min, bmax = max)

fig_dat <- decalage %>%
  select(annee, stade.c, pheno.c, pose) %>%
  left_join(bio, by = c("annee", "stade.c", "pheno.c")) %>%
  mutate(fin      = pose + 6,
         inside   = round(biosim) >= pose & round(biosim) <= fin,
         stage    = paste0("L", stade.c),
         scenario = factor(c(`-2` = "Early", `0` = "Peak", `2` = "Late")[as.character(pheno.c)],
                           levels = c("Early", "Peak", "Late")),
         position = ifelse(inside, "Inside window", "Outside window"))

fig_dates <- ggplot(fig_dat, aes(y = scenario)) +
  geom_segment(aes(x = pose, xend = fin, yend = scenario,
                   color = "Exposure window (placement + 6 d)"),
               linewidth = 5, alpha = 0.6) +
  geom_segment(aes(x = bmin, xend = bmax, yend = scenario, color = "BIOSIM"),
               linewidth = 0.8) +
  geom_point(aes(x = biosim, color = "BIOSIM", shape = position), size = 3.5) +
  scale_color_manual(values = cols, name = NULL) +
  scale_shape_manual(values = c("Inside window" = 16, "Outside window" = 4), name = NULL) +
  scale_y_discrete(limits = rev) +
  facet_grid(stage ~ annee, scales = "free_x") +
  labs(x = "Day of year", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.border = element_rect(color = "grey70", fill = NA),
        legend.position = "bottom")

fig_dates

## Refaire la Figure principale mais avec Date BIOSIM ----

# 1) Grilles de prédiction avec les dates BioSIM
# Écrites en tribble parce que certaines dates BioSIM se répètent
# (159 en 2025 et 161 en 2026), ce qui brise un case_when sur les dates.

# Grille 2025 (dates BioSIM)

prediction_data_2025_biosim <- tribble(
  ~date_pose.c, ~pheno.c, ~stage_date,
  147, "early", "L4",
  153, "early", "L5",
  162, "early", "L6",
  152, "peak",  "L4",
  159, "peak",  "L5",
  170, "peak",  "L6",
  159, "late",  "L4",
  167, "late",  "L5",
  181, "late",  "L6"
) %>%
  mutate(
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
    annee    = factor("2025", levels = levels(data_nopupe$annee))
  )

# Grille 2026 (dates BioSIM)

prediction_data_2026_biosim <- tribble(
  ~date_pose.c, ~pheno.c, ~stage_date,
  150, "early", "L4",
  156, "early", "L5",
  164, "early", "L6",
  155, "peak",  "L4",
  161, "peak",  "L5",
  172, "peak",  "L6",
  161, "late",  "L4",
  168, "late",  "L5",
  182, "late",  "L6"
) %>%
  mutate(
    pheno.c  = factor(pheno.c, levels = c("early", "peak", "late")),
    feuillus = mean(data_nopupe$feuillus, na.rm = TRUE), # couvert feuillus moyen 33%
    annee    = factor("2026", levels = levels(data_nopupe$annee))
  )


# Prédictions marginales, feuillus = 33% fixe, effet au niveau population ----

prediction_data_2025_biosim <- prediction_data_2025_biosim %>%
  mutate(parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle))) # a besoin d'une parcelle

prediction_data_2026_biosim <- prediction_data_2026_biosim %>%
  mutate(parcelle = factor(levels(data_nopupe$parcelle)[1], levels = levels(data_nopupe$parcelle)))

# Prédictions 2025

preds_bayes_2025_biosim <- predictions(ptoid_main_interaction, newdata = prediction_data_2025_biosim,
                                       re_formula = NA, type = "response") # chiffre entre 0 et 1
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

# Prédictions 2026

preds_bayes_2026_biosim <- predictions(ptoid_main_interaction, newdata = prediction_data_2026_biosim,
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

## Vérifications

# prediction_data_2025_biosim %>% count(pheno.c, stage_date)  # 9 lignes, 1 par combinaison
# prediction_data_2026_biosim %>% count(pheno.c, stage_date)

## Comparaison dates de pose réelles vs BioSIM
## (nécessite survie_bayes_2025 et survie_bayes_2026 du script original)

# bind_rows(
#   survie_bayes_2025        %>% mutate(annee = "2025", dates = "pose"),
#   survie_bayes_2025_biosim %>% mutate(annee = "2025", dates = "BioSIM"),
#   survie_bayes_2026        %>% mutate(annee = "2026", dates = "pose"),
#   survie_bayes_2026_biosim %>% mutate(annee = "2026", dates = "BioSIM")
# ) %>%
#   arrange(annee, pheno.c, dates)


## Figure Real data VS Pred ----
# La courbe et les points observés restent sur les vraies dates de pose.
# Les lignes pointillées montrent où tombent les dates BioSIM.

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

## 1. Regrouper les vraies données observées par date, par année

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


## Figure Survie cumulative (dates BioSIM) : survie / pheno.c en spaghetti plot ----

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
log_surv_draws_all_biosim

# Vérifications
# sum(is.infinite(log_surv_draws_all_biosim$log_surv))  # devrait être 0
# sum(log_surv_draws_all_biosim$surv_prod == 0)         # devrait être 0
# range(log_surv_draws_all_biosim$log_surv)             # étendue des valeurs (en log)

## pheno_num

surv_all_num_biosim <- log_surv_draws_all_biosim %>%
  mutate(
    pheno_num = case_when(
      pheno.c == "early" ~ -2,
      pheno.c == "peak"  ~ 0,
      pheno.c == "late"  ~ 2
    )
  )

## Sous-échantillon (~200 tirages par année) pour ne pas faire planter l'ordi
set.seed(123)
sample_draws_200_biosim <- surv_all_num_biosim %>%
  distinct(annee, drawid) %>%
  group_by(annee) %>%
  slice_sample(n = 200) %>%
  ungroup()

surv_subset_200_biosim <- surv_all_num_biosim %>%
  semi_join(sample_draws_200_biosim, by = c("annee", "drawid"))

cols_annee <- c("2025" = "#0072B2", "2026" = "#D55E00")

ggplot(surv_all_num_biosim, aes(x = pheno_num, y = surv_prod, color = annee, fill = annee)) +
  # group = annee + drawid : sinon un même drawid relie les deux années en zigzag
  geom_line(data = surv_subset_200_biosim, aes(group = interaction(annee, drawid)), alpha = 0.08) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), alpha = 0.2, linewidth = 1.1) +
  scale_color_manual(values = cols_annee) +
  scale_fill_manual(values = cols_annee) +
  scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("-2\nEarly", "-1", "0\nPeak", "1", "2\nLate")
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = scales::log_breaks(n = 5),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  annotation_logticks(sides = "l") +
  labs(
    x = "Scénario phénologique",
    y = "Survie cumulative moyenne",
    color = "Année", fill = "Année",
    caption = "Prédictions aux dates BioSIM"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

## Ici c'est la date BIOSIM au début de la période de pose, mais au milieu de la période d'exposition
# Pour avoir la date BIOSIM au milieu de la période d'exposition, nous devrions faire
# Date biosim en JJ - 3 jours. 

## Trouver Stade larvaire moyen avec BIOSIM (Average instar/parcelle) ----
# La variable est données directement par Biosim mais le df est très imposant.

Full_data_biosim <- read_csv("Average_instar.csv")
glimpse(Full_data_biosim)

# Moyenne des instars par dates

instar_moy_date <- Full_data_biosim %>%
  mutate(date = make_date(Year, as.integer(Month), as.integer(Day)),
         jj   = yday(date)) %>%
  group_by(annee = Year, date, jj) %>%
  summarise(instar_moy = mean(`Average instar(Sum)`, na.rm = TRUE),
            instar_sd  = sd(`Average instar(Sum)`, na.rm = TRUE), .groups = "drop")

instar_moy_date

#Ajouter information à data_nopupe

# Colonne d'instar moyen à la date de pose
ajoute_instar <- function(df) {
  df %>%
    mutate(annee_num = as.numeric(as.character(annee))) %>%
    left_join(instar_moy_date %>% select(annee, jj, instar_pose = instar_moy),
              by = c("annee_num" = "annee", "date_pose.c" = "jj")) %>%
    select(-annee_num)
}

data_nopupe      <- ajoute_instar(data_nopupe)
data_nopupe_2025 <- ajoute_instar(data_nopupe_2025)
data_nopupe_2026 <- ajoute_instar(data_nopupe_2026)


data_nopupe %>%
  filter(stade.c %in% 4:6) %>%
  group_by(annee, stade.c, pheno.c) %>%
  summarise(instar_pose = mean(instar_pose), n = n(), .groups = "drop") %>%
  print(n = 18)

#Date Médiane à la place car milieu de fenêtre d'exposition représente mieux 

# Interpolation linéaire de l'instar moyen par année : accepte les dates non entières
# (la pose de 7 jours de L5 tardif 2026 a une médiane à +3.5)
instar_a_date <- function(annee_num, jour) {
  d <- instar_moy_date %>% filter(annee == annee_num)
  approx(d$jj, d$instar_moy, xout = jour)$y
}

ajoute_instar_med <- function(df, decalage = 3) {
  df %>%
    mutate(annee_num = as.numeric(as.character(annee)),
           date_med.c = date_pose.c + decalage,
           instar_med = purrr::map2_dbl(annee_num, date_med.c, instar_a_date)) %>%
    select(-annee_num)
}

data_nopupe      <- ajoute_instar_med(data_nopupe)
data_nopupe_2025 <- ajoute_instar_med(data_nopupe_2025)
data_nopupe_2026 <- ajoute_instar_med(data_nopupe_2026)

# Contrôles
sum(is.na(data_nopupe$instar_med))       # doit être 0
nrow(data_nopupe)                        # inchangé

data_nopupe %>%
  filter(stade.c %in% 4:6) %>%
  group_by(annee, stade.c, pheno.c) %>%
  summarise(instar_pose = mean(instar_pose), instar_med = mean(instar_med), n = n(), .groups = "drop") %>%
  mutate(diff = instar_med - instar_pose) %>%
  print(n = 18)

# Figure comparaison stade larvaire vs JJ ----

ggplot(instar_moy_date, aes(jj, instar_moy, color = factor(annee))) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = c(141, 150, 157), linetype = "dashed") +   # exemple : poses de L4 en 2025
  coord_cartesian(xlim = c(100, 220)) +
  labs(x = "Jour de l'année", y = "Stade moyen (instar)", color = "Année") +
  theme_minimal()
#Relativement semblable entre années?

## Figure comparaison stade larvaire BIOSIM vs Placement

# Dates BIOSIM moyennes par année, stade et scénario (biosim_moy vient du bloc BIOSIM, C59 recodé en C49)
bio <- biosim_moy %>%
  filter(stade != "Pupae") %>%
  mutate(stade.c = as.numeric(sub("L", "", stade)),
         pheno   = as.character(scenario)) %>%        # early, peak, late
  select(annee, stade.c, pheno, biosim = jour_moy)

# Instar moyen à une date donnée (interpolé, accepte les dates non entières)

instar_a_date <- function(annee_num, jour) {
  d <- instar_moy_date %>% filter(annee == annee_num)
  approx(d$jj, d$instar_moy, xout = jour)$y
}

fig_dat <- grille_pose %>%                            # dates de pose réelles (18 lignes)
  mutate(stage   = stade_date,
         stade.c = as.numeric(sub("L", "", stade_date)),
         pheno   = as.character(pheno.c)) %>%
  left_join(bio, by = c("annee", "stade.c", "pheno")) %>%
  mutate(scenario      = factor(pheno, levels = c("early", "peak", "late"),
                                labels = c("Early", "Peak", "Late")),
         instar_pose   = map2_dbl(annee, date_pose.c + 3, instar_a_date),   # date médiane
         instar_biosim = map2_dbl(annee, biosim, instar_a_date))

long <- fig_dat %>%
  select(annee, stage, scenario, Placement = instar_pose, BIOSIM = instar_biosim) %>%
  tidyr::pivot_longer(c(Placement, BIOSIM), names_to = "source", values_to = "instar")

cols <- c("Placement" = "#0072B2", "BIOSIM" = "#D55E00")

fig_instar <- ggplot() +
  geom_hline(data = fig_dat, aes(yintercept = stade.c), linetype = "dotted", color = "grey60") +
  geom_segment(data = fig_dat,
               aes(x = scenario, xend = scenario, y = instar_pose, yend = instar_biosim),
               color = "grey50", linewidth = 0.8) +
  geom_point(data = long, aes(scenario, instar, color = source), size = 3.5) +
  scale_color_manual(values = cols, name = NULL) +
  facet_grid(annee ~ stage) +
  labs(x = NULL, y = "Average instar") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.border = element_rect(color = "grey70", fill = NA),
        legend.position = "bottom")

fig_instar
#dir.create("figures", showWarnings = FALSE)
#ggsave("figures/instar_placement_vs_biosim.png", fig_instar, width = 9, height = 5.5, dpi = 300)


## Fig avec dates normales (pas médianes) ----

# Objets nécessaires : grille_pose, biosim_moy, instar_moy_date
instar_a_date <- function(annee_num, jour) {
  d <- instar_moy_date %>% filter(annee == annee_num)
  approx(d$jj, d$instar_moy, xout = jour)$y
}

bio <- biosim_moy %>%
  filter(stade != "Pupae") %>%
  mutate(stade.c = as.numeric(sub("L", "", stade)),
         pheno   = as.character(scenario)) %>%
  select(annee, stade.c, pheno, biosim = jour_moy)

fig_dat <- grille_pose %>%
  mutate(stage   = stade_date,
         stade.c = as.numeric(sub("L", "", stade_date)),
         pheno   = as.character(pheno.c)) %>%
  left_join(bio, by = c("annee", "stade.c", "pheno")) %>%
  mutate(scenario      = factor(pheno, levels = c("early", "peak", "late"),
                                labels = c("Early", "Peak", "Late")),
         instar_debut  = map2_dbl(annee, date_pose.c,     instar_a_date),   # début de l'exposition
         instar_fin    = map2_dbl(annee, date_pose.c + 6, instar_a_date),   # fin de l'exposition (6 jours)
         instar_biosim = map2_dbl(annee, biosim,          instar_a_date))

# Contrôles
nrow(fig_dat)                                          # 18
sum(is.na(fig_dat$instar_fin)); sum(is.na(fig_dat$instar_biosim))   # 0 et 0

fig_fenetre <- ggplot(fig_dat, aes(x = scenario)) +
  geom_hline(aes(yintercept = stade.c), linetype = "dotted", color = "grey60") +
  geom_linerange(aes(ymin = instar_debut, ymax = instar_fin),
                 color = "#0072B2", linewidth = 5, alpha = 0.5) +
  geom_point(aes(y = instar_biosim), color = "#D55E00", size = 3.5) +
  facet_grid(annee ~ stage) +
  labs(x = NULL, y = "Average instar",
       caption = "Blue bar: instar over the 6-day exposure; orange point: BIOSIM date") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.border = element_rect(color = "grey70", fill = NA))

fig_fenetre

## Test controle

dec_tab <- fig_dat %>%
  mutate(d_pose = date_pose.c - biosim,
         d_med  = date_pose.c + 3 - biosim,
         i_pose = map2_dbl(annee, date_pose.c,     instar_a_date),
         i_med  = map2_dbl(annee, date_pose.c + 3, instar_a_date))

dec_tab %>% group_by(annee) %>%
  summarise(jours_pose = mean(abs(d_pose)), jours_med = mean(abs(d_med)),
            instar_pose = mean(abs(i_pose - instar_biosim)),
            instar_med  = mean(abs(i_med  - instar_biosim)),
            decalage_moyen = mean(d_pose), .groups = "drop")


## 2025 date biosim au centre de la période d'exposition
## 2026 date biosim beaucoup plus proche du début de la période qu'au centre
## Test avec un décalage différent. Le stade larvaire semble bon pour visualiser 
# les écarts avec les dates de pose.

#Tableau contrôle qui confirme quel date BIOSIM utilisé

## Dates BIOSIM moyennes (84 parcelles) et deux écarts-types par année et stade

dist2 <- biosim_moy %>%
  filter(stade != "Pupae") %>%
  transmute(annee, stade_date = as.character(stade), scenario, jour_moy) %>%
  pivot_wider(names_from = scenario, values_from = jour_moy) %>%      # colonnes early, peak, late
  mutate(mu    = peak,
         sig_g = (peak - early) / qnorm(0.95),
         sig_d = (late - peak)  / qnorm(0.95))

## Proportion cumulée d'individus ayant atteint le stade à la date x

pnorm_asym <- function(x, mu, sig_g, sig_d) pnorm((x - mu) / ifelse(x < mu, sig_g, sig_d))

## Plage des dates de pose observées

plage <- data_nopupe %>%
  mutate(annee = as.numeric(as.character(annee))) %>%
  group_by(annee) %>%
  summarise(dmin = min(date_pose.c), dmax = max(date_pose.c), .groups = "drop")

## Proportion couverte pendant la fenêtre de 6 jours, par stade, scénario et décalage
couv <- dist2 %>%
  pivot_longer(c(early, peak, late), names_to = "scenario", values_to = "biosim") %>%
  crossing(decalage = c(-3, 0)) %>%
  left_join(plage, by = "annee") %>%
  mutate(scenario = factor(scenario, levels = c("early", "peak", "late")),
         pose     = pmin(pmax(biosim + decalage, dmin), dmax),        # date réellement évaluée par le modèle
         plafonne = abs(pose - (biosim + decalage)) > 1e-9,           # date ramenée à la plage des poses
         fin_fen  = pose + 6,
         p_debut  = pnorm_asym(pose,    mu, sig_g, sig_d),
         p_fin    = pnorm_asym(fin_fen, mu, sig_g, sig_d),
         p_moy    = pmap_dbl(list(pose, fin_fen, mu, sig_g, sig_d),
                             function(a, b, m, g, d) mean(pnorm_asym(seq(a, b, length.out = 25), m, g, d))))

## Tableau (moyenne sur L4, L5 et L6, en %)

tableau_couverture <- couv %>%
  group_by(annee, decalage, scenario) %>%
  summarise(debut = round(100 * mean(p_debut), 1),
            fin = round(100 * mean(p_fin), 1),
            moyenne = round(100 * mean(p_moy), 1),
            n_plafonnes = sum(plafonne), .groups = "drop") %>%
  arrange(annee, decalage, scenario)

print(tableau_couverture, n = 12)

## Détail par stade/date réelles vs BIOSIM ----

couv %>%
  transmute(annee, stade_date, scenario, decalage, plafonne,
            debut = round(100 * p_debut, 1), fin = round(100 * p_fin, 1), moyenne = round(100 * p_moy, 1)) %>%
  arrange(annee, decalage, stade_date, scenario) %>% print(n = 36)

## Refaire analyses avec date BIOSIM centrés, représenté par Décalage = -3

DECALAGE <- -3      # exposition de 6 jours centrée sur la date BIOSIM (date de pose = date BIOSIM - 3)

## 1. Dates BIOSIM moyennes (84 parcelles) par année, stade et scénario
grille_biosim <- biosim_moy %>%
  filter(stade != "Pupae") %>%
  transmute(annee, stage_date = as.character(stade),
            pheno.c = factor(as.character(scenario), levels = c("early", "peak", "late")),
            biosim = jour_moy)

## 2. Plage des dates de pose observées (le lissage n'est pas contraint au-delà)
plage <- data_nopupe %>% mutate(annee = as.numeric(as.character(annee))) %>%
  group_by(annee) %>% summarise(dmin = min(date_pose.c), dmax = max(date_pose.c), .groups = "drop")

## 3. Grilles de prédiction (mêmes noms qu'avant)
fabrique_prediction_data <- function(annee_lab) {
  p <- plage %>% filter(annee == annee_lab)
  grille_biosim %>% filter(annee == annee_lab) %>%
    mutate(date_demandee = biosim + DECALAGE,
           date_pose.c   = pmin(pmax(date_demandee, p$dmin), p$dmax),
           feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
           annee    = factor(as.character(annee_lab), levels = levels(data_nopupe$annee)),
           parcelle = as.character(na.omit(data_nopupe$parcelle)[1]))   # sans effet avec re_formula = NA
}
prediction_data_2025 <- fabrique_prediction_data(2025)
prediction_data_2026 <- fabrique_prediction_data(2026)

# Contrôle : dates utilisées, et celles ramenées à la plage des poses
bind_rows(prediction_data_2025, prediction_data_2026) %>%
  mutate(plafonnee = date_pose.c != date_demandee) %>%
  select(annee, stage_date, pheno.c, biosim, date_demandee, date_pose.c, plafonnee) %>%
  arrange(annee, stage_date, pheno.c) %>% print(n = 18)

## 4. Prédictions (modèle principal)
preds_draws_2025 <- get_draws(predictions(ptoid_main_interaction_3, newdata = prediction_data_2025,
                                          re_formula = NA, type = "response"))
preds_draws_2026 <- get_draws(predictions(ptoid_main_interaction_3, newdata = prediction_data_2026,
                                          re_formula = NA, type = "response"))

## 5. Ton code, inchangé
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

# Contrôle : doit retrouver 0.200 / 0.111 / 0.095 (2025) et 0.442 / 0.452 / 0.297 (2026)

surv_all_num %>% group_by(annee, pheno.c) %>% summarise(moy = mean(surv_prod), .groups = "drop")

set.seed(123)
sample_draws_200 <- surv_all_num %>%
  distinct(annee, drawid) %>%
  group_by(annee) %>%
  slice_sample(n = 200) %>%
  ungroup()

surv_subset_200 <- surv_all_num %>%
  semi_join(sample_draws_200, by = c("annee", "drawid"))

## 6. Parabole lisse par tirage, passant par les trois scénarios

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

## 7. Figure survie BIOSIM -3 ----
cols_annee <- c("2025" = "#0072B2", "2026" = "#D55E00")

fig_survie_biosim <- ggplot() +
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

fig_survie_biosim

# Analyses avec couvert forestier ----

## Contrôle : prediction_data_* doivent contenir les dates BIOSIM -3
#    (L4 en 2025 : 144, 149, 156 ; L4 en 2026 : 147, 152, 158)
bind_rows(prediction_data_2025, prediction_data_2026) %>%
  select(annee, stage_date, pheno.c, date_pose.c) %>%
  arrange(annee, stage_date, pheno.c) %>% print(n = 18)

## Fonction : beta ET gamma, séquence continue de feuillus, re_formula = NA

compute_gradients_NA_simple <- function(grille_dates, range_feuillus, modele, annee_label,
                                        n_points = 30, ndraws = 2000) {
  
  feuillus_grid <- seq(range_feuillus[1], range_feuillus[2], length.out = n_points)
  
  prediction_data <- map_dfr(feuillus_grid, function(f) grille_dates %>% mutate(feuillus = f)) %>%
    mutate(
      annee    = factor(annee_label, levels = levels(data_nopupe$annee)),
      parcelle = as.character(na.omit(data_nopupe$parcelle)[1])   # sans effet avec re_formula = NA
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

## Gradients selon le % de feuillus, BIOSIM -3

gradients_int_2025_biosim_m3 <- compute_gradients_NA_simple(
  prediction_data_2025, range(data_nopupe_2025$feuillus, na.rm = TRUE),
  ptoid_main_interaction_3, annee_label = "2025"
)

gradients_int_2026_biosim_m3 <- compute_gradients_NA_simple(
  prediction_data_2026, range(data_nopupe_2026$feuillus, na.rm = TRUE),
  ptoid_main_interaction_3, annee_label = "2026"
)

gradients_interaction_biosim_m3 <- bind_rows(gradients_int_2025_biosim_m3, gradients_int_2026_biosim_m3) %>%
  mutate(gradient = factor(gradient, levels = c("beta", "gamma"),
                           labels = c("β (directionnel)", "γ (quadratique)")))

saveRDS(gradients_interaction_biosim_m3, "gradients_interaction_biosim_m3.rds")

## 3. Figure β et γ ----

cols_annee <- c("2025" = "#0072B2", "2026" = "#D55E00")

fig_gradients_feuillus_biosim_m3 <- ggplot(gradients_interaction_biosim_m3,
                                           aes(x = feuillus, y = mean, color = annee, fill = annee)) +
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

fig_gradients_feuillus_biosim_m3

## 4. Figure β seulement ----

fig_beta_feuillus_biosim_m3 <- ggplot(gradients_interaction_biosim_m3 %>% filter(gradient == "β (directionnel)"),
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

fig_beta_feuillus_biosim_m3

