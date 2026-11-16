## Analyses on ptoid species richness by time and space

source("Data_cleaning.R")

## Data cleaning

id_ptoid_data <- data_nopupe %>%
  mutate(taxon = case_when(
    id_ptoid == "Hymenoptère" ~ "Hymenoptera_autre",   # PCR négatif pour T. rostrale et Phytodietus
    id_ptoid == "Diptere"     ~ "Diptera_autre",
    TRUE ~ id_ptoid
  ))
data_nopupe %>% count(id_ptoid, sort = TRUE)

ptoid25 <- id_ptoid_data %>%
  mutate(taxon = na_if(taxon, "NA")) %>%
  filter(annee == "2025", pres_ptoid == 1, !is.na(taxon)) %>%
  mutate(parcelle = as.character(parcelle))

## Test observatoire
#Proportion de Tranosema (espèce dominante) par feuillus

ptoid25 %>%
  left_join(classes, by = c("parcelle", "feuillus")) %>%
  group_by(classe) %>%
  summarise(
    n             = n(),
    n_tranosema   = sum(taxon == "Tranosema rostrale"),
    n_phyto       = sum(taxon == "Phytodietus sp."),
    n_autres      = sum(!taxon %in% c("Tranosema rostrale", "Phytodietus sp.")),
    prop_tranosema = n_tranosema / n,
    prop_phyto     = n_phyto / n,
    prop_autres    = n_autres / n
  )

## Test avec vegan

#mat <- ptoid25 %>% left_join(classes, by = c("parcelle", "feuillus")) %>%
#  count(classe, taxon) %>%
#  pivot_wider(names_from = taxon, values_from = n, values_fill = 0) %>%
#  tibble::column_to_rownames("classe")

#rowSums(mat)                          # individus par classe
#specnumber(mat)                       # richesse observée
#rarefy(mat, sample = min(rowSums(mat)))   # richesse raréfiée au plus petit effectif
#diversity(mat, "shannon")

# Sans les deux groupes non résolus
#mat3 <- mat[, !names(mat) %in% c("Diptera_autre", "Hymenoptera_autre")]
#rowSums(mat3); specnumber(mat3); rarefy(mat3, min(rowSums(mat3)))

#install.packages("iNEXT")
library(iNEXT)

out <- iNEXT(t(mat3), q = c(0, 1), datatype = "abundance")
ggiNEXT(out, type = 1, facet.var = "Order.q")

# aucune différence entre les trois classes de feuillus?

d25 <- id_ptoid_data %>%
  mutate(taxon = na_if(taxon, "NA")) %>%
  filter(annee == "2025", !is.na(pres_ptoid)) %>%
  filter(pres_ptoid == 0 | !is.na(taxon)) %>%     # exclut les parasitées non identifiées
  mutate(tranosema   = as.integer(pres_ptoid == 1 & taxon == "Tranosema rostrale"),
         phytodietus = as.integer(pres_ptoid == 1 & taxon == "Phytodietus sp."))

ctrl <- list(adapt_delta = 0.999, max_treedepth = 12)

m_tr <- brm(tranosema ~ feuillus + s(date_pose.c, k = 5) + (1 | parcelle),
            family = bernoulli(), data = d25,
            chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123, control = ctrl)

m_ph <- brm(phytodietus ~ feuillus + s(date_pose.c, k = 5) + (1 | parcelle),
            family = bernoulli(), data = d25,
            chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123, control = ctrl)

#saveRDS(m_tr, "m_tranosema_2025.rds"); saveRDS(m_ph, "m_phytodietus_2025.rds")

## Modèle avec les deux espèces

long <- d25 %>%
  mutate(id_larve = row_number()) %>%                  
  pivot_longer(c(tranosema, phytodietus), names_to = "espece", values_to = "y") %>%
  mutate(espece = factor(espece, levels = c("tranosema", "phytodietus")))

m_joint <- brm(
  y ~ espece * feuillus + s(date_pose.c, by = espece, k = 5) +
    (1 | parcelle) + (1 | id_larve),
  family = bernoulli(), data = long,
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123, control = ctrl
)

#saveRDS(m_joint, "m_joint_2025.rds")
readRDS("m_joint.RDS")
summary(m_joint)

## Dans le temps

long <- long %>%
  mutate(feuillus_10 = (feuillus - mean(feuillus)) / 10,
         date_10     = (date_pose.c - mean(date_pose.c)) / 10)

m_joint_t <- brm(
  y ~ espece * feuillus_10 +
    s(date_pose.c, by = espece, k = 5) +
    feuillus_10:date_10:espece +
    (1 | parcelle) + (1 | id_larve),
  family = bernoulli(), data = long,
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.999, max_treedepth = 12)
)
#saveRDS(m_joint_t, "m_joint_temps_2025.rds")
summary(m_joint_t)

##Figure
cols_taxon <- c("Tranosema rostrale" = "#0072B2", "Phytodietus sp." = "#D55E00")

## 1. Prédiction marginale : moyenne sur les larves réelles de d25 (toutes dates, toutes parcelles),
##    avec feuillus fixé à f. Comparable aux proportions observées.

fe <- seq(min(d25$feuillus), max(d25$feuillus), length.out = 40)

marg <- function(m, nom, ndraws = 500) {
  idx <- sample(ndraws(m), ndraws)                  
  res <- lapply(fe, function(f) {
    nd <- d25 %>% mutate(feuillus = f)
    ep <- posterior_epred(m, newdata = nd, re_formula = NULL, draw_ids = idx)
    p  <- rowMeans(ep)                                
    tibble(feuillus = f, mean = mean(p),
           lwr = quantile(p, 0.025), upr = quantile(p, 0.975))
  })
  bind_rows(res) %>% mutate(espece = nom)
}

set.seed(123)
pr <- bind_rows(marg(m_tr, "Tranosema rostrale"),
                marg(m_ph, "Phytodietus sp."))

saveRDS(pr, "pred_marginale_especes_2025.rds")         

## 2. Observations : proportion par classe de feuillus 
obs <- d25 %>%
  mutate(cl = cut_number(feuillus, 6)) %>%
  group_by(cl) %>%
  summarise(feuillus = mean(feuillus), n = n(),
            `Tranosema rostrale` = mean(tranosema),
            `Phytodietus sp.`    = mean(phytodietus), .groups = "drop") %>%
  pivot_longer(c(`Tranosema rostrale`, `Phytodietus sp.`),
               names_to = "espece", values_to = "prop")

## 3. Figure

fig_sp <- ggplot(pr, aes(feuillus, mean, color = espece, fill = espece)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_point(data = obs, aes(y = prop, size = n), alpha = 0.8, show.legend = FALSE) +
  scale_color_manual(values = cols_taxon) +
  scale_fill_manual(values = cols_taxon) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(x = "Feuillus (%)",
       y = "Probabilité de parasitisme par l'espèce",
       color = NULL, fill = NULL) +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom")

fig_sp
dir.create("figures", showWarnings = FALSE)
ggsave("figures/parasitisme_par_espece_feuillus_2025.png", fig_sp, width = 9, height = 5.5, dpi = 300)

## Essayons de partiotionner l'effet de phytodietus sur la sélection

data_sans_ph <- data_nopupe %>%
  mutate(id_ptoid = na_if(id_ptoid, "NA"),
         annee_num = as.numeric(as.character(annee)),
         ph = pres_ptoid == 1 & id_ptoid == "Phytodietus sp.") %>%
  filter(!(annee_num == 2025 & ph %in% TRUE))

nrow(data_nopupe) - nrow(data_sans_ph)

readRDS("ptoid_sans_ph.RDS")

ptoid_sans_ph <- brm(
  pres_ptoid ~ feuillus * annee +
    s(date_pose.c, by = annee, k = 12) +
    (1 | parcelle:annee),
  family = bernoulli(link = "logit"),
  data = data_sans_ph,
  chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

#saveRDS(ptoid_sans_ph, "ptoid_sans_phytodietus.rds")
summary(ptoid_sans_ph)

#Intéressant, feuillus n'est plus signif (comme attendu)

grille_pose <- data_nopupe %>%
  filter(stade.c %in% 4:6) %>%
  mutate(annee = as.numeric(as.character(annee))) %>%
  group_by(annee, stade.c, pheno.c) %>%
  summarise(date_pose.c = first(date_pose.c), n_dates = n_distinct(date_pose.c),
            .groups = "drop") %>%
  mutate(stade_date = paste0("L", stade.c),
         pheno.c = factor(case_when(sign(pheno.c) < 0  ~ "early",
                                    sign(pheno.c) == 0 ~ "peak",
                                    TRUE               ~ "late"),
                          levels = c("early", "peak", "late"))) %>%
  select(annee, stade_date, pheno.c, date_pose.c, n_dates)

#Selection sans phyto
calc_sel <- function(grille, annee_lab, modele = ptoid_main_interaction_3) {
  nd <- grille %>% filter(annee == annee_lab) %>%
    mutate(feuillus = mean(data_nopupe$feuillus, na.rm = TRUE),
           annee    = factor(as.character(annee_lab), levels = levels(data_nopupe$annee)),
           parcelle = as.character(na.omit(data_nopupe$parcelle)[1]))   # sans effet avec re_formula = NA
  surv <- 1 - posterior_epred(modele, newdata = nd, re_formula = NA)
  logW <- sapply(c("early", "peak", "late"),
                 function(s) rowSums(log(surv[, nd$pheno.c == s, drop = FALSE])))
  tibble(beta  = (logW[, "late"] - logW[, "early"]) / 4,
         gamma = (logW[, "late"] + logW[, "early"] - 2 * logW[, "peak"]) / 4,
         rr_early = exp(logW[, "early"] - logW[, "peak"]),
         rr_late  = exp(logW[, "late"]  - logW[, "peak"])) %>%
    pivot_longer(everything(), names_to = "quantite") %>%
    group_by(quantite) %>%
    summarise(mean = mean(value), lwr = quantile(value, .025),
              upr = quantile(value, .975), .groups = "drop")
}

res_cmp <- bind_rows(
  calc_sel(grille_pose, 2025, modele = ptoid_main_interaction_3) %>% mutate(modele = "Tous les parasitoïdes"),
  calc_sel(grille_pose, 2025, modele = ptoid_sans_ph)            %>% mutate(modele = "Sans Phytodietus")
)
res_cmp %>% arrange(quantite, modele) %>% print(n = 8)

## Avec feuillus maintenant

calc_sel_f <- function(grille, annee_lab, modele, f) {
  nd <- grille %>% filter(annee == annee_lab) %>%
    mutate(feuillus = f,
           annee    = factor(as.character(annee_lab), levels = levels(data_nopupe$annee)),
           parcelle = as.character(na.omit(data_nopupe$parcelle)[1]))
  surv <- 1 - posterior_epred(modele, newdata = nd, re_formula = NA)
  logW <- sapply(c("early", "peak", "late"),
                 function(s) rowSums(log(surv[, nd$pheno.c == s, drop = FALSE])))
  tibble(beta = (logW[, "late"] - logW[, "early"]) / 4,
         rr_early = exp(logW[, "early"] - logW[, "peak"]),
         rr_late  = exp(logW[, "late"]  - logW[, "peak"])) %>%
    pivot_longer(everything(), names_to = "quantite") %>%
    group_by(quantite) %>%
    summarise(mean = mean(value), lwr = quantile(value, .025), upr = quantile(value, .975), .groups = "drop")
}

bind_rows(lapply(c(10, 60), function(f)
  bind_rows(calc_sel_f(grille_pose, 2025, ptoid_main_interaction_3, f) %>% mutate(modele = "Tous"),
            calc_sel_f(grille_pose, 2025, ptoid_sans_ph, f)            %>% mutate(modele = "Sans Phytodietus")) %>%
    mutate(feuillus = f))) %>% arrange(quantite, feuillus, modele)

# Visuellement

# β et γ pour un modèle, une année, sur une séquence de feuillus
grad_feuillus <- function(modele, annee_lab, fe, draw_ids) {
  g <- grille_pose %>% filter(annee == annee_lab)
  map_dfr(fe, function(f) {
    nd <- g %>% mutate(feuillus = f,
                       annee    = factor(as.character(annee_lab), levels = levels(data_nopupe$annee)),
                       parcelle = as.character(na.omit(data_nopupe$parcelle)[1]))   # sans effet avec re_formula = NA
    surv <- 1 - posterior_epred(modele, newdata = nd, re_formula = NA, draw_ids = draw_ids)
    logW <- sapply(c("early", "peak", "late"),
                   function(s) rowSums(log(surv[, nd$pheno.c == s, drop = FALSE])))
    tibble(beta  = (logW[, "late"] - logW[, "early"]) / 4,
           gamma = (logW[, "late"] + logW[, "early"] - 2 * logW[, "peak"]) / 4) %>%
      pivot_longer(everything(), names_to = "gradient") %>%
      group_by(gradient) %>%
      summarise(mean = mean(value), lwr = quantile(value, .025), upr = quantile(value, .975),
                .groups = "drop") %>%
      mutate(feuillus = f)
  })
}

d25_f <- data_nopupe %>% filter(as.character(annee) == "2025")
fe <- seq(min(d25_f$feuillus, na.rm = TRUE), max(d25_f$feuillus, na.rm = TRUE), length.out = 30)

set.seed(123)
ids_tous <- sample(ndraws(ptoid_main_interaction_3), 1500)
ids_sans <- sample(ndraws(ptoid_sans_ph), 1500)

res_f <- bind_rows(
  grad_feuillus(ptoid_main_interaction_3, 2025, fe, ids_tous) %>% mutate(modele = "All parasitoids"),
  grad_feuillus(ptoid_sans_ph,            2025, fe, ids_sans) %>% mutate(modele = "Without Phytodietus")
) %>%
  mutate(gradient = factor(gradient, levels = c("beta", "gamma"),
                           labels = c("β (directional)", "γ (quadratic)")))
saveRDS(res_f, "gradients_feuillus_sans_phyto_2025.rds")

cols_mod <- c("All parasitoids" = "#0072B2", "Without Phytodietus" = "#009E73")

fig_sans_ph <- ggplot(res_f, aes(feuillus, mean, color = modele, fill = modele, linetype = modele)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = cols_mod) +
  scale_fill_manual(values = cols_mod) +
  scale_linetype_manual(values = c("All parasitoids" = "solid", "Without Phytodietus" = "longdash")) +
  facet_wrap(~ gradient, scales = "free_y") +
  labs(x = "% of deciduous trees", y = "Selection gradient", color = NULL, fill = NULL, linetype = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        legend.position = "bottom")

fig_sans_ph

## Même qu'avant mais sans phyto et tous les autres

data_tr <- data_nopupe %>%
  mutate(id_ptoid  = na_if(id_ptoid, "NA"),
         annee_num = as.numeric(as.character(annee)),
         autre_cause = pres_ptoid == 1 & (is.na(id_ptoid) | id_ptoid != "Tranosema rostrale")) %>%
  filter(!(annee_num == 2025 & autre_cause %in% TRUE)) %>%          
  filter(!(pres_ptoid == 0 & !is.na(id_ptoid)))                      

nrow(data_nopupe) - nrow(data_tr)


ptoid_tr_only <- brm(
  pres_ptoid ~ feuillus * annee +
    s(date_pose.c, by = annee, k = 12) +
    (1 | parcelle:annee),
  family = bernoulli(link = "logit"),
  data = data_tr,
  chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
saveRDS(ptoid_tr_only, "ptoid_tranosema_seul.rds")
summary(ptoid_tr_only)

## Analyse + figure
res_cmp3 <- bind_rows(
  calc_sel(grille_pose, 2025, modele = ptoid_main_interaction_3) %>% mutate(modele = "Tous les parasitoïdes"),
  calc_sel(grille_pose, 2025, modele = ptoid_sans_ph)            %>% mutate(modele = "Sans Phytodietus"),
  calc_sel(grille_pose, 2025, modele = ptoid_tr_only)            %>% mutate(modele = "Tranosema seul")
)
res_cmp3 %>% arrange(quantite, modele) %>% print(n = 12)
d25_f <- data_nopupe %>% filter(as.character(annee) == "2025")
fe <- seq(min(d25_f$feuillus, na.rm = TRUE), max(d25_f$feuillus, na.rm = TRUE), length.out = 30)

set.seed(123)
ids_tr <- sample(ndraws(ptoid_tr_only), 1500)


# β et γ pour un modèle, une année, sur une séquence de feuillus
grad_feuillus <- function(modele, annee_lab, fe, draw_ids) {
  g <- grille_pose %>% filter(annee == annee_lab)
  map_dfr(fe, function(f) {
    nd <- g %>% mutate(feuillus = f,
                       annee    = factor(as.character(annee_lab), levels = levels(data_nopupe$annee)),
                       parcelle = as.character(na.omit(data_nopupe$parcelle)[1]))   # sans effet avec re_formula = NA
    surv <- 1 - posterior_epred(modele, newdata = nd, re_formula = NA, draw_ids = draw_ids)
    logW <- sapply(c("early", "peak", "late"),
                   function(s) rowSums(log(surv[, nd$pheno.c == s, drop = FALSE])))
    tibble(beta  = (logW[, "late"] - logW[, "early"]) / 4,
           gamma = (logW[, "late"] + logW[, "early"] - 2 * logW[, "peak"]) / 4) %>%
      pivot_longer(everything(), names_to = "gradient") %>%
      group_by(gradient) %>%
      summarise(mean = mean(value), lwr = quantile(value, .025), upr = quantile(value, .975),
                .groups = "drop") %>%
      mutate(feuillus = f)
  })
}

d25_f <- data_nopupe %>% filter(as.character(annee) == "2025")
fe <- seq(min(d25_f$feuillus, na.rm = TRUE), max(d25_f$feuillus, na.rm = TRUE), length.out = 30)

set.seed(123)
ids_tous <- sample(ndraws(ptoid_main_interaction_3), 1500)
ids_sans <- sample(ndraws(ptoid_sans_ph), 1500)
ids_tr   <- sample(ndraws(ptoid_tr_only), 1500)

res_f3 <- bind_rows(
  grad_feuillus(ptoid_main_interaction_3, 2025, fe, ids_tous) %>% mutate(modele = "All parasitoids"),
  grad_feuillus(ptoid_sans_ph,            2025, fe, ids_sans) %>% mutate(modele = "Without Phytodietus"),
  grad_feuillus(ptoid_tr_only,            2025, fe, ids_tr)   %>% mutate(modele = "T. rostrale only")
) %>%
  mutate(gradient = factor(gradient, levels = c("beta", "gamma"),
                           labels = c("β (directional)", "γ (quadratic)")),
         modele = factor(modele, levels = c("All parasitoids", "Without Phytodietus", "T. rostrale only")))

saveRDS(res_f3, "gradients_feuillus_par_parasitoide_2025.rds")

cols3 <- c("All parasitoids" = "#0072B2", "Without Phytodietus" = "#009E73", "T. rostrale only" = "#D55E00")

fig3 <- ggplot(res_f3, aes(feuillus, mean, color = modele, fill = modele, linetype = modele)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = cols3) + scale_fill_manual(values = cols3) +
  scale_linetype_manual(values = c("All parasitoids" = "solid", "Without Phytodietus" = "longdash",
                                   "T. rostrale only" = "dotdash")) +
  facet_wrap(~ gradient, scales = "free_y") +
  labs(x = "% of deciduous trees", y = "Selection gradient", color = NULL, fill = NULL, linetype = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"), axis.ticks.length = unit(0.15, "cm"),
        legend.position = "bottom")

fig3
dir.create("figures", showWarnings = FALSE)
ggsave("figures/gradients_feuillus_par_parasitoide_2025.png", fig3, width = 10, height = 5, dpi = 300)
