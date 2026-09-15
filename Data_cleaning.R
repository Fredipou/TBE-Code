##### Importing and cleaning data

## Install and Load packages ----

#install.packages("haven")
#install.packages("emmeans")
#install.packages("ggokabeito")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("cowplot")
#install.packages("lmertest")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("ggeffects")
#install.packages("lme4")
#install.packages("collapse", type = "binary")
#install.packages("glmmTMB")
#install.packages("marginaleffects")
#install.packages("brms")
#install.packages("tidybayes")
#install.packages("bayesplot")
#install.packages("tidyr")
#install.packages("visreg")
#install.packages("mgcv")
#install.packages("gratia")
#install.packages("igraph")
#install.packages("ggraph")
#installed.packages("tidygraph")
#install.packages("visNetwork")
#install.packages("plotly")
#install.packages("ggdist")
#install.packages("plotly")
# install.packages("rayshader")
#install.packages("rayshader")
#install.packages("MuMIn")
#install.packages("MetBrewer")
#install.packages("viridis")
library(viridis)
library(MetBrewer)
library(haven)
library(MuMIn)
library(rayshader)
library(plotly)
library(ggdist)
library(plotly)
library(visNetwork)
library(shiny)
library(ggraph)
library(tidygraph)
library(igraph)
library(gratia)
library(mgcv)
library(visreg)
library(tidyr)
library(bayesplot)
library(tidybayes)
library(collapse)
library(brms)
library(marginaleffects)
library(lme4)
library(glmmTMB)
library(ggeffects)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(lmerTest)
library(ggokabeito)
library(emmeans)

## Det plot theme
theme_set(theme_cowplot()) 

# Importing data in R ----

Carac_arbre <- read_excel("Carac.xlsx", sheet = 1)
#view(Carac_arbre)

Recolte_2025 <- read_excel("Recolte_TBE.xlsx")
#View(Recolte_TBE)

Carac_arbuste <- read_excel("Carac.xlsx", sheet = 2)
#view(Carac_arbuste)

Biomasse <- read_excel("biomasse_2026.xlsx")
#view(Biomasse)

Recolte_2026 <- read_excel("Recolte_TBE.xlsx", sheet = 2)
#View(Recolte_2026)

rain_2025 <- read.csv("meteo_megantic.csv", skip = 4)
rain_2026 <- read.csv("meteo_megantic_2026.csv", skip = 4)

#### Function to change Counting data into relative abundance ----

normalize_row <- function(row) {
  id <- row[1]
  values <- as.numeric(row[-1])
  total <- sum(values[values != 0])
  norm_values <- ifelse(values == 0, 0, (values / total)*100)
  return(c(id, norm_values))
}

Carac_normal <- as.data.frame(t(apply(Carac_arbre, 1, normalize_row)))
colnames(Carac_normal) <- colnames(Carac_arbre)
Carac_normal[, -1] <- lapply(Carac_normal[, -1], as.numeric)
#print(Carac_normal)

# Cleaning some variables
Carac_normal_clean <- Carac_normal %>%
  mutate(parcelle = str_replace_all(parcelle, "-", ""))

#Adding categorical variables for later analysis

Type_foret_1 <- Carac_normal_clean %>%
  mutate(
    conifere = ABBA + PIMA + PIGL,
    feuillus = BEPA + BEAL + ACSA + ACPE + SODE + PODE + FAGR
  ) #%>%
#select(-ABBA, -PIMA, -PIGL, -BEPA, -BEAL, -ACSU, -ACPE, -SODE, -PODE, -FAGR)

Type_foret <- Type_foret_1 %>%
  mutate(
    categorie = case_when(
      feuillus > 70 ~ "Dominance_Feuillus",
      conifere > 70 ~ "Dominance_Conifere",
      TRUE          ~ "Mixte"
    )
  )

# Adding diversity variables for later analysis ----

arbre_arbuste = left_join(Carac_arbre, Carac_arbuste, by = "parcelle")
arbre_arbuste$parcelle <- gsub("-", "", arbre_arbuste$parcelle)
arbre_arbuste <- arbre_arbuste %>%
mutate(div = rowSums(across(-parcelle, ~ .x > 0)))

## Adding Biomass data from 2026 for later analysis ----

# mean DHP

Biomasse <- Biomasse %>%
  mutate(DHP = as.numeric(DHP))

DHP_moyen <- Biomasse %>%
  group_by(parcelle, species) %>%
  summarise(
    DHP_qmd = sqrt(mean(DHP^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    DHP_qmd = ifelse(is.nan(DHP_qmd), NA_real_, DHP_qmd),
    species = paste0(species, "_DHP")
  )

DHP_large <- DHP_moyen %>%
  pivot_wider(
    id_cols = parcelle,
    names_from = species,
    values_from = DHP_qmd
  )

## Joining carac, DHP and Larvae survival data

Foret_2025 <- Recolte_2025 %>%
  left_join(Type_foret, by = "parcelle")

Foret_2025 <- Foret_2025 %>%
  mutate(pres_ptoid = as.numeric(pres_ptoid))

Foret_2026 <- Recolte_2026 %>%
  left_join(Type_foret, by = "parcelle")

Foret_2026 <- Foret_2026 %>%
  mutate(pres_ptoid = as.numeric(pres_ptoid))

Recolte = bind_rows(Foret_2025,Foret_2026)

Recolte_foret <- Recolte %>%
  left_join(DHP_large, by = "parcelle")

### Clean data set ----

### Add Pheno, stade, and date as a continuous variable

Recolte_foret <- Recolte_foret %>%
  mutate(pheno.c = ifelse(pheno == "E", -1,
                          ifelse(pheno == "P",  0,
                                 ifelse(pheno == "L", 1, NA))))
Recolte_foret <- Recolte_foret %>%
  mutate(stade.c = ifelse(stade == "L4", 4,
                          ifelse(stade == "L5",  5,
                                 ifelse(stade == "L6", 6,
                                        ifelse(stade == "Pupae", 7, NA)))))

Recolte_foret <- Recolte_foret %>%
  mutate(date_pose.c = yday(date_pose))

Recolte_foret <- Recolte_foret %>%
  mutate(date_recolte.c = yday(date_recolte))

### Cleaning survival and parasitism variable to not include case where larvae dies
### before either completing life cycle or ptoid emergence (no longer usefull,
### since DNA testing has confirmed presence or absence even when 0-0)

Recolte_foret <- Recolte_foret %>%
  mutate(pres_ptoid_clean = ifelse(survie_larve == 0 & 
                                   pres_ptoid == 0, 
                                   NA, pres_ptoid))
Recolte_foret <- Recolte_foret %>%
  mutate(survie_clean = ifelse(survie_larve == 0 & 
                                     pres_ptoid == 0, 
                                   NA, survie_larve))
#view(Recolte_foret)

### Importing ptoid/host traits dataset ----

Ptoid_traits <- read_excel("Traits_ptoid.xlsx", sheet = 1)
Ptoid_traits_long <- read_excel("Traits_ptoid.xlsx", sheet = 2)
Ptoid_host <- read_excel("Traits_ptoid.xlsx", sheet = 3)
Lepidop_host <- read_excel("Traits_ptoid.xlsx", sheet = 4)

# Change NA for 0 in Ptoid_host and Lepidop_host, and data cleaning ----

Ptoid_host <- Ptoid_host %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         across(where(is.character), ~replace_na(.x, "0")))
Ptoid_host <- Ptoid_host[-nrow(Ptoid_host), ]

Ptoid_host[-1] <- lapply(Ptoid_host[-1], function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  x_num[is.na(x_num)] <- 0
  x_num
})

Lepidop_host <- Lepidop_host %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         across(where(is.character), ~replace_na(.x, "0")))
Lepidop_host <- Lepidop_host[, -ncol(Lepidop_host)]
Lepidop_host[-1] <- lapply(Lepidop_host[-1], function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  x_num[is.na(x_num)] <- 0
  x_num
})

## Importing BIOSIM data in R ----

# First tried with analysis in BIOSIM, wasn't sure of the results, confirmed by hand
# that the results I got didn't fit what I needed, so do it in R with the 
# entire dataset

#BIOSIM_2025_EARLY = read_csv("TBE_2025_95.csv")
#BIOSIM_2025_PEAK = read.csv("TBE_2025_50.csv")
#BIOSIM_2025_LATE = read.csv("TBE_2025_5.csv")

#Drawback is this is a really big dataset

#BIOSIM_FULL = read.csv("FULL_DATA_2025_2026.csv")

## Transforming into DOY 

BIOSIM_FULL <- BIOSIM_FULL %>%
  mutate(
    date = make_date(Year, Month, Day),
    DOY = yday(date)
  )

#Df for Estimating real date for 2025 and each phenological scenario

BIOSIM_2025 <- BIOSIM_FULL %>%
  filter(Year == 2025)

#Create function to get specific quantile (0.05, 0.5, 0.95) ----

get_doy_pheno <- function(data, stage_larve, pheno) {
  data %>%
    arrange(date) %>% 
    mutate(
      cum = cumsum(.data[[stage_larve]]),
      pct = cum / sum(.data[[stage_larve]], na.rm = TRUE)
    ) %>%
    filter(pct >= pheno) %>%
    slice(1) %>%
    pull(DOY)
}

# Apply function

stage_larve <- c("L4", "L5", "L6", "Pupae")
pheno <- c(EARLY = 0.05, PEAK = 0.50, LATE = 0.95)

PHENO_2025 <- expand.grid(stage_larve = stage_larve,
                          stage = names(pheno)) %>%
  rowwise() %>%
  mutate(
    DOY = get_doy_pheno(
      BIOSIM_2025,
      as.character(stage_larve),
      pheno[as.character(stage)]
    )
  ) %>%
  ungroup()

## Same thing but by parcels

PHENO_ALL <- BIOSIM_FULL %>%
  group_by(Year, Name) %>%
  group_modify(~ {
    
    data_parcelle <- .x %>% arrange(date)
    
    expand_grid(stage_larve = stage_larve,
                stage = names(pheno)) %>%
      mutate(
        DOY = map2_dbl(stage_larve, stage,
                       ~ get_doy_pheno(data_parcelle, .x, pheno[.y]))
      )
  }) %>%
  ungroup()

## Predicted date for 2026 ----

BIOSIM_2026 = BIOSIM_FULL %>%
  filter(Year == 2026)

# Apply function

PHENO_2026 <- expand.grid(stage_larve = stage_larve,
                          stage = names(pheno)) %>%
  rowwise() %>%
  mutate(
    DOY = get_doy_pheno(
      BIOSIM_2026,
      as.character(stage_larve),
      pheno[as.character(stage)]
    )
  ) %>%
  ungroup()

#EARLY_L4 <- BIOSIM_2025 %>%
#  arrange(date) %>%
#  mutate(
#    cum_L4 = cumsum(L4),
#    pct_L4 = cum_L4 / sum(L4, na.rm = TRUE)
#  ) %>%
#  filter(pct_L4 >= 0.05) %>%
#  slice(1) %>%
#  pull(DOY)


## DHP * Adundance for biomass/site
# With mean DHP/species and general site composition in %

especes <- c(
  "ABBA", "PIMA", "BEPA", "BEAL",
  "ACSA", "ACPE", "SODE", "FAGR"
)

biomasse_index <- Carac_arbre %>%
  left_join(DHP_large, by = "parcelle") %>%
  rowwise() %>%
  mutate(
    biomasse_index = sum(
      c_across(all_of(especes)) *
        c_across(all_of(paste0(especes, "_DHP")))^2,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(parcelle, biomasse_index)

Recolte_foret <- Recolte_foret %>%
  left_join(biomasse_index, by = "parcelle")
# Higher biomass_index = more tree or bigger tree in a site (or both)

## Seprate 2025 and 2026 for later analysis

Recolte_foret_2025 <- Recolte_foret %>%
  filter(substr(date_pose, 1, 4) == "2025")

Recolte_foret_2026 <- Recolte_foret %>%
  filter(substr(date_pose, 1, 4) == "2026")

### Now object substracting stade Pupae from the data
data_nopupe <- Recolte_foret[Recolte_foret$stade.c != 7, ]
data_justpupe = Recolte_foret[Recolte_foret$stade.c == 7,]

data_nopupe_2025 <- Recolte_foret_2025[Recolte_foret_2025$stade.c != 7, ]
data_nopupe_2026 <- Recolte_foret_2026[Recolte_foret_2026$stade.c != 7, ]

data_nopupe$date_pose <- as.Date(data_nopupe$date_pose)
data_nopupe$annee <- factor(format(data_nopupe$date_pose, "%Y"))
data_nopupe$jour_saison <- as.numeric(format(data_nopupe$date_pose, "%j"))

data_nopupe <- data_nopupe %>%
  mutate(annee = factor(year(date_pose)))

data_nopupe_2025 <- data_nopupe_2025 %>%
  mutate(annee = factor(year(date_pose)))
         
data_nopupe_2026 <- data_nopupe_2026 %>%
  mutate(annee = factor(year(date_pose)))

  ## Adding rain as a variable ----

rain_2025 <- rain_2025 %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"),
         jour_julien = yday(DATE))

rain_2026 <- rain_2026 %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"),
         jour_julien = yday(DATE))

rain_all <- bind_rows(rain_2025, rain_2026) %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>%
  distinct(DATE, .keep_all = TRUE)

precip_par_periode <- Recolte_foret %>%
  mutate(
    date_pose = as.Date(date_pose, format = "%Y-%m-%d"),
    date_recolte = as.Date(date_recolte, format = "%Y-%m-%d"),
    row_id = row_number()
  ) %>%
  rowwise() %>%
  mutate(
    precip_cumulee = sum(
      rain_all$PRECIP_TOTAL_DAY_MM[rain_all$DATE >= date_pose & rain_all$DATE <= date_recolte],
      na.rm = TRUE
    ),
    nb_jours = as.numeric(date_recolte - date_pose) + 1,
    precip_moyenne_jour = precip_cumulee / nb_jours
  ) %>%
  ungroup()



