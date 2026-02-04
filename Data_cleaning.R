#####

## Install and Load packages

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
#install.packages("glmmTMB")
#install.packages("marginaleffects")
#install.packages("brms")
#install.packages("tidybayes")
#install.packages("bayesplot")
#install.packages("tidyr")
#install.packages("visreg")
#install.packages("mgcv")
#install.packages("gratia")

library(gratia)
library(mgcv)
library(visreg)
library(tidyr)
library(bayesplot)
library(tidybayes)
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

#Set working directory
# MAB: this step is not necessary since you are using Rprojects. It also makes your code less reproducible
#setwd("C:/Users/fred1/Downloads/Maîtrise 2025/Analyses_TBE/Git/TBE-Code")

# Importing data in R

Carac_arbre <- read_excel("Carac.xlsx", sheet = 1)
#view(Carac_arbre)

Recolte <- read_excel("Recolte_TBE.xlsx")
#View(Recolte_TBE)

Carac_arbuste <- read_excel("Carac.xlsx", sheet = 2)
#view(Carac_arbuste)

#### Function to change Counting data into relative abundance

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
    feuillus = BEPA + BEAL + ACSU + ACPE + SODE + PODE + FAGR
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

# Adding diversity variables for later analysis

arbre_arbuste = left_join(Carac_arbre, Carac_arbuste, by = "parcelle")
arbre_arbuste$parcelle <- gsub("-", "", arbre_arbuste$parcelle)
arbre_arbuste <- arbre_arbuste %>%
mutate(div = rowSums(across(-parcelle, ~ .x > 0)))

## Joining carac data and Larvae survival data

Recolte_foret <- Recolte %>%
  left_join(Type_foret, by = "parcelle")

### Clean data set

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
### before either completing life cycle or ptoid emergence

Recolte_foret <- Recolte_foret %>%
  mutate(pres_ptoid_clean = ifelse(survie_larve == 0 & 
                                     pres_ptoid == 0, 
                                   NA, pres_ptoid))
Recolte_foret <- Recolte_foret %>%
  mutate(survie_clean = ifelse(survie_larve == 0 & 
                                     pres_ptoid == 0, 
                                   NA, survie_larve))
view(Recolte_foret)
