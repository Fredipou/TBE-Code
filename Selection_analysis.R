### Slection gradient analysis 
### From Lande (1983) and M. Barbour (xxxx)

## Setup ----

# source clean data
source("Data_cleaning.R")

# set plot theme
theme_set(theme_cowplot()) # I had to for my eyes...

## Calculating mean survival for each larval stages and globally

mean_survival = mean(Recolte_foret$survie_clean, na.rm = T)
mean_survival

## Same but without the Pupae stage because there seems to be very little parasitism

mean_survival_nopupae = mean(Recolte_foret[Recolte_foret$stade != "Pupae",]$survie_clean, na.rm = T)
mean_survival_nopupae
View(Recolte_foret)

## For now, all stage separatly since we are not sure how to incorporate life stages
## Modeling for L4 to start

Recolte_foret <- Recolte_foret %>%
  group_by(stade.c) %>%
  # note that relative fitness should be calculated separately for each larval stage separately if you are trying to calculate
  # selection gradients. A way to confirm if you did it correctly, is that the Estimate of the model (Intercept) should be 1
  mutate(rel_fitness = survie_clean / mean(survie_clean, na.rm = T)) %>%
  ungroup()

Recolte_foret$phenology_sq <- Recolte_foret$pheno.c^2 # this works, although I usually just do + I(pheno.c^2) directly in the model
# doing it directly in the model will be easier for other packages to visualize the effects, otherwise it is treated as a different variable
# either way, it needs to go before other subsets to continue working downstream.

data_sub_L4 <- Recolte_foret[Recolte_foret$stade.c == 4, ]


mod_L4 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_L4)
summary(mod_L4)
summary(lm(rel_fitness ~ pheno.c, data = data_sub_L4)) # you could drop the higher-order term since there is no evidence supporting it
# you will need to do it anyway to calculate the actual effect of directional selection

# removing because this is redundant with above
# Recolte_foret <- Recolte_foret %>%
#   group_by(stade.c) %>%
#   mutate(rel_fitness = survie_clean / mean(survie_clean, na.rm = T)) %>%
#   ungroup()

## And for other Larval stages
data_sub_L5 <- Recolte_foret[Recolte_foret$stade.c == 5, ]

mod_L5 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_L5)
summary(mod_L5)
visreg::visreg(mod_L5)
visreg::visreg(lm(rel_fitness ~ pheno.c + I(pheno.c^2), data = data_sub_L5)) # see how pheno.c is plotted together

data_sub_L6 <- Recolte_foret[Recolte_foret$stade.c == 6, ]

mod_L6 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_L6)
summary(mod_L6)

data_sub_pupe <- Recolte_foret[Recolte_foret$stade.c == 7, ]

mod_L7 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_pupe)
summary(mod_L7)
 
