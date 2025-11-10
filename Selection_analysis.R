### Slection gradient analysis 
### From Lande (1983) and M. Barbour (xxxx)

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
  mutate(rel_fitness = survie_clean / mean(survie_clean, na.rm = T)) %>%
  ungroup()

data_sub_L4 <- Recolte_foret[Recolte_foret$stade.c == 4, ]

Recolte_foret$phenology_sq <- Recolte_foret$pheno.c^2

mod_L4 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_L4)
summary(mod_L4)


Recolte_foret <- Recolte_foret %>%
  group_by(stade.c) %>%
  mutate(rel_fitness = survie_clean / mean(survie_clean, na.rm = T)) %>%
  ungroup()

## And for other Larval stages
data_sub_L5 <- Recolte_foret[Recolte_foret$stade.c == 5, ]

mod_L5 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_L5)
summary(mod_L5)

data_sub_L6 <- Recolte_foret[Recolte_foret$stade.c == 6, ]

mod_L6 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_L6)
summary(mod_L6)

data_sub_pupe <- Recolte_foret[Recolte_foret$stade.c == 7, ]

mod_L7 <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_sub_pupe)
summary(mod_L7)
 
## Trying with relative fitness across stages to see (and without pupae)

data_nopupe <- Recolte_foret[Recolte_foret$stade.c != 7, ]
data_nopupe <- data_nopupe %>%
  group_by(stade.c) %>%
  mutate(rel_fitness = survie_clean / mean(survie_clean, na.rm = T)) %>%
  ungroup() 

mod_all_stages <- lm(rel_fitness ~ pheno.c + phenology_sq, data = data_nopupe)
summary(mod_all_stages)
