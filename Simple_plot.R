### Raw data Visualization 2025 ----

## Source clean data ----
source("Data_cleaning.R")

## Early visualisation ----

## 1) Feuillus and survival ----

ggplot(Recolte_foret, aes(x = feuillus, y = survie_larve, color = stade.c)) +
  geom_point(alpha = 0.3, position = position_jitter(height = 0.05)) +
  geom_smooth(aes(group=stade.c),method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "#0072B2", size = 1.2) +
  labs(
    title = "Effet du % de feuillus sur la probabilité de survie",
    x = "% de feuillus",
    y = "Probabilité de survie"
  ) +
  theme_minimal()

## 1.1) By pheno

ggplot(Recolte_foret, aes(x = feuillus, y = survie_larve, color = factor(pheno.c))) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## 1.2) By larval stages

ggplot(Recolte_foret, aes(x = feuillus, y =survie_larve, color = stade)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## 1.3) By pheno and stages

ggplot(Recolte_foret, aes(x = pheno.c, y =survie_larve, color = stade)) +
  #geom_point() +
  #facet_wrap(~stade)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


## 2) Feuillus and pres_ptoid ----
  
  ggplot(Recolte_foret, aes(x = feuillus, y = pres_ptoid)) +
  geom_point(alpha = 0.3, position = position_jitter(height = 0.05)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "#0072B2", size = 1.2) +
  labs(
    title = "Effet du % de feuillus sur la probabilité de présence de parasitoïde",
    x = "% de feuillus",
    y = "Probabilité de parasitisme"
  ) +
  theme_minimal()

## 2.1) By pheno

ggplot(Recolte_foret, aes(x = feuillus, y = pres_ptoid, color = factor(pheno.c))) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## 1.2) By larval stages

ggplot(Recolte_foret, aes(x = feuillus, y =pres_ptoid, color = stade)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## 1.3) By pheno and stages

ggplot(Recolte_foret, aes(x = pheno.c, y =pres_ptoid, color = stade)) +
  #geom_point() +
  #facet_wrap(~stade)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# First signs of non-linearity. Each stages of pheno.c also corresponds to a date
# in time. Let's look at it like that

## 3) Pres_ptoid/survival in time (date_pose.c) ----

# ggplot(Recolte_foret, aes(x=date_pose.c, y = survie_larve))+
#  geom_smooth(method = "glm", method.args = list(family = "binomial" ))

# ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid))+
#  geom_smooth(method = "glm", method.args = list(family = "binomial" ))

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid)) +
  geom_smooth()

## Very clearly not linear, this is expected for parastitoid presence. Seasonal
## abundance should fluctuate like this. Seems like we cover most, if not all,
## of pres_ptoid during that period. 

## 3.1) Survival and pres_ptoid with second degree term

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid))+
  geom_smooth(formula = y ~ poly(x,2), method = "gam",
              method.args = list(family = "binomial" ))

# Clear date when larvae are more likely to be parasitized

ggplot(Recolte_foret, aes(x=date_pose.c, y = survie_larve))+
  geom_smooth(formula = y ~ poly(x,2), method = "gam", 
              method.args = list(family = "binomial" ))

## 3.2) Cutting it down by stages

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid, color = as.factor(stade.c)))+
  geom_smooth(method = "gam", formula = y ~ poly(x,2))

# interesting, seems like stages L4 and L6 might no be so much non-linear but,
# stage L5 which covers The peak (and decline) of parasitoid presence shows non-
# linarity

ggplot(Recolte_foret, aes(x=date_pose.c, y = survie_larve, color = as.factor(stade.c)))+
  geom_smooth(method = "gam", formula = y ~ poly(x,2))

# Relatively similar relation with survival, except that  early L5 seamed to survive
# a lot more than late L4 despite being on the field nearly at the same time.
# Could suggest an interaction of stage*pheno. Biologically, we expect earlier
# stages to have higher base mortality rates, but the data shows that peak L5
# have lower mortality than late/peak L4 maybe suggesting that the parasitoid community
# at this time could have a preference for these larvae. We also had bad weather 
# at early L4 stages + high mortality in the rearing process for these. Will be 
# interesting to keep track for 2026

## 4) Ptoid identity and forest composition ----

## Looking at relation between ptoid_id and parcels/forest composition

Recolte_foret %>% 
  filter(!is.na(feuillus), !is.na(id_ptoid),
         id_ptoid != "NA", feuillus != "NA") %>%
  mutate(id_ptoid = factor(as.character(id_ptoid))) %>%
  ggplot(aes(x = feuillus, y = id_ptoid)) +
  geom_jitter(height = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(y = "Espèce de parasitoïde",
       x = "Composition en feuillus")

## 4.1) Phenology of the two main ptoids

Recolte_foret_clean %>%
  filter(id_ptoid %in% c("Tranosema rostrale", "Phytodietus sp.")) %>%
  mutate(id_ptoid = factor(as.character(id_ptoid))) %>%
  count(date_pose.c, id_ptoid) %>%
  ggplot(aes(x = date_pose.c, y = n, color = id_ptoid, fill = id_ptoid)) +
  #geom_col(position = "dodge", alpha = 0.6) +
  geom_smooth(method = "gam", formula = y ~ poly(x,4),se = FALSE) +
  #geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(y = "Abundance (count)", x = "Date",
       color = "Parasitoid species", fill = "Parasitoid species")

