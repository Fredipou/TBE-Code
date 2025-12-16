### early visualisation and models with raw data

## Setup ----

# source clean data
source("Data_cleaning.R")

# set plot theme
theme_set(theme_cowplot()) # I had to for my eyes...

## Early visulisation ----
# MAB: try adding 4 dashes (or #) after different sections of your code to make it easier to hide it.
# Not completely necessary here, but helpful for longer scripts.

## Feuillus vs ptoid
ggplot(Recolte_foret, aes(x = feuillus, y = survie_clean)) +
  geom_point(alpha = 0.3, position = position_jitter(height = 0.05)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "#0072B2", size = 1.2) +
  labs(
    title = "Effet du % de feuillus sur la probabilité de survie",
    x = "% de feuillus",
    y = "Probabilité de parasitisme"
  ) +
  theme_minimal()

## alternative view: I actually don't think this is a good idea, but I was just exploring it.
# Recolte_foret %>%
#   select(parcelle, stade, pheno, feuillus, survie_clean) %>%
#   filter(stade != "Pupae") %>%
#   drop_na() %>%
#   group_by(parcelle, feuillus, stade, pheno) %>%
#   summarise(count = n(),
#             sum_survie_clean = sum(survie_clean)) %>%
#   ggplot(aes(x = feuillus, y = sum_survie_clean / count)) +
#   geom_point(aes(size = count)) +
#   geom_smooth(method = "glm", method.args = list(family = "binomial"))


## Same thing but with ptoids

ggplot(Recolte_foret, aes(x = feuillus, y =pres_ptoid_clean)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


## Survival vs forest composition by pheno.c

ggplot(Recolte_foret, aes(x = feuillus, y = survie_clean, color = factor(pheno.c))) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


## Survival vs forest comp by larval stages

ggplot(Recolte_foret, aes(x = feuillus, y =survie_clean, color = stade)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## Survival/pheno by stages

ggplot(Recolte_foret, aes(x = pheno.c, y =survie_clean, color = stade)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## Same but ungrouped

ggplot(Recolte_foret, aes(x = pheno.c, y =survie_clean, color = stade)) +
  #geom_point() +
  facet_wrap(~stade)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## Survival by date
ggplot(Recolte_foret, aes(x=date_pose.c, y = survie_clean))+
  geom_smooth(formula = y ~ poly(x,2), method = "glm", method.args = list(family = "binomial" ))
# interesting. I would like to see the date range for early, normal, and late phenological stages super-imposed on this.
# or would this not be informative because it covers basically the entire range of dates?

## Same but with ptoids

ggplot(Recolte_foret, aes(x = pheno.c, y =pres_ptoid_clean, color = stade.c)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## Ptoids/date, by stages and pheno

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid_clean, color = as.factor(stade.c)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial" ))

## Same but as a poly relation

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid, color = as.factor(pheno.c)))+
  geom_smooth(formula = y ~ poly(x,2), method = "glm", method.args = list(family = "binomial" ))

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid))+
  geom_smooth(formula = y ~ poly(x,2), method = "glm", method.args = list(family = "binomial" ))
# seems to be a clear date when larvae are more likely to be parasitized.

## Early models

model_glmer <- glmer(pres_ptoid_clean ~ feuillus + pheno.c*stade.c + (1 | parcelle), 
                     family = binomial, data = Recolte_foret)
summary(model_glmer)

