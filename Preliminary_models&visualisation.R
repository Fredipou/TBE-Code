### early visualisation and models with raw data

## Early visulisation

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

## Same but with ptoids

ggplot(Recolte_foret, aes(x = pheno.c, y =pres_ptoid_clean, color = stade.c)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## Ptoids/date, by stages and pheno

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid_clean, color = as.factor(stade.c)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial" ))

## Same but as a poly relation

ggplot(Recolte_foret, aes(x=date_pose.c, y = pres_ptoid, color = as.factor(pheno.c)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial" ))+
  binomial_smooth(formula = y ~ poly(c.stade,2))

## Early models

model_glmer <- glmer(pres_ptoid_clean ~ feuillus + pheno.c*stade.c + (1 | parcelle), 
                     family = binomial, data = Recolte_foret)
summary(model_glmer)
