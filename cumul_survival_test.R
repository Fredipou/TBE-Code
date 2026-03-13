### Cumulative survival and selection analysis ----
# Step 1: Calculate the mean survival of early, peak and late larvae since a larvae
# early phenology at L4 would also most likely be an early L5.
# Survival = Nsurvivor/ Ntotal

# Liste des datasets
data_list <- list(
  L4 = data_sub_L4,
  L5 = data_sub_L5,
  L6 = data_sub_L6
)

# Fonction unique
calc_survival <- function(data, phenotype) {
  data %>%
    filter(pheno.c == phenotype) %>%
    summarise(survival = mean(survie_larve == 1)) %>%
    pull(survival)
}

# Phénotypes
phenotypes <- c(Early = -1, Peak = 0, Late = 1)

# Calcul automatique
results <- map_dbl(phenotypes, function(p) {
  map_dbl(data_list, calc_survival, phenotype = p) %>%
    prod(na.rm = TRUE)
})

results
