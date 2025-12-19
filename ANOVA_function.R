library(tidyverse)

breeding_data <- tribble(
  ~rep, ~genotype, ~plant_height_cm, ~tillers_per_plant, ~days_to_flowering, ~grain_yield_g,
  
  1, "G1", 95,  4, 72, 28.5,
  1, "G2", 102, 5, 70, 32.1,
  1, "G3", 110, 6, 68, 35.4,
  1, "G4", 118, 7, 66, 38.9,
  1, "G5", 125, 8, 64, 42.3,
  
  2, "G1", 96,  4, 73, 29.0,
  2, "G2", 101, 5, 71, 31.8,
  2, "G3", 109, 6, 69, 35.0,
  2, "G4", 117, 7, 67, 39.2,
  2, "G5", 126, 8, 65, 42.7,
  
  3, "G1", 94,  4, 72, 28.2,
  3, "G2", 103, 5, 70, 32.4,
  3, "G3", 111, 6, 68, 35.8,
  3, "G4", 119, 7, 66, 39.0,
  3, "G5", 124, 8, 64, 42.0
)

data <- breeding_data |> 
  mutate(replication = factor(rep),
         genotype = factor(genotype)
         ) 

traits <- colnames(data[, 3:6])
traits

ANOVA <- function(trait) {
  anova_results <- list()
  for (trait in traits) {
    formula <- as.formula(
      paste(trait, "~ replication + genotype")
    )
    model <- aov(formula, data)
    anova_results[[trait]] <- summary(model)
  }
  return(summary(model))
}


ANOVA(data$plant_height_cm)
ANOVA(data$tillers_per_plant)
ANOVA(data$days_to_flowering)
ANOVA(data$grain_yield_g)

