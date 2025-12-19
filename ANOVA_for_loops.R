#### Some simple fpr loop example #####


numbers <- 1:5
for (i in numbers) {
  print(i)
}


numbers <- 1:5
for (i in numbers) {
  result <- i * i
  print(result)
}


fruits <- c("applebananana", "banana", "mango")

for (i in fruits) {
  print(i)
}


#### Main Code #####
##### Automation of ANOVA via for loop ####

library(tidyverse)

yield_data <- tribble(
  ~rep, ~genotype, ~plant_height_cm, ~tillers_per_plant, ~panicle_length_cm, ~thousand_grain_weight_g, ~grain_yield_t_ha,
  
  1, "G1", 102, 5, 24.5, 32.1, 4.25,
  1, "G2", 110, 6, 26.2, 34.0, 4.80,
  1, "G3", 98,  4, 23.1, 30.5, 3.95,
  
  2, "G1", 104, 5, 25.0, 32.4, 4.30,
  2, "G2", 112, 6, 26.8, 34.3, 4.90,
  2, "G3", 100, 4, 23.5, 30.8, 4.05,
  
  3, "G1", 103, 5, 24.8, 32.2, 4.28,
  3, "G2", 111, 6, 26.5, 34.1, 4.85,
  3, "G3", 99,  4, 23.3, 30.6, 4.00
)

yield_data

yield_data <- yield_data %>%
  mutate(
    genotype = as.factor(genotype),
    rep = as.factor(rep)
  )


traits <- c("plant_height_cm", "tillers_per_plant", "panicle_length_cm", "thousand_grain_weight_g", "grain_yield_t_ha")

anova_results <- list()
for (trait in traits) {
  formula <- as.formula(
    paste(trait, "~ genotype + rep")
  )
  model <- aov(formula, data = yield_data)
  anova_results[[trait]] <- summary(model)
}

# 6. PRINT ALL RESULTS IN CONSOLE
for (trait in traits) {
  
  cat("\n========================================================\n")
  cat("ANALYSIS OF VARIANCE (ANOVA) FOR:", trait, "\n")
  cat("==========================================================\n")
  
  print(anova_results[[trait]])
}







