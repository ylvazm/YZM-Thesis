#' 
#' ---
#' title: "Estimation of Model 1 - Treating Omissions as Not Administered"
#' author: "Ylva Matejko"
#' date: "Apr 19, 2026"
#' ---
#' 
#' This script contains the code to estimate all three models for the present analysis. For each model, first randomly drawn subsample is used 
#' to estimate item parameters. Then, the resulting parameters are applied to the full sample models to estimate country means and variances.
#' 
#' The central output are model1, model2, and model3, which will be stored for further inspection and extraction of results.
#' 

library(TAM)

subset_v1 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/subset_v1.rds"))
subset_v2 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/subset_v2.rds"))
subset_v3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/subset_v3.rds"))

g8s_v1 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v1.rds"))
g8s_v2 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v2.rds"))
g8s_v3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v3.rds"))

#'------------------------------------------------------------------------------
#'
#' ### Model 1

#' The Subset Model

# Model preparation
items_v1 <- colnames(subset_v1[2:213])

# Model estimation
model1b <- tam.mml.2pl(
  resp = subset_v1[, items_v1],
  irtmodel = "GPCM",
  est.variance = FALSE,
  pweights = subset_v1$TOTWGT,
  control = list(maxiter = 5000))

# Save results
saveRDS(model1b, file = paste0(getwd(), "/YZM-Thesis/Models/model1b.rds"))

# Exctract item parameters
xsi_fixed <- model1b$xsi.fixed.estimated # locations
B_fixed <- model1b$B.fixed.estimated #difficulties

#' ---
#' The Full Model

# Model estimation
model1 <- tam.mml.2pl(
  resp = g8s_v1[, items_v1],
  group = as.factor(g8s_v1$IDCNTRY),
  irtmodel = "GPCM",
  est.variance = TRUE,
  pweights = g8s_v1$TOTWGT,
  xsi.fixed = xsi_fixed,
  B.fixed = B_fixed,
  control = list(maxiter = 5000))

# Save results
saveRDS(model1, file = paste0(getwd(), "/YZM-Thesis/Models/model1.rds"))


#'------------------------------------------------------------------------------
#'
#' ### Model 2

#' The Subset Model

# Model preparation
items_v2 <- colnames(subset_v2[2:213])

# Model estimation
model2b <- tam.mml.2pl(
  resp = subset_v2[, items_v2],
  irtmodel = "GPCM",
  est.variance = FALSE,
  pweights = subset_v2$TOTWGT,
  control = list(maxiter = 5000))

# Save results
saveRDS(model2b, file = paste0(getwd(), "/YZM-Thesis/Models/model2b.rds"))

# Extract item parameters
xsi_fixed <- model2b$xsi.fixed.estimated
B_fixed <- model2b$B.fixed.estimated

#' ---
#' The Full Model

# Model estimation
model2 <- tam.mml.2pl(
  resp = g8s_v2[, items_v2],
  group = as.factor(g8s_v2$IDCNTRY),
  irtmodel = "GPCM",
  est.variance = TRUE,
  pweights = g8s_v2$TOTWGT,
  xsi.fixed = xsi_fixed,
  B.fixed = B_fixed,
  control = list(maxiter = 5000))

# Save results
saveRDS(model2, file = paste0(getwd(), "/YZM-Thesis/Models/model2.rds"))


#'------------------------------------------------------------------------------
#'
#' ### Model 3

#' The Subset Model

# Model preparation
items_v3 <- c(colnames(subset_v3[2:213]), "completeness_MC", "completeness_CR")

# Model estimation
model3b <- tam.mml.2pl(
  resp = subset_v3[, items_v3],
  irtmodel = "GPCM",
  est.variance = FALSE,
  pweights = subset_v3$TOTWGT,
  control = list(maxiter = 5000))

# Save results
saveRDS(model3b, file = paste0(getwd(), "/YZM-Thesis/Models/model3b.rds"))

# Extract item parameters
xsi_fixed <- model3b$xsi.fixed.estimated
B_fixed <- model3b$B.fixed.estimated

#' ---
#' The Full Model

# Model estimation
model3 <- tam.mml.2pl(
  resp = g8s_v3[, items_v3],
  group = as.factor(g8s_v3$IDCNTRY),
  irtmodel = "GPCM",
  est.variance = TRUE,
  pweights = g8s_v3$TOTWGT,
  xsi.fixed = xsi_fixed,
  B.fixed = B_fixed,
  control = list(maxiter = 5000))

# Save results
saveRDS(model3, file = paste0(getwd(), "/YZM-Thesis/Models/model3.rds"))


