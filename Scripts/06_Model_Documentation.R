
#' ---
#' title: "Model - Documentation"
#' author: "Ylva Matejko"
#' date: "Apr 14, 2026"
#' ---
#' 
#' In this script, relevant output is extracted from the models and compiled in tables.  
#' The central output are item parameter tables, as well as item and model fit statistics and they will be saved in the folder "Output".
#' 

library(TAM)

model1 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model1.rds"))
model2 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model2.rds"))
model3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model3.rds"))

ctrycodes <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/countrycodes.csv"), sep = ";", header = F)
colnames(ctrycodes) <- c("Country","ISO","IDCNTRY")

#'------------------------------------------------------------------------------
#'
#' ## Fit Statistics 

model1_fit <- tam.modelfit(model1) # these take a while to compute
saveRDS(model1_fit, file = paste0(getwd(), "/YZM-Thesis/Output/model1_fit.rds"))
model1_fit <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/model1_fit.rds"))

model2_fit <- tam.modelfit(model2)
saveRDS(model2_fit, file = paste0(getwd(), "/YZM-Thesis/Output/model2_fit.rds"))
model2_fit <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/model2_fit.rds"))

model3_fit <- tam.modelfit(model3)
saveRDS(model3_fit, file = paste0(getwd(), "/YZM-Thesis/Output/model3_fit.rds"))
model3_fit <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/model3_fit.rds"))

#'---
#'
#' ### Model Fit

model1_fit$statlist
model2_fit$statlist
model3_fit$statlist

#'---
#'
#' ### Item Fit

model1_itemfit <- IRT.itemfit(model1) 
saveRDS(model1_itemfit, file = paste0(getwd(), "/YZM-Thesis/Output/model1_itemfit.rds"))

model2_itemfit <- IRT.itemfit(model2) 
saveRDS(model2_itemfit, file = paste0(getwd(), "/YZM-Thesis/Output/model2_itemfit.rds"))

model3_itemfit <- IRT.itemfit(model3) 
saveRDS(model3_itemfit, file = paste0(getwd(), "/YZM-Thesis/Output/model3_itemfit.rds"))

#'------------------------------------------------------------------------------
#'
#' ## Item Parameters

poly_items <- c("SE72141B", "SE72345", "SE62018", "SE62095", "SQ82T02A", "SQ72S13B",
                "SE82060", "SE72000", "SE72523", "SE62101", "SE62243", "SE82263",
                "SE62093", "SE72005", "SE72033", "SE72920", "SE72016", "SE72210")

xsi <- model1$xsi

model1_itempar <- data.frame(
  Item = model1$item_irt$item,
  Alpha = round(model1$item_irt$alpha, 3),
  Delta = ifelse(!model1$item_irt$item %in% poly_items,
                 round(xsi[paste0(model1$item_irt$item, "_Cat1"), "xsi"] / model1$item_irt$alpha, 3), NA),
  Delta1 = ifelse(model1$item_irt$item %in% poly_items,
                  round(xsi[paste0(model1$item_irt$item, "_Cat1"), "xsi"] / model1$item_irt$alpha, 3), NA),
  Delta2 = ifelse(model1$item_irt$item %in% poly_items,
                  round(xsi[paste0(model1$item_irt$item, "_Cat2"), "xsi"] / model1$item_irt$alpha, 3), NA),
  WRMSD = model1_itemfit$RMSD$WRMSD)

saveRDS(model1_itempar, file = paste0(getwd(), "/YZM-Thesis/Output/model1_itempar.rds"))

#---

xsi <- model2$xsi

model2_itempar <- data.frame(
  Item = model2$item_irt$item,
  Alpha = round(model2$item_irt$alpha, 3),
  Delta = ifelse(!model2$item_irt$item %in% poly_items,
                 round(xsi[paste0(model2$item_irt$item, "_Cat1"), "xsi"] / model2$item_irt$alpha, 3), NA),
  Delta1 = ifelse(model2$item_irt$item %in% poly_items,
                  round(xsi[paste0(model2$item_irt$item, "_Cat1"), "xsi"] / model2$item_irt$alpha, 3), NA),
  Delta2 = ifelse(model2$item_irt$item %in% poly_items,
                  round(xsi[paste0(model2$item_irt$item, "_Cat2"), "xsi"] / model2$item_irt$alpha, 3), NA),
  WRMSD = model2_itemfit$RMSD$WRMSD)

saveRDS(model2_itempar, file = paste0(getwd(), "/YZM-Thesis/Output/model2_itempar.rds"))

#---

xsi <- model3$xsi

model3_itempar <- data.frame(
  Item = model3$item_irt$item,
  Alpha = round(model3$item_irt$alpha, 3),
  Delta = ifelse(!model3$item_irt$item %in% poly_items,
                 round(xsi[paste0(model3$item_irt$item, "_Cat1"), "xsi"] / model3$item_irt$alpha, 3), NA),
  Delta1 = ifelse(model3$item_irt$item %in% poly_items,
                  round(xsi[paste0(model3$item_irt$item, "_Cat1"), "xsi"] / model3$item_irt$alpha, 3), NA),
  Delta2 = ifelse(model3$item_irt$item %in% poly_items,
                  round(xsi[paste0(model3$item_irt$item, "_Cat2"), "xsi"] / model3$item_irt$alpha, 3), NA),
  WRMSD = model3_itemfit$RMSD$WRMSD)

saveRDS(model3_itempar, file = paste0(getwd(), "/YZM-Thesis/Output/model3_itempar.rds"))
