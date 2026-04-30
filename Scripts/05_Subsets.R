
#' ---
#' title: "Drawing and Investigating a Random Subset for IRT Model Estimation"
#' author: "Ylva Matejko"
#' date: "Apr 11, 2025"
#' ---
#' 
#' In this script, the subsample for item parameter estimation is drawn and compared with the full sample to check representativity.
#' 
#'------------------------------------------------------------------------------
#'
#' ### Drawing a random subset 

# Load data
g8s_v0 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v0.rds"))

# Draw random rows
set.seed(1)
subset_ids <- tapply(1:nrow(g8s_v0), # draw sample from the row numbers
                   g8s_v0$IDCNTRY,  # splittet by country
                   function(x) sample(x, size = round(0.2 * length(x)))) # 20% of examinees per country
# inspect list
summary(subset_ids)
length(subset_ids[["31"]]) # 1235 examinees from country 31
nrow(g8s_v0[g8s_v0$IDCNTRY==31,])*0.2 # fits

# Subset randomly drawn rows
subset_ids <- unlist(subset_ids)
subset_v0 <- g8s_v0[subset_ids,]

g8s_v3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v3.rds"))
subset_v3 <- g8s_v3[subset_ids,]


#'------------------------------------------------------------------------------
#'
#' ### Represantativeness Check

final_desc <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/final_desc.csv")) # call previously computed sample descriptives
analyse_missings <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/analyse_missings.rds")) # call analyse_missings function
MC_items <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/MC_items.rds"))
CR_items <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/CR_items.rds"))

# Compute sample descriptives for the subset

# Quick Check
summary(subset_v0[,2:11]) # Sex and Age contain invalid codes 9 and 99

subset_v0$BSDAGE[subset_v0$BSDAGE == 99 | subset_v0$BSDAGE == 9] <- NA
subset_v0$ITSEX[subset_v0$ITSEX == 9]    <- NA

# Compute descriptives
sample_desc <- data.frame(
  IDCNTRY = sort(unique(subset_v0$IDCNTRY)),
  n_schools = tapply(subset_v0$IDSCHOOL, subset_v0$IDCNTRY, function(x) length(unique(x))),
  n_classes = tapply(subset_v0$IDCLASS,  subset_v0$IDCNTRY, function(x) length(unique(x))),
  n_students = tapply(subset_v0$IDSTUD,   subset_v0$IDCNTRY, length),
  pct_female = tapply(subset_v0$ITSEX, subset_v0$IDCNTRY, function(x) round(100 * mean(x == 1, na.rm = TRUE), 2)),
  pct_male = tapply(subset_v0$ITSEX, subset_v0$IDCNTRY, function(x) round(100 * mean(x == 2, na.rm = TRUE), 2)),
  age_M  = round(tapply(subset_v0$BSDAGE, subset_v0$IDCNTRY, mean, na.rm = TRUE), 2),
  age_SD = round(tapply(subset_v0$BSDAGE, subset_v0$IDCNTRY, sd,   na.rm = TRUE), 2)
)

# Compute omissions and not reached per country
items <- colnames(subset_v0)[grep(pattern="^S", x=colnames(subset_v0))]
missings_clean <- analyse_missings(subset_v0[, c("IDCNTRY", items)])
missings_clean <- missings_clean[,c("IDCNTRY", "Omitted_Invalid", "Not_Reached")]
colnames(missings_clean) <- c("IDCNTRY", "omissions", "not_reached")

# Quick check
all(missings_clean$IDCNTRY == sort(unique(subset_v3$IDCNTRY))) # if TRUE, the resulting df from analyse_missings is sorted in the same order as the descriptive df

# Create table with missings and missingness indicators per country
missing_desc <- data.frame(
  IDCNTRY = sort(unique(subset_v3$IDCNTRY)),
  omissions = missings_clean$omissions,
  not_reached = missings_clean$not_reached,
  completeness_MC_M = round(tapply(subset_v3$completeness_MC, subset_v3$IDCNTRY, mean, na.rm = TRUE)*100, 2),
  completeness_MC_SD = round(tapply(subset_v3$completeness_MC, subset_v3$IDCNTRY, sd, na.rm = TRUE)*100, 2),
  completeness_CR_M = round(tapply(subset_v3$completeness_CR, subset_v3$IDCNTRY, mean, na.rm = TRUE)*100,2),
  completeness_CR_SD = round(tapply(subset_v3$completeness_CR, subset_v3$IDCNTRY, sd, na.rm = TRUE)*100, 2)
)

# Merge the data.frames
subset_desc <- merge(sample_desc, missing_desc, by = "IDCNTRY")

# Compute missing rates across countries
missings_total <- analyse_missings(
  cbind(IDCNTRY = rep("Total", nrow(subset_v0)), subset_v0[, items]) # substituting all country IDs with "Total" to obtain only one value per category
)

# Add total row
total_row <- data.frame(
  IDCNTRY = "Total",
  n_schools = nrow(unique(subset_v0[, c("IDCNTRY", "IDSCHOOL")])),
  n_classes = nrow(unique(subset_v0[, c("IDCNTRY", "IDSCHOOL", "IDCLASS")])),
  n_students = nrow(subset_v0),
  pct_female = round(100 * mean(subset_v0$ITSEX == 1, na.rm = TRUE), 2),
  pct_male = round(100 * mean(subset_v0$ITSEX == 2, na.rm = TRUE), 2),
  age_M = round(mean(subset_v0$BSDAGE, na.rm = TRUE), 2),
  age_SD = round(sd(subset_v0$BSDAGE, na.rm = TRUE), 2),
  omissions = missings_total$Omitted_Invalid,
  not_reached = missings_total$Not_Reached,
  completeness_MC_M = round(mean(subset_v3$completeness_MC, na.rm = TRUE)*100, 2),
  completeness_MC_SD = round(sd(subset_v3$completeness_MC, na.rm = TRUE)*100, 2),
  completeness_CR_M = round(mean(subset_v3$completeness_CR, na.rm = TRUE)*100, 2),
  completeness_CR_SD = round(sd(subset_v3$completeness_CR, na.rm = TRUE)*100, 2)
  )

subset_desc <- rbind(subset_desc, total_row)

# checkpoint
write.csv(subset_desc, file = paste0(getwd(), "/YZM-Thesis/Data/subset_desc.csv"), row.names = FALSE)

# Comparing full sample and subset sample
diff_desc <- data.frame(
  IDCNTRY = final_desc$ISO,
  pct_female = round(subset_desc$pct_female - final_desc$pct_female, 2),
  age_M = round(subset_desc$age_M - final_desc$age_M, 2),
  omissions = round(subset_desc$omissions - final_desc$omissions, 2),
  not_reached = round(subset_desc$not_reached - final_desc$not_reached, 2),
  completeness_MC_M = round(subset_desc$completeness_MC_M - final_desc$completeness_MC_M, 2),
  completeness_CR_M = round(subset_desc$completeness_CR_M - final_desc$completeness_CR_M, 2)
)

summary(diff_desc)

# To verify that the random subsample was representative of the full dataset, key demographic and response characteristics were compared across countries. Differences in the proportion of female students, mean age, omission rates, and not-reached rates between the subsample and the full dataset were negligible across all countries (all mean differences < 0.1), confirming adequate representativeness of the subsample

#'------------------------------------------------------------------------------
#'
#' ### Save subsets

# Load data
g8s_v1 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v1.rds"))
g8s_v2 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v2.rds"))
g8s_v3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v3.rds"))

# Create subsets
subset_v1 <- g8s_v1[subset_ids,]
subset_v2 <- g8s_v2[subset_ids,]
subset_v3 <- g8s_v3[subset_ids,]

# Save subsets
saveRDS(subset_v0, 
        file = paste0(getwd(), "/YZM-Thesis/Data/subset_v0.rds"))
saveRDS(subset_v1, 
        file = paste0(getwd(), "/YZM-Thesis/Data/subset_v1.rds"))
saveRDS(subset_v2, 
        file = paste0(getwd(), "/YZM-Thesis/Data/subset_v2.rds"))
saveRDS(subset_v3, 
        file = paste0(getwd(), "/YZM-Thesis/Data/subset_v3.rds"))

