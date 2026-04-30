
#' ---
#' title: "Data Preparation - Check"
#' author: "Ylva Matejko"
#' date: "Mar 21, 2025"
#' ---

#'This script serves to examine the preparation of the three dataset versions by making some logical checks. 
#'
#'1. g8s_v1 treats item omissions as not administered (default in TIMSS Scoring Program)
#'2. g8s_v2 treats item omissions as incorrect (0 points)
#'3. g8s_v3 treats item omissions with Modified Laplace Smoothing
#'
#'------------------------------------------------------------------------------
#'
#' ### Load dfs

g8s_v1 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v1.rds"))
g8s_v2 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v2.rds"))
g8s_v3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v3.rds"))

#'------------------------------------------------------------------------------
#' 
#' ### Dimension check
#' 

# to check if g8s data were prepared properly, perform the following checks:
# a) dim() in v1 and v2 should be the same, 

dim(g8s_v1) == dim(g8s_v2) 

# and b) in v3 there should be more columns because I kept the missing indicators in v3

dim(g8s_v1)[2] < dim(g8s_v3)[2] 


#'------------------------------------------------------------------------------
#' 
#' ### Check for logical errors in NAs
#' 

# there should be the same number of NA in v1 and v3 (when including only the item variables)

items <- colnames(g8s_v1)[grepl("^S..2", colnames(g8s_v1))] # 212 remaining items is correct according to Aldrich et al. (2024)
length(items)

# there should be fewer NA and more 0 in v2 than in v1
sum(is.na(g8s_v1[,items]))
sum(is.na(g8s_v2[,items]))
sum(is.na(g8s_v1[,items])) - sum(is.na(g8s_v2[,items])) 

sum(g8s_v1[,items] == 0, na.rm = TRUE)
sum(g8s_v2[,items] == 0, na.rm = TRUE)
sum(g8s_v1[,items] == 0, na.rm = TRUE) - sum(g8s_v2[,items] == 0, na.rm = TRUE)

# ------------------------------------------------------------------------------
 
# v1 should not == v2, because v2 should have fewer NAs

test_v1 <- as.character(unlist(g8s_v1, use.names = FALSE))
test_v2 <- as.character(unlist(g8s_v2, use.names = FALSE))

test_v1[is.na(test_v1)] <- "NA"
test_v2[is.na(test_v2)] <- "NA"

length(test_v1) - sum(test_v1 == test_v2) # compares each cell of v1 and v2 and subtracts the amount of equal cells from the amount of cells in v1

# the result should be equal to the number of NAs in v2

sum(test_v1 == "NaN") - sum(test_v2 == "NaN") 

sum(test_v1 == 0, na.rm = TRUE) 
sum(test_v2 == 0, na.rm = TRUE) 
sum(test_v1 == 0, na.rm = TRUE) - sum(test_v2 == 0, na.rm = TRUE) 

#'------------------------------------------------------------------------------
#' 
#' ### rowMeans check

means_v1 <- rowMeans(g8s_v1[,items], na.rm = TRUE) 
means_v2 <- rowMeans(g8s_v2[,items], na.rm = TRUE) 
means_v3 <- rowMeans(g8s_v3[,items], na.rm = TRUE) 

# there should be no cases where the computation of a row mean failed due to absence of values
sum(is.na(means_v1)) 
sum(is.na(means_v2))  
sum(is.na(means_v3)) 

# the rowMeans in v1 and v3 should be the same if missing indicators are removed
comp1 <- means_v1 == means_v3
sum(is.na(comp1)) 
sum(comp1 == TRUE, na.rm = TRUE) 
sum(comp1 == FALSE, na.rm = TRUE) #if this is 0, v1 and v3 are identical

# ------------------------------------------------------------------------------

# the rowMeans should be lower in v2 due to scoring omissions as incorrect 

comp2 <- means_v1 == means_v2
sum(is.na(comp2)) 
sum(comp2 == TRUE, na.rm = TRUE) # in these cases row means do not differ
sum(comp2 == FALSE, na.rm = TRUE) # in these cases, row means do differ

comp3 <- means_v1 > means_v2 # in cases where row means do differ, the row mean should be higher in v1 compared to v2
sum(is.na(comp3)) 
sum(comp3 == TRUE, na.rm = TRUE) # these are the cases where the row means in v1 are indeed higher
sum(comp3 == FALSE, na.rm = TRUE) # these are the cases where the row means in v1 are not higher than in v2


#'------------------------------------------------------------------------------
#' 
#' ### Conclusion
#' 
#' The preparation of datasets was done without error.
