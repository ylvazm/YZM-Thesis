
#' ---
#' title: "Supplemental Descriptive Statistics"
#' author: "Ylva Matejko"
#' date: "Mar 28, 2025"
#' ---
#' 
#' This script produces some supplemental descriptive graphs for the full sample in the analyses.
#' 

library(ggplot2)

final_desc <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/final_desc.csv"))

plot_desc <- final_desc[1:43,]

style <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/style.rds"))

#'------------------------------------------------------------------------------
#'
#' ### Plot of omission and not-reached rates:

plot_data_2 <- data.frame(ISO = rep(plot_desc$ISO, 2),
                          missing_type = c(rep("Omission", nrow(plot_desc)), rep("Not Reached", nrow(plot_desc))),
                          missing_rate = c(plot_desc$omissions, plot_desc$not_reached),
                          sort = plot_desc$omissions # sort by overall omission rate
)

plot_2 <- ggplot(data = plot_data_2, 
                aes(y = missing_rate, 
                    x = reorder(ISO, sort), # sort by overall omission rate
                    fill = missing_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Missing Rate (%)", 
       x = "Country", 
       fill = "Missing Type",
       title = "Missing Rates by Country and Missing Type") +
  style

#'------------------------------------------------------------------------------
#'
#' ### Plot of completeness, sorted by MC and CR

plot_data_3 <- data.frame(ISO = rep(plot_desc$ISO, 2),
                          item_type = c(rep("MC", nrow(plot_desc)), rep("CR", nrow(plot_desc))),
                          completeness_rate = c(plot_desc$completeness_MC_M, plot_desc$completeness_CR_M),
                          sort = plot_desc$omissions # sort by overall omission rate
)

plot_3 <- ggplot(data = plot_data_3, 
                 aes(y = completeness_rate, 
                     x = reorder(ISO, sort), # sort by overall omission rate
                     fill = item_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Completeness Rate (%)", 
       x = "Country", 
       fill = "Item Type",
       title = "Completeness Rates by Country and Item Type") +
  style





