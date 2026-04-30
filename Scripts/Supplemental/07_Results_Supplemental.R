
#' ---
#' title: "Results - Supplementary Exlorations"
#' author: "Ylva Matejko"
#' date: "Apr 14, 2026"
#' ---
#' 
#' In this script, Rank differences between the models are explored further.

ctrycodes <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/countrycodes.csv"), sep = ";", header = F)
colnames(ctrycodes) <- c("Country","ISO","IDCNTRY")

library(ggplot2) # for plotting
library(grid) # for putting plots next to each other
library(gridExtra) # for putting plots next to each other
library(ggrepel) # for non-overlapping ISO labels
style <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/style.rds"))
palette <- c("#F8766D","#00BA38","#619CFF")


#'------------------------------------------------------------------------------
#'
#' ### Plotting Model Differences regarding Means

df_diff_M <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/df_diff_M.rds"))

# Differences in Country Mean Rankings by Mean Rank in Model 1 

M_diff_by_mean12 <- ggplot(data = df_diff_M, 
                           aes(x = MRank1, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Mean Rank in Model 1", 
       y = "Difference in Mean Ranks") +
  scale_x_continuous(breaks = seq(0, 43, by = 5)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1),
                     limits = c(-3, 3)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Model 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

M_diff_by_mean13 <- ggplot(data = df_diff_M, 
                           aes(x = MRank1, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Mean Rank in Model 1", 
       y = "Difference in Mean Ranks") +
  scale_x_continuous(breaks = seq(0, 43, by = 5)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1),
                     limits = c(-3, 3)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Model 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

M_diff_by_mean23 <- ggplot(data = df_diff_M, 
                           aes(x = MRank1, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Mean Rank in Model 1", 
       y = "Difference in Mean Ranks") +
  scale_x_continuous(breaks = seq(0, 43, by = 5)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1),
                     limits = c(-3, 3)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Model 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

M_diff_by_mean12 <- M_diff_by_mean12 + labs(x = "", y = "")
M_diff_by_mean13 <- M_diff_by_mean13 + labs(x = "", y = "")
M_diff_by_mean23 <- M_diff_by_mean23 + labs(x = "", y = "")

M_diff_by_mean <- grid.arrange(M_diff_by_mean12, M_diff_by_mean13, M_diff_by_mean23, 
                               ncol = 3,
                               left = textGrob("Difference in Mean Ranks", rot = 90,
                                               gp = gpar(fontsize = 12, fontfamily = "serif"),
                                               vjust = 2, hjust = 0.5),
                               bottom = textGrob("Mean Rank in Model 1",
                                                 gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                 vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/M_diff_by_mean.svg"), M_diff_by_mean, width = 25, height = 10, units = "cm") # not used in the report


#'------------------------------------------------------------------------------
#'
#' ### Plotting Model Differences regarding Variances 

df_diff_V <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/df_diff_V.rds"))

# Differences in Country Variances by Country Variance Ranks in Model 1

V_diff_by_mean12 <- ggplot(data = df_diff_V, 
                           aes(x = VRank1, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Country Variance Rank in Model 1", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 43, by = 5)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

V_diff_by_mean13 <- ggplot(data = df_diff_V, 
                           aes(x = VRank1, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Country Variance Rank in Model 1", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 43, by = 5)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_mean23 <- ggplot(data = df_diff_V, 
                           aes(x = VRank1, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Country Variance Rank in Model 1", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 43, by = 5)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_mean12 <- V_diff_by_mean12 + labs(x = "", y = "")
V_diff_by_mean13 <- V_diff_by_mean13 + labs(x = "", y = "")
V_diff_by_mean23 <- V_diff_by_mean23 + labs(x = "", y = "")

V_diff_by_mean <- grid.arrange(V_diff_by_mean12, V_diff_by_mean13, V_diff_by_mean23, 
                               ncol = 3,
                               left = textGrob("Difference in Variance Ranks", rot = 90,
                                               gp = gpar(fontsize = 12, fontfamily = "serif"),
                                               vjust = 2, hjust = 0.5),
                               bottom = textGrob("Variance Rank in Model 1",
                                                 gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                 vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_mean.svg"), V_diff_by_mean, width = 25, height = 10, units = "cm") # not used in the report


#'------------------------------------------------------------------------------
#'
#' ## Correlation of intra-country omission rate variances and inter-country variance rank variability

# modify the omission rate counter function from the descriptives

g8s_v0 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v0.rds"))
items <- colnames(g8s_v0)[grep(pattern="^S", x=colnames(g8s_v0))]

# person-level variance
person_omissions <- rowSums(g8s_v0[, items] == 9 | g8s_v0[, items] == 99, na.rm = TRUE)
omission_sd_person <- tapply(person_omissions, g8s_v0$IDCNTRY, sd, na.rm = TRUE) # using SD for better readability/interpretability

# item-level variance
omission_sd_item <- tapply(seq_len(nrow(g8s_v0)), g8s_v0$IDCNTRY, function(idx) {
  d <- g8s_v0[idx, items]
  item_rates <- colMeans(d == 9 | d == 99, na.rm = TRUE) * 100
  sd(item_rates, na.rm = TRUE)
})

# put this in a df
omission_variance <- data.frame(
  IDCNTRY = names(omission_sd_person),
  sd_person = round(as.numeric(omission_sd_person), 2),
  sd_item = round(as.numeric(omission_sd_item), 2)
)

omission_variance <- merge(omission_variance, ctrycodes, by = "IDCNTRY")
omission_variance <- merge(omission_variance, df_diff_V, by = "ISO")

#' ### Plot Variance Rank Differences against Item-Level Variance (SD) per Country

V_diff_by_io_itemvar12 <- ggplot(data = omission_variance, 
                                 aes(x = sd_item, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item-Level SD of Omission Rates", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 15, by = 3)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_io_itemvar13 <- ggplot(data = omission_variance, 
                                 aes(x = sd_item, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item-Level SD of Omission Rates", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 15, by = 3)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_io_itemvar23 <- ggplot(data = omission_variance, 
                                 aes(x = sd_item, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item-Level SD of Omission Rates", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 15, by = 3)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

# put them next to each other

V_diff_by_io_itemvar12 <- V_diff_by_io_itemvar12 + labs(x = "", y = "")
V_diff_by_io_itemvar13 <- V_diff_by_io_itemvar13 + labs(x = "", y = "")
V_diff_by_io_itemvar23 <- V_diff_by_io_itemvar23 + labs(x = "", y = "")

V_diff_by_io_itemvar <- grid.arrange(V_diff_by_io_itemvar12, V_diff_by_io_itemvar13, V_diff_by_io_itemvar23, 
                                     ncol = 3,
                                     left = textGrob("Difference in Variance Ranks", rot = 90,
                                                     gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                     vjust = 2, hjust = 0.5),
                                     bottom = textGrob("Item-Level SD of Omission Rates",
                                                       gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                       vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_io_itemvar.svg"), V_diff_by_io_itemvar, width = 25, height = 10, units = "cm") #


#'------------------------------------------------------------------------------

#' ### Plot Variance Rank Differences against Person-Level Variance (SD) per Country

V_diff_by_io_personvar12 <- ggplot(data = omission_variance, 
                                   aes(x = sd_person, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Person-Level SD of Omission Rates", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 4, by = 1)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_io_personvar13 <- ggplot(data = omission_variance, 
                                   aes(x = sd_person, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Person-Level SD of Omission Rates", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 4, by = 1)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_io_personvar23 <- ggplot(data = omission_variance, 
                                   aes(x = sd_person, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Person-Level SD of Omission Rates", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(0, 4, by = 1)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

# put them next to each other

V_diff_by_io_personvar12 <- V_diff_by_io_personvar12 + labs(x = "", y = "")
V_diff_by_io_personvar13 <- V_diff_by_io_personvar13 + labs(x = "", y = "")
V_diff_by_io_personvar23 <- V_diff_by_io_personvar23 + labs(x = "", y = "")

V_diff_by_io_personvar <- grid.arrange(V_diff_by_io_personvar12, V_diff_by_io_personvar13, V_diff_by_io_personvar23, 
                                       ncol = 3,
                                       left = textGrob("Difference in Variance Ranks", rot = 90,
                                                       gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                       vjust = 2, hjust = 0.5),
                                       bottom = textGrob("Person-Level SD of Omission Rates",
                                                         gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                         vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_io_personvar.svg"), V_diff_by_io_personvar, width = 25, height = 10, units = "cm") #

#'------------------------------------------------------------------------------
#'
#' ## Correlation of intra-country omission rate variances and response completeness

final_desc <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/final_desc.csv"))
completeness <- final_desc[1:43, c("ISO", "completeness_MC_M", "completeness_MC_SD", "completeness_CR_M", "completeness_CR_SD")]

omission_variance <- merge(omission_variance, completeness, by = "ISO")

#' ### Variance x MC Completeness Mean

V_diff_by_completeness_MC_M12 <- ggplot(data = omission_variance, 
                                        aes(x = completeness_MC_M, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "MC Completeness (%)", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(75, 100, by = 5),
                     limits = c(75, 100)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_MC_M13 <- ggplot(data = omission_variance, 
                                        aes(x = completeness_MC_M, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "MC Completeness (%)", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(75, 100, by = 5),
                     limits = c(75, 100)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_MC_M23 <- ggplot(data = omission_variance, 
                                        aes(x = completeness_MC_M, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "MC Completeness (%)", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(75, 100, by = 5),
                     limits = c(75, 100)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

# put them next to each other

V_diff_by_completeness_MC_M12 <- V_diff_by_completeness_MC_M12 + labs(x = "", y = "")
V_diff_by_completeness_MC_M13 <- V_diff_by_completeness_MC_M13 + labs(x = "", y = "")
V_diff_by_completeness_MC_M23 <- V_diff_by_completeness_MC_M23 + labs(x = "", y = "")

V_diff_by_completeness_MC_M <- grid.arrange(V_diff_by_completeness_MC_M12, V_diff_by_completeness_MC_M13, V_diff_by_completeness_MC_M23, 
                                            ncol = 3,
                                            left = textGrob("Difference in Variance Ranks", rot = 90,
                                                            gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                            vjust = 2, hjust = 0.5),
                                            bottom = textGrob("MC Completeness (%)",
                                                              gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                              vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_completeness_MC_M.svg", V_diff_by_completeness_MC_M, width = 25, height = 10, units = "cm"))

#'------------------------------------------------------------------------------

#' ### Variance x MC Completeness SD

V_diff_by_completeness_MC_SD12 <- ggplot(data = omission_variance, 
                                         aes(x = completeness_MC_SD, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "SD of MC Completeness", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(10, 40, by = 5),
                     limits = c(10, 40)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_MC_SD13 <- ggplot(data = omission_variance, 
                                         aes(x = completeness_MC_SD, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "SD of MC Completeness", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(10, 40, by = 5),
                     limits = c(10, 40)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_MC_SD23 <- ggplot(data = omission_variance, 
                                         aes(x = completeness_MC_SD, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "SD of MC Completeness", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(10, 40, by = 5),
                     limits = c(10, 40)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

# put them next to each other

V_diff_by_completeness_MC_SD12 <- V_diff_by_completeness_MC_SD12 + labs(x = "", y = "")
V_diff_by_completeness_MC_SD13 <- V_diff_by_completeness_MC_SD13 + labs(x = "", y = "")
V_diff_by_completeness_MC_SD23 <- V_diff_by_completeness_MC_SD23 + labs(x = "", y = "")

V_diff_by_completeness_MC_SD <- grid.arrange(V_diff_by_completeness_MC_SD12, V_diff_by_completeness_MC_SD13, V_diff_by_completeness_MC_SD23, 
                                             ncol = 3,
                                             left = textGrob("Difference in Variance Ranks", rot = 90,
                                                             gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                             vjust = 2, hjust = 0.5),
                                             bottom = textGrob("SD of MC Completeness",
                                                               gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                               vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_completeness_MC_SD.svg"), V_diff_by_completeness_MC_SD, width = 25, height = 10, units = "cm") #

#'------------------------------------------------------------------------------

#' Variance x CR Completeness Mean

V_diff_by_completeness_CR_M12 <- ggplot(data = omission_variance, 
                                        aes(x = completeness_CR_M, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "CR Completeness (%)", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(25, 100, by = 10),
                     limits = c(25, 100)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_CR_M13 <- ggplot(data = omission_variance, 
                                        aes(x = completeness_CR_M, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "CR Completeness (%)", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(25, 100, by = 10),
                     limits = c(25, 100)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_CR_M23 <- ggplot(data = omission_variance, 
                                        aes(x = completeness_CR_M, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "CR Completeness (%)", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(25, 100, by = 10),
                     limits = c(25, 100)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

# put them next to each other

V_diff_by_completeness_CR_M12 <- V_diff_by_completeness_CR_M12 + labs(x = "", y = "")
V_diff_by_completeness_CR_M13 <- V_diff_by_completeness_CR_M13 + labs(x = "", y = "")
V_diff_by_completeness_CR_M23 <- V_diff_by_completeness_CR_M23 + labs(x = "", y = "")

V_diff_by_completeness_CR_M <- grid.arrange(V_diff_by_completeness_CR_M12, V_diff_by_completeness_CR_M13, V_diff_by_completeness_CR_M23, 
                                            ncol = 3,
                                            left = textGrob("Difference in Variance Ranks", rot = 90,
                                                            gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                            vjust = 2, hjust = 0.5),
                                            bottom = textGrob("CR Completeness (%)",
                                                              gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                              vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_completeness_CR_M.svg"), V_diff_by_completeness_CR_M, width = 25, height = 10, units = "cm") #

#'------------------------------------------------------------------------------

#' Variance x CR Completeness SD

V_diff_by_completeness_CR_SD12 <- ggplot(data = omission_variance, 
                                         aes(x = completeness_CR_SD, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "SD of CR Completeness", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(25, 50, by = 5),
                     limits = c(25, 50)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_CR_SD13 <- ggplot(data = omission_variance, 
                                         aes(x = completeness_CR_SD, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "SD of CR Completeness", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(25, 50, by = 5),
                     limits = c(25, 50)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_completeness_CR_SD23 <- ggplot(data = omission_variance, 
                                         aes(x = completeness_CR_SD, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "SD of CR Completeness", 
       y = "Difference in Variance Ranks") +
  scale_x_continuous(breaks = seq(25, 50, by = 5),
                     limits = c(25, 50)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Difference between Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

# put them next to each other

V_diff_by_completeness_CR_SD12 <- V_diff_by_completeness_CR_SD12 + labs(x = "", y = "")
V_diff_by_completeness_CR_SD13 <- V_diff_by_completeness_CR_SD13 + labs(x = "", y = "")
V_diff_by_completeness_CR_SD23 <- V_diff_by_completeness_CR_SD23 + labs(x = "", y = "")

V_diff_by_completeness_CR_SD <- grid.arrange(V_diff_by_completeness_CR_SD12, V_diff_by_completeness_CR_SD13, V_diff_by_completeness_CR_SD23, 
                                             ncol = 3,
                                             left = textGrob("Difference in Variance Ranks", rot = 90,
                                                             gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                             vjust = 2, hjust = 0.5),
                                             bottom = textGrob("SD of CR Completeness",
                                                               gp = gpar(fontsize = 12, fontfamily = "serif"),
                                                               vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_completeness_CR_SD.svg"), V_diff_by_completeness_CR_SD, width = 25, height = 10, units = "cm") #







