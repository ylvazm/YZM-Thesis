#' ---
#' title: "Results - Country Means"
#' author: "Ylva Matejko"
#' date: "Apr 14, 2026"
#' ---
#' 
#' In this script, all reported results regarding country means and achievement ranks are produced. This includes (1) the ectraction of country means and the 
#' computation of achevement rankings for all models, (2) the investigation of the agreement between the produced rankings and ranking differences, (3) the central
#' graph depicting both 1 and 2, and (4) the investigation of the relationship between item omissions and rankd differences between the models.

library(TAM)
model1 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model1.rds"))
model2 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model2.rds"))
model3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model3.rds"))

library(ggplot2) # for plotting
library(grid) # for putting plots next to each other
library(gridExtra) # for putting plots next to each other
library(ggrepel) # for non-overlapping ISO labels
style <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/style.rds"))
palette <- c("#F8766D","#00BA38","#619CFF")


ctrycodes <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/countrycodes.csv"), sep = ";", header = F)
colnames(ctrycodes) <- c("Country","ISO","IDCNTRY")

final_desc <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/final_desc.csv"))
omissions <- final_desc[1:43, c("ISO", "omissions")]

#'------------------------------------------------------------------------------
#'
#' ### Mean Rankings

model1_means <- data.frame(IDCNTRY = model1$groups,
                           Mean1 = model1$beta,
                           MRank1 = rank(-model1$beta)) # highest mean gets rank 1

model2_means <- data.frame(IDCNTRY = model2$groups,
                           Mean2 = model2$beta,
                           MRank2 = rank(-model2$beta))

model3_means <- data.frame(IDCNTRY = model3$groups,
                           Mean3 = model3$beta,
                           MRank3 = rank(-model3$beta))

# Merge them all into one

MRanking <- merge(model1_means, model2_means, by = "IDCNTRY")
MRanking <- merge(MRanking, model3_means, by = "IDCNTRY")
MRanking <- merge(ctrycodes, MRanking, by = "IDCNTRY")
MRanking <- MRanking[,3:9]

# Checkpoint
saveRDS(MRanking, file = paste0(getwd(), "/YZM-Thesis/Output/MRanking.rds"))
MRanking <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/MRanking.rds"))

#'------------------------------------------------------------------------------
#'
#' ## Describe Mean Rank Differences

#' ### Mean Absolute Rank Differences

df_diff_M <- merge(omissions, MRanking, by = "ISO")
df_diff_M$diff12 <- (df_diff_M$MRank2-df_diff_M$MRank1) 
df_diff_M$diff13 <- (df_diff_M$MRank3-df_diff_M$MRank1)
df_diff_M$diff23 <- (df_diff_M$MRank3-df_diff_M$MRank2)

# Checkpoint
saveRDS(df_diff_M, file = paste0(getwd(), "/YZM-Thesis/Output/df_diff_M.rds"))
df_diff_M <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/df_diff_M.rds"))

MMARD12 <- mean(abs(df_diff_M$diff12))
MMARD13 <- mean(abs(df_diff_M$diff13))
MMARD23 <- mean(abs(df_diff_M$diff23))

MARDs <- round(c(MMARD12, MMARD13, MMARD23),2)

#' Frequency table summarizing Rank Differences

Mrank_diff_table <- data.frame(
  Difference = sort(unique(c(abs(df_diff_M$diff12), abs(df_diff_M$diff13), abs(df_diff_M$diff23)))),
  M1_M2 = as.numeric(table(factor(abs(df_diff_M$diff12), levels = sort(unique(c(abs(df_diff_M$diff12), abs(df_diff_M$diff13), abs(df_diff_M$diff23))))))),
  M1_M3 = as.numeric(table(factor(abs(df_diff_M$diff13), levels = sort(unique(c(abs(df_diff_M$diff12), abs(df_diff_M$diff13), abs(df_diff_M$diff23))))))),
  M2_M3 = as.numeric(table(factor(abs(df_diff_M$diff23), levels = sort(unique(c(abs(df_diff_M$diff12), abs(df_diff_M$diff13), abs(df_diff_M$diff23))))))))
Mrank_diff_table <- rbind(Mrank_diff_table, c("MARD", MARDs))

#'------------------------------------------------------------------------------
#'
#' ### Plot Country Means and Ranks

# Obtain Rank Ranges for the Plot

rankrange <- c() 
for(i in 1:43){
  r <- c(MRanking$MRank1[i], MRanking$MRank2[i], MRanking$MRank3[i])
  rankrange[i] <- ifelse(min(r) == max(r),
                         as.character(min(r)),
                         paste0(min(r), "-", max(r)))}

# Build df for the plot

Mplot_data <- data.frame(country = rep(MRanking$ISO,3),
                         model = c(rep("1", 43), rep("2", 43), rep("3", 43)),
                         means = c(MRanking$Mean1, MRanking$Mean2, MRanking$Mean3),
                         ranks = c(MRanking$MRank1, MRanking$MRank2, MRanking$MRank3),
                         rankrange = rep(rankrange, 3),
                         sort_var = rep(MRanking$Mean1, 3)) # sort by Model 1 Mean

# Plot

Mplot <- ggplot(data = Mplot_data, 
                aes(x = means, 
                    y = reorder(country, sort_var), # sorted by Model 1 mean, in decreasing order
                    color = model)) + 
  geom_point(alpha = 0.6, size = 4) + # bigger and slightly transparent to show overlap
  scale_y_discrete(sec.axis = dup_axis(labels = rankrange[order(Mplot_data$sort_var[1:43])])) +
  scale_x_continuous(breaks = seq(-2, 1, by = 0.5)) +
  style +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.text=element_text(size=12,  family="serif"),
        legend.title=element_text(size=12,  family="serif")) +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom") + 
  scale_color_manual(values = palette[1:3],
                     name = "",
                     labels = c("Model 1",  "Model 2", "Model 3")) +
  labs(y = "", x = "Country Means")


# Export
ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/Mplot.svg"), Mplot, width = 25, height = 20, units = "cm")


#'------------------------------------------------------------------------------
#'
#' ### Mean Rank Differences and Item Omission Rates

# Differences in Country Mean Rankings by Omission Rates

M_diff_by_io12 <- ggplot(data = df_diff_M, 
                         aes(x = omissions, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item Omission Rate (%)", 
       y = "Δ Rank (Model 2 - Model 1)") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1),
                     limits = c(-3, 3)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Achievement Ranks in Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

M_diff_by_io13 <- ggplot(data = df_diff_M, 
                         aes(x = omissions, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item Omission Rate (%)", 
       y = "Δ Rank (Model 3 - Model 1)") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1),
                     limits = c(-3, 3)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Achievement Ranks in Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

M_diff_by_io23 <- ggplot(data = df_diff_M, 
                         aes(x = omissions, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item Omission Rate (%)", 
       y = "Δ Rank (Model 3 - Model 2)") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1),
                     limits = c(-3, 3)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Achievement Ranks in Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif")) 

# Arrange them for the report

M_diff_by_io12 <- M_diff_by_io12 + labs(x = "", y = "Δ Rank (Model 2 - Model 1)")
M_diff_by_io13 <- M_diff_by_io13 + labs(x = "", y = "Δ Rank (Model 3 - Model 1)")
M_diff_by_io23 <- M_diff_by_io23 + labs(x = "", y = "Δ Rank (Model 3 - Model 2)")

M_diff_by_io <- grid.arrange(M_diff_by_io12, M_diff_by_io13, M_diff_by_io23, 
                             ncol = 3,
                             bottom = textGrob("Item Omission Rate (%)",
                                               gp = gpar(fontsize = 12, fontfamily = "serif"),
                                               vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/M_diff_by_io.svg"), M_diff_by_io, width = 25, height = 10, units = "cm")


