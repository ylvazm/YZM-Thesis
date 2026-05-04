
#' ---
#' title: "Results - Country Variances"
#' author: "Ylva Matejko"
#' date: "Apr 14, 2026"
#' ---
#' 
#' In this script, all reported results regarding country variances and variance ranks are produced. This includes (1) the extraction of country varainces
#' and the computation of variance rankings for all models, (2) the investigation of the agreement between the produced rankings and ranking differences, 
#' (3) the central graph depicting both 1 and 2, and (4) the investigation of the relationship between item omissions and rank differences between the models.


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
#' ### Variance Rankings

model1_vars <- data.frame(IDCNTRY = model1$groups,
                          Var1 = as.numeric(tapply(model1$variance, model1$group, unique)),
                          VRank1 = rank(as.numeric(tapply(model1$variance, model1$group, unique)))) # lowest variance gets rank 1
saveRDS(model1_vars, file = paste0(getwd(), "/YZM-Thesis/Output/model1_vars.rds"))

model2_vars <- data.frame(IDCNTRY = model2$groups,
                          Var2 = as.numeric(tapply(model2$variance, model2$group, unique)),
                          VRank2 = rank(as.numeric(tapply(model2$variance, model2$group, unique))))
saveRDS(model2_vars, file = paste0(getwd(), "/YZM-Thesis/Output/model2_vars.rds"))

model3_vars <- data.frame(IDCNTRY = model3$groups,
                          Var3 = as.numeric(tapply(model3$variance, model3$group, unique)),
                          VRank3 = rank(as.numeric(tapply(model3$variance, model3$group, unique))))
saveRDS(model3_vars, file = paste0(getwd(), "/YZM-Thesis/Output/model3_vars.rds"))

# Merge them all into one

VRanking <- merge(model1_vars, model2_vars, by = "IDCNTRY")
VRanking <- merge(VRanking, model3_vars, by = "IDCNTRY")
VRanking <- merge(ctrycodes, VRanking, by = "IDCNTRY")
VRanking <- VRanking[,3:9]

# Checkpoint
saveRDS(VRanking, file = paste0(getwd(), "/YZM-Thesis/Output/VRanking.rds"))
VRanking <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/VRanking.rds"))

#'------------------------------------------------------------------------------
#'
#' ## Describe Variances Rank Differences

#' ### Mean Absolute Rank Differences

df_diff_V <- merge(omissions, VRanking, by = "ISO")
df_diff_V$diff12 <- df_diff_V$VRank2-df_diff_V$VRank1
df_diff_V$diff13 <- df_diff_V$VRank3-df_diff_V$VRank1
df_diff_V$diff23 <- df_diff_V$VRank3-df_diff_V$VRank2

# Checkpoint
saveRDS(df_diff_V, file = paste0(getwd(), "/YZM-Thesis/Output/df_diff_V.rds"))
df_diff_V <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/df_diff_V.rds"))

VMARD12 <- mean(abs(df_diff_V$diff12))
VMARD13 <- mean(abs(df_diff_V$diff13))
VMARD23 <- mean(abs(df_diff_V$diff23))

MARDs <- round(c(VMARD12, VMARD13, VMARD23),2)

#' Frequency tables summarizing Rank Differences

Vrank_diff_table <- data.frame(
  Difference = sort(unique(c(abs(df_diff_V$diff12), abs(df_diff_V$diff13), abs(df_diff_V$diff23)))),
  V1_V2 = as.numeric(table(factor(abs(df_diff_V$diff12), levels = sort(unique(c(abs(df_diff_V$diff12), abs(df_diff_V$diff13), abs(df_diff_V$diff23))))))),
  V1_V3 = as.numeric(table(factor(abs(df_diff_V$diff13), levels = sort(unique(c(abs(df_diff_V$diff12), abs(df_diff_V$diff13), abs(df_diff_V$diff23))))))),
  V2_V3 = as.numeric(table(factor(abs(df_diff_V$diff23), levels = sort(unique(c(abs(df_diff_V$diff12), abs(df_diff_V$diff13), abs(df_diff_V$diff23))))))))
Vrank_diff_table <- rbind(Vrank_diff_table, c("MARD", MARDs))


#'------------------------------------------------------------------------------
#'
#' ### Plot Country Variances and Ranks

# Obtain Rank Ranges for the Plot

rankrange <- c()
for(i in 1:43){
  r <- c(VRanking$VRank1[i], VRanking$VRank2[i], VRanking$VRank3[i])
  rankrange[i] <- ifelse(min(r) == max(r),
                         as.character(min(r)),
                         paste0(min(r), "-", max(r)))}

# Build df for the plot

Vplot_data <- data.frame(country = rep(VRanking$ISO,3),
                         model = c(rep("1", 43), rep("2", 43), rep("3", 43)),
                         vars = c(VRanking$Var1, VRanking$Var2, VRanking$Var3),
                         ranks = c(VRanking$VRank1, VRanking$VRank2, VRanking$VRank3),
                         rankrange = rep(rankrange, 3),
                         sort_var = rep(VRanking$Var1, 3)) # sort by Model 1 Var

# Plot

Vplot <- ggplot(data = Vplot_data, 
                aes(x = vars, 
                    y = reorder(country, sort_var, decreasing = T), # sorted by Model 1 var, in decreasing order
                    color = model)) + 
  geom_point(alpha = 0.6, size = 4) + # bigger and slightly transparent to show overlap
  scale_y_discrete(sec.axis = dup_axis(labels = rankrange[order(Vplot_data$sort_var[1:43], decreasing = T)])) +
  scale_x_continuous(breaks = seq(-1, 1.6, by = 0.2)) +
  style +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(legend.text=element_text(size=12,  family="serif"),
        legend.title=element_text(size=12,  family="serif")) +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom") + 
  scale_color_manual(values = palette[1:3],
                     name = "",
                     labels = c("Model 1",  "Model 2", "Model 3")) +
  labs(y = "", x = "Country Variances")

# Export
ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/Vplot.svg"), Vplot, width = 25, height = 20, units = "cm")


#'------------------------------------------------------------------------------
#'
#' ### Variance Rank Differences and Item Omission Rates

# Differences in Country Vars by Omission Rates

V_diff_by_io12 <- ggplot(data = df_diff_V, 
                         aes(x = omissions, y = diff12, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item Omission Rate (%)", 
       y = "Δ Rank (Model 2 - Model 1)") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Variance Ranks in Models 1 and 2") +
  theme(plot.title = element_text(size = 12, family = "serif"))


V_diff_by_io13 <- ggplot(data = df_diff_V, 
                         aes(x = omissions, y = diff13, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item Omission Rate (%)", 
       y = "Δ Rank (Model 3 - Model 1)") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Variance Ranks in Models 1 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))


V_diff_by_io23 <- ggplot(data = df_diff_V, 
                         aes(x = omissions, y = diff23, label = ISO)) +
  geom_abline(intercept = 0, slope = 0, linewidth = 0.75, color = palette[1]) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(size = 3, family = "serif") +
  labs(x = "Item Omission Rate (%)", 
       y = "Δ Rank (Model 3 - Model 2)") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1),
                     limits = c(-4, 4)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80")) +
  style +
  ggtitle("Variance Ranks in Models 2 and 3") +
  theme(plot.title = element_text(size = 12, family = "serif"))

V_diff_by_io12 <- V_diff_by_io12 + labs(x = "", y = "Δ Rank (Model 2 - Model 1)")
V_diff_by_io13 <- V_diff_by_io13 + labs(x = "", y = "Δ Rank (Model 3 - Model 1)")
V_diff_by_io23 <- V_diff_by_io23 + labs(x = "", y = "Δ Rank (Model 3 - Model 2)")

V_diff_by_io <- grid.arrange(V_diff_by_io12, V_diff_by_io13, V_diff_by_io23, 
                             ncol = 3,
                             #left = textGrob("Difference in Variance Ranks", rot = 90,
                              #               gp = gpar(fontsize = 12, fontfamily = "serif"),
                               #              vjust = 2, hjust = 0.5),
                             bottom = textGrob("Item Omission Rate (%)",
                                               gp = gpar(fontsize = 12, fontfamily = "serif"),
                                               vjust = -1, hjust = 0))

ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/V_diff_by_io.svg"), V_diff_by_io, width = 25, height = 10, units = "cm") 




