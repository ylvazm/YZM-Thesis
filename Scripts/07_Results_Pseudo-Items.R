#' ---
#' title: "Results - Completeness Indicators"
#' author: "Ylva Matejko"
#' date: "Apr 19, 2025"
#' ---
#' 
#' This script is used to explore the completeness indicators. The key output is a graph depicting the item parameter distributions, higlighting the parameters
#' of the completeness indicators, as well as their ICCs and IICs.

library(TAM)
model3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Models/model3.rds"))
model3_itempar <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/model3_itempar.rds"))

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
palette <- c("#F8766D","#00BA38","#619CFF")
style <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/style.rds"))

#'------------------------------------------------------------------------------
#'
#' ### Investigating the completeness indicators

# Inspect item parameters

model3_itempar[213:214,]

summary(model3_itempar$Alpha)
summary(model3_itempar$Delta)

boxplot_df <- data.frame(item = rep(model3_itempar[,1],2),
                         param = c(rep("alpha", 214), rep("delta", 214)),
                         value = c(model3_itempar$Alpha, model3_itempar$Delta))

# Boxplot of Alpha parameters

boxplot_alpha <- ggplot(boxplot_df[1:214,], aes(x = value)) +
  geom_boxplot() +
  geom_jitter(aes(y = 0), height = 0.02, color = "grey80", alpha = 0.5) +
  annotate("point", x = boxplot_df[213,"value"], y = 0, size = 3, color = palette[3]) +
  annotate("point", x = boxplot_df[214,"value"], y = 0, size = 3, color = palette[2]) +
  style +
  scale_x_continuous(breaks = seq(-1, 3, by = 0.5)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "", x = expression(alpha))

#'------------------------------------------------------------------------------

# Boxplot of Delta parameters

boxplot_delta <- ggplot(boxplot_df[215:428,], aes(x = value)) +
  geom_boxplot(outlier.color = "grey80") +
  geom_jitter(data = boxplot_df[boxplot_df$value > -2.5,], aes(y = 0), 
              height = 0.02, color = "grey80", alpha = 0.5) +
  annotate("point", x = boxplot_df[427,"value"], y = 0, size = 3, color = palette[3]) +
  annotate("point", x = boxplot_df[428,"value"], y = 0, size = 3, color = palette[2]) +
  style +
  scale_x_continuous(breaks = seq(-5, 3, by = 1)) +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "", x = expression(delta))

# both in one graph
boxplot_param <- grid.arrange(boxplot_alpha, boxplot_delta, ncol = 2)

#'------------------------------------------------------------------------------

# ICC: Both items in one plot 

m3_icc_data <- plot(model3, items = 213:214, low = -6, high = 6, 
                    ngroups = 10, groups_by_item = TRUE,
                    export = FALSE, ask = FALSE)
saveRDS(m3_icc_data, file = paste0(getwd(), "/YZM-Thesis/Output/m3_icc_data.rds"))

# Checkpoint
m3_icc_data <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/m3_icc_data.rds"))


# Create data.frames for the subsequent plot
icc_df <- data.frame(item = c(rep("CR Completeness", 100), rep("MC Completeness", 100)),
                     theta = rep(m3_icc_data$type_expected$theta, 2),
                     exp = c(m3_icc_data$type_expected$expScore[, 214], m3_icc_data$type_expected$expScore[, 213]))

obs_df <- data.frame(item = c(rep("CR Completeness", 10), rep("MC Completeness",10)),
                     theta = rep(m3_icc_data$type_expected$wle_obs_plotted, 2),
                     obs = c(m3_icc_data$type_expected$obScore[["completeness_CR"]]$x,
                             m3_icc_data$type_expected$obScore[["completeness_MC"]]$x))

# Plot

icc_plot <- ggplot() +
  geom_line(data = icc_df, aes(x = theta, y = exp, color = item),
            linewidth = 0.75) +
  geom_point(data = obs_df, aes(x = theta, y = obs, color = item)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(breaks = seq(-6, 6, by = 1)) +
  labs(x = expression(theta), y = "P(Completeness)",
       color = "Indicator") +
  style +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80")) +
  theme(strip.background = element_blank()) +
  theme(legend.text=element_text(size=12,  family="serif"),
        legend.title=element_text(size=12,  family="serif")) +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom") + 
  scale_color_manual(values = palette[2:3],
                     name = "",
                     labels = c("CR Completeness", "MC Completeness")) +
  ggtitle("Item Characteristic Curves") +
  theme(plot.title = element_text(size = 14, family = "serif"))

#'------------------------------------------------------------------------------

# IIC: Both items in one plot

m3_info <- IRT.informationCurves(model3, items = 213:214, curve_type = "test") # this gives an object usable to plot the IIC and TIC
saveRDS(m3_info, file = paste0(getwd(), "/YZM-Thesis/Output/m3_info.rds"))

# Checkpoint
m3_info <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Output/m3_info.rds"))

# IIC summary across items

df <- data.frame(item = rownames(m3_info$info_curves_item),
                 maxInfo = apply(m3_info$info_curves_item, 1, max))
summary(df)


# Create data.frame for the subsequent plot
iic_df <- data.frame(item = c(rep("CR Completeness", 100), rep("MC Completeness", 100)),
                     theta = rep(m3_info$theta, 2),
                     info = c(m3_info$info_curves_item[214,],m3_info$info_curves_item[213,]))

# Plot
iic_plot <- ggplot() +
  geom_line(data = iic_df, aes(x = theta, y = info, color = item),
            linewidth = 0.75) +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05)) +
  scale_x_continuous(breaks = seq(-6, 6, by = 1)) +
  labs(x = expression(theta), y = "I(θ)",
       color = "Indicator") +
  style +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80")) +
  theme(strip.background = element_blank()) +
  theme(legend.text=element_text(size=12,  family="serif"),
        legend.title=element_text(size=12,  family="serif")) +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom") + 
  scale_color_manual(values = palette[2:3],
                     name = "",
                     labels = c("CR Completeness", "MC Completeness")) +
  ggtitle("Item Information Curves") +
  theme(plot.title = element_text(size = 14, family = "serif"))

#'------------------------------------------------------------------------------

# Format them to put them all together in one graph
irt_legend <- get_legend(icc_plot)
a <- icc_plot+theme(legend.position = "none")
b <- iic_plot+theme(legend.position = "none")

g1 <- ggplotGrob(boxplot_alpha)
g2 <- ggplotGrob(boxplot_delta)
g3 <- ggplotGrob(a)
g4 <- ggplotGrob(b)

# Align widths
g1$widths <- g3$widths
g2$widths <- g4$widths

boxplot_row <- arrangeGrob(g1, g2, ncol = 2, 
                           top = textGrob("Distributions of Item Parameters", 
                                          gp = gpar(fontsize = 14, fontfamily = "serif")))
irt_plots <- grid.arrange(boxplot_row, g3, g4, irt_legend,
                          layout_matrix = rbind(c(1,1), c(2,3), c(4,4)),
                          heights = c(3, 10, 0.5))

# Export
ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/irt_plots.svg"), irt_plots, width = 25, height = 15, units = "cm", device = "svg") # noth happy with this
