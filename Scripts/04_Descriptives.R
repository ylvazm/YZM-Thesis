
#' ---
#' title: "Descriptive Statistics"
#' author: "Ylva Matejko"
#' date: "Mar 28, 2025"
#' ---
#' 
#' This script produces the descriptive statistics for the full sample in the analyses.

library(ggplot2)

#'------------------------------------------------------------------------------
#'
#' ### Load data

g8s_v0 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v0.rds"))
g8s_v3 <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v3.rds"))

ctrycodes <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/countrycodes.csv"), sep = ";", header = F)
colnames(ctrycodes) <- c("Country","ISO","IDCNTRY")

#'------------------------------------------------------------------------------
#'
#' ### Description of the sample
#' 

# Quick Check
summary(g8s_v0[,2:11]) # Sex and Age contain invalid codes 9 and 99

g8s_v0$BSDAGE[g8s_v0$BSDAGE == 99 | g8s_v0$BSDAGE == 9] <- NA
g8s_v0$ITSEX[g8s_v0$ITSEX == 9]    <- NA

# Compute descriptives
sample_desc <- data.frame(
  IDCNTRY = sort(unique(g8s_v0$IDCNTRY)),
  n_schools = tapply(g8s_v0$IDSCHOOL, g8s_v0$IDCNTRY, function(x) length(unique(x))),
  n_classes = tapply(g8s_v0$IDCLASS,  g8s_v0$IDCNTRY, function(x) length(unique(x))),
  n_students = tapply(g8s_v0$IDSTUD,   g8s_v0$IDCNTRY, length),
  pct_female = tapply(g8s_v0$ITSEX, g8s_v0$IDCNTRY, function(x) round(100 * mean(x == 1, na.rm = TRUE), 2)),
  pct_male = tapply(g8s_v0$ITSEX, g8s_v0$IDCNTRY, function(x) round(100 * mean(x == 2, na.rm = TRUE), 2)),
  age_M  = round(tapply(g8s_v0$BSDAGE, g8s_v0$IDCNTRY, mean, na.rm = TRUE), 2),
  age_SD = round(tapply(g8s_v0$BSDAGE, g8s_v0$IDCNTRY, sd,   na.rm = TRUE), 2)
)

# Note: Percentages are based on students with valid sex information. A total of 32 students (0.01%) were excluded from this calculation due to missing or invalid sex codes.

#'------------------------------------------------------------------------------
#'
#' ### Description of Missingness Patterns across Countries
#'

# Function that counts the number of incidentally missing values per country

analyse_missings <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/analyse_missings.rds"))

# Compute omissions and not reached per country
items <- colnames(g8s_v0)[grep(pattern="^S", x=colnames(g8s_v0))]
missings_clean <- analyse_missings(g8s_v0[, c("IDCNTRY", items)])

# Quick check
all(missings_clean$IDCNTRY == sort(unique(g8s_v3$IDCNTRY))) # if TRUE, the resulting df from analyse_missings is sorted in the same order as the descriptive df

# Create table with missings and missingness indicators per country
missing_desc <- data.frame(
  IDCNTRY = sort(unique(g8s_v3$IDCNTRY)),
  omissions = missings_clean$Omitted_Invalid,
  not_reached = missings_clean$Not_Reached,
  completeness_MC_M = round(tapply(g8s_v3$completeness_MC, g8s_v3$IDCNTRY, mean, na.rm = TRUE)*100, 2),
  completeness_MC_SD = round(tapply(g8s_v3$completeness_MC, g8s_v3$IDCNTRY, sd, na.rm = TRUE)*100, 2),
  completeness_CR_M = round(tapply(g8s_v3$completeness_CR, g8s_v3$IDCNTRY, mean, na.rm = TRUE)*100,2),
  completeness_CR_SD = round(tapply(g8s_v3$completeness_CR, g8s_v3$IDCNTRY, sd, na.rm = TRUE)*100, 2)
)

#'------------------------------------------------------------------------------
#'
#' ### Descriptive table

# Merge the data.frames
final_desc <- merge(sample_desc, missing_desc, by = "IDCNTRY")

# Substitute IDCNTRY with ISO codes
final_desc <- merge(ctrycodes, final_desc, by = "IDCNTRY")
final_desc <- final_desc[,3:16]

# Compute missing rates across countries
missings_total <- analyse_missings(
  cbind(IDCNTRY = rep("Total", nrow(g8s_v0)), g8s_v0[, items]) # substituting all country IDs with "Total" to obtain only one value per category
)

# Add total row
total_row <- data.frame(
  ISO = "Total",
  n_schools = nrow(unique(g8s_v0[, c("IDCNTRY", "IDSCHOOL")])),
  n_classes = nrow(unique(g8s_v0[, c("IDCNTRY", "IDSCHOOL", "IDCLASS")])),
  n_students = nrow(g8s_v0),
  pct_female = round(100 * mean(g8s_v0$ITSEX == 1, na.rm = TRUE), 2),
  pct_male = round(100 * mean(g8s_v0$ITSEX == 2, na.rm = TRUE), 2),
  age_M = round(mean(g8s_v0$BSDAGE, na.rm = TRUE), 2),
  age_SD = round(sd(g8s_v0$BSDAGE, na.rm = TRUE), 2),
  omissions = missings_total$Omitted_Invalid,
  not_reached = missings_total$Not_Reached,
  completeness_MC_M = round(mean(g8s_v3$completeness_MC, na.rm = TRUE)*100, 2),
  completeness_MC_SD = round(sd(g8s_v3$completeness_MC, na.rm = TRUE)*100, 2),
  completeness_CR_M = round(mean(g8s_v3$completeness_CR, na.rm = TRUE)*100, 2),
  completeness_CR_SD = round(sd(g8s_v3$completeness_CR, na.rm = TRUE)*100, 2)
)

final_desc <- rbind(final_desc, total_row)

#'------------------------------------------------------------------------------
#'
#' ### Save data

write.csv(final_desc, file = paste0(getwd(), "/YZM-Thesis/Data/final_desc.csv"), row.names = FALSE)

#'------------------------------------------------------------------------------
#'
#' ### Plot of omission rates, sorted by MC and CR

#' Prepare plot data

final_desc <- read.csv(file = paste0(getwd(), "/YZM-Thesis/Data/final_desc.csv")) # checkpoint

MC_items <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/MC_items.rds"))
CR_items <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/CR_items.rds"))

missings_MC <- analyse_missings(g8s_v0[, c("IDCNTRY", MC_items)])
missings_CR <- analyse_missings(g8s_v0[, c("IDCNTRY", CR_items)])

plot_desc <- final_desc[1:43,]

plot_data_1 <- data.frame(ISO = rep(plot_desc$ISO, 2),
                        item_type = c(rep("MC", nrow(missings_MC)), rep("CR", nrow(missings_CR))),
                        omission_rate = c(missings_MC$Omitted_Invalid, missings_CR$Omitted_Invalid),
                        sort = plot_desc$omissions # sort by overall omission rate
)

saveRDS(plot_data_1, file = paste0(getwd(), "/YZM-Thesis/Data/plot_data_1.rds"))

#' Plot

omission_plot <- ggplot(data = plot_data_1, # sort by overall omission rate
       aes(x = reorder(ISO, sort), y = omission_rate, 
           fill = item_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", 
       y = "Omission Rate (%)", 
       fill = "Item Type") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), # Put 0 line on the x axis
                     breaks = seq(0, max(plot_data_1$omission_rate), by = 5)) + # control breaks
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(legend.text=element_text(size=12,  family="serif"),
        legend.title=element_text(size=12,  family="serif")) +
  theme(axis.text.x = element_text(size=12,  family="serif", angle = 45, hjust = 1), 
        axis.text.y = element_text(size=12,  family="serif"),
        axis.title = element_text(size=14,  family="serif"),
        strip.text = element_text(size=12,  family="serif"))

# Export 
ggsave(file = paste0(getwd(), "/YZM-Thesis/Output/omission_plot.svg"), missings_plot, width = 25, height = 10, unit = "cm")




