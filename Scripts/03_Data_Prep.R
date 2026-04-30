
#' ---
#' title: "Subset Cleaning and Preparation"
#' author: "Ylva Matejko"
#' date: "Mar 28, 2025"
#' ---

#'Purpose of this script: Prepare the three scored datasets for analysis. 
#'
#'Essentially, for each version of the dataset, variables irrelevant to further analyses are removed. As a key step, items that were dropped from scaling 
#'in TIMSS 2023 were removed. According to the User Guide (Fishbein et al., 2025), not all variables were included in achievement scaling, e.g. because they 
#'were merely parts of multi-part constructed response items. The scaling status was documented in the Item Information Excel Sheet appended in the TIMSS 2023 
#'Technical Report. Additionally, one subset of data will be prepared for computation of descriptive statistics.
#'
#'The output of these scripts are three versions of the dataset to be used in analyses:
#'
#'1. g8s_v1 treats item omissions as not administered (default in TIMSS Scoring Program)
#'2. g8s_v2 treats item omissions as incorrect (0 points)
#'3. g8s_v3 will treat item omissions with Modified Laplace Smoothing
#'4. g8s_v0 includes scored items, but keeps the original codes for not-reached and omitted items.
#'
#'------------------------------------------------------------------------------
#'
#' ### Subset cleaning function
#' 
#' This function removes all rows and columns not relevant to the analysis in the
#' present study. The remaining variables are then the science items and the country 
#' IDs, unless one opts in for other demographic variables.
#' 

clean_g8s <- function(df, drop_demo = TRUE) {
  
  # 1. Filter out math items, math PVs and benchmarks, and science PVs, benchmarks and TIMSS nonresponse indicators
  df <- df[, !grepl("^M", names(df))] # math items 
  df <- df[, !grepl("^BSM", names(df))] # math PVs and benchmarks
  df <- df[, !grepl("^BSS", names(df))] # science PVs and benchmarks
  
  nonresponse_indicators <- colnames(df)[grep(pattern = "^SE.2[A-Z]{3}(NEN)?$", x = colnames(df))]
  df <- df[, !colnames(df) %in% nonresponse_indicators] # TIMSS nonresponse indicators
  
  # 2. Exclude item-parts and items not included in scaling (Fishbein et al., 2025)
  drop_vars <- c("SE72345A", "SE72345B", "SE72345C", "SE72345D", "SE72345E", "SE72345F", "SE72345G", "SE72403A", "SE72403B", 
                 "SE72403C", "SE72403D", "SE62018A", "SE62018B", "SE62018C", "SE62018D", "SE62018E", "SE82025A", "SE82025B", 
                 "SE82025C", "SE82025D", "SE82275A", "SE82275B", "SE82275C", "SE82343A", "SE82343B", "SE82343C", "SQ82T01A", 
                 "SQ82T01B", "SQ82T01C", "SQ82T01D", "SQ82T01E", "SQ82T02AA", "SQ82T02AB", "SQ72S07A", "SQ72S07B", "SQ72S09A", 
                 "SQ72S09B", "SQ72S09C", "SE82008A", "SE82008B", "SE82008C", "SE82008D", "SE82008E", "SE82008F", "SE82008G", 
                 "SE82008H", "SE82021A", "SE82021B", "SE82021C", "SE82021D", "SE82104A", "SE82104B", "SE82104C", "SE82104D", 
                 "SE82172A", "SE82172B", "SE82172C", "SE82172D", "SE82175A", "SE82175B", "SE82175C", "SE82175D", "SE82175E", 
                 "SE82205A", "SE82205B", "SE82205C", "SE82205D", "SE82323A", "SE82323B", "SE82323C", "SE82341A", "SE82341B", 
                 "SE82341C", "SE82341D", "SE82341E", "SE82001A", "SE82001B", "SE82001C", "SE82001D", "SE82001E", "SE82040A", 
                 "SE82040B", "SE82040C", "SE82040D", "SE82040E", "SE82060A", "SE82060B", "SE82064A", "SE82064B", "SE82064C", 
                 "SE82064D", "SE82327A", "SE82327B", "SE82327C", "SE82327D", "SE82340A", "SE82340B", "SE82340C", "SE82340D", 
                 "SE82340E", "SE72000A", "SE72000B", "SE72000C", "SE72000D", "SE72000E", "SE72143A", "SE72143B", "SE72143C", 
                 "SE72143D", "SE72906", "SE72906A", "SE72906B", "SE72906C", "SE72906D", "SE72906E", "SE82002A", "SE82002B", 
                 "SE82002C", "SE82002D", "SE82002E", "SE82200AA", "SE82200AB", "SE82200AC", "SE82200BA", "SE82200BB", "SE82200BC", 
                 "SE82244A", "SE82244B", "SE82244C", "SE82271A", "SE82271B", "SE82271C", "SE82271D", "SE62022A", "SE62022B", 
                 "SE62022C", "SE62022D", "SE62042A", "SE62042B", "SE62042C", "SE62042D", "SE62047A", "SE62047B", "SE62047C", 
                 "SE62101A", "SE62101B", "SE62101C", "SE62101D", "SE62243A", "SE62243B", "SE62243C", "SE62243D", "SE62266", 
                 "SE82005A", "SE82005B", "SE82005C", "SE82005D", "SE82005E", "SE82263A", "SE82263B", "SE82077A", "SE82077B", 
                 "SE82077C", "SE82246A", "SE82246B", "SE82246C", "SE82246D", "SQ82L01A", "SQ82L01B", "SQ82L01C", "SQ82L01D", 
                 "SQ82L01E", "SQ82L01F", "SQ82L08", "SE62006A", "SE62006B", "SE62006C", "SE62036", "SE62242A", "SE62242B", 
                 "SE62242C", "SE62242D", "SE62242E", "SE72033A", "SE72033B", "SE72033C", "SE72033D", "SE72033E", "SE72048", 
                 "SE72220", "SE72261A", "SE72261B", "SE72261C", "SE72261D", "SE72261E", "SE72016A", "SE72016B", "SE72905A", 
                 "SE72905B", "SE72905C", "SE72905D", "SP72403", "SP72345A", "SP72345B", "SP72345C", "SP72345D", "SP72345E", 
                 "SP72345F", "SP72345G", "SP72133", "SP72265A", "SP72265B", "SP72265C", "SP72265D", "SP72265E", "SP62018A", 
                 "SP62018B", "SP62018C", "SP62018D", "SP62018E", "SP72048", "SP72261A", "SP72261B", "SP72261C", "SP72261D", 
                 "SP72261E", "SP62036", "SP62242A", "SP62242B", "SP62242C", "SP62242D", "SP62242E", "SP72906", "SP72906A", 
                 "SP72906B", "SP72906C", "SP72906D", "SP72906E", "SP72329", "SP62266", "SP62047A", "SP62047B", "SP62047C", 
                 "SP62022A", "SP62022B", "SP62022C", "SP62022D", "SP72016A", "SP72016B")
  df <- df[, !names(df) %in% drop_vars]
  
  # 3. Exclude paper countries and items
  g8_paper_countries <- c(384, 364, 414, 710)
  df <- df[!df$IDCNTRY %in% g8_paper_countries, ] # paper countries
  df <- df[, !grepl("^SP", colnames(df))] # paper items
  
  # 4. Filter out technical and administrative variables
  drop_admin <- c("ITASSESS", "ITADMINI", "ILRELIAB", "ITLANG_SA", "LCID_SA",
                  "BNRGCAL1", "BNRGCAL2", "VERSION", "idbid", "SCOPE")
  df <- df[, !names(df) %in% drop_admin]
  
  # 5. Filter out sampling and weight variables, except total weight
  drop_sampling <- c("HOUWGT", "SENWGT", "WGTFAC1", "WGTADJ1", 
                     "WGTFAC2", "WGTADJ2", "WGTFAC3", "WGTADJ3",
                     "JKZONE", "JKREP")
  df <- df[, !names(df) %in% drop_sampling]
  
  # 6. Optionally filter out demographic variables
  if (drop_demo) {
    drop_demo <- c("CTY", "IDPOP", "IDGRADER", "IDGRADE", "IDBOOK", 
                   "IDSCHOOL", "IDCLASS", "IDSTUD", "ITSEX", "BSDAGE")
    df <- df[, !names(df) %in% drop_demo] }

  return(df)
}


#'------------------------------------------------------------------------------
#'
#' ### Load data


load(file = paste0(getwd(), "/YZM-Thesis/Data/GRADE8DATA_COMPLETE_SCR_v1.Rdata"))
g8s_v1 <- GRADE8DATA_COMPLETE_SCR_v1

load(file = paste0(getwd(), "/YZM-Thesis/Data/GRADE8DATA_COMPLETE_SCR_v2.Rdata"))
g8s_v2 <- GRADE8DATA_COMPLETE_SCR_v2

load(file = paste0(getwd(), "/YZM-Thesis/Data/GRADE8DATA_COMPLETE_SCR_v3.Rdata"))
g8s_v3 <- GRADE8DATA_COMPLETE_SCR_v3

load(file = paste0(getwd(), "/YZM-Thesis/Data/GRADE8DATA_COMPLETE_SCR_v0.Rdata"))
g8s_v0 <- GRADE8DATA_COMPLETE_SCR_v0

rm(list = c("GRADE8DATA_COMPLETE_SCR_v0", "GRADE8DATA_COMPLETE_SCR_v1", 
            "GRADE8DATA_COMPLETE_SCR_v2", "GRADE8DATA_COMPLETE_SCR_v3"))

#'------------------------------------------------------------------------------
#'
#' ### Apply function

g8s_v1 <- clean_g8s(g8s_v1)
g8s_v2 <- clean_g8s(g8s_v2)
g8s_v3 <- clean_g8s(g8s_v3)
g8s_v0 <- clean_g8s(g8s_v0, drop_demo = FALSE)

#'------------------------------------------------------------------------------
#'
#' ### Quick check
#' For more checks, see 03_Data_Prep_Check.R

colnames(g8s_v3) #remaining vars as of now 

items <- colnames(g8s_v3)[grepl("^S..2", colnames(g8s_v3))] # 212 remaining items is correct according to Aldrich et al. (2024)
length(items)


#' ----------------------------------------------------------------
#' 
#' ### Save files

saveRDS(g8s_v1, 
     file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v1.rds"))

saveRDS(g8s_v2, 
        file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v2.rds"))

saveRDS(g8s_v3, 
        file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v3.rds"))

saveRDS(g8s_v0, 
        file = paste0(getwd(), "/YZM-Thesis/Data/g8s_v0.rds"))

#'------------------------------------------------------------------------------
#'
#' ### References
#' 
#' Fishbein, B., Taneva, M., & Kowolik, K. (2025). TIMSS 2023 User Guide for the International Database. Boston College, TIMSS & PIRLS International Study Center. https://timss2023.org/data
#'
#' Aldrich, C., Bookbinder, A., & Khorramdel, L. (2024). Developing the TIMSS mathematics and science achievement instruments. TIMSS & PIRLS International Study Center, Boston College. https://doi.org/10.6017/lse.tpisc.timss.rs3063


