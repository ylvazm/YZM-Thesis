
#' ---
#' title: "Useful Functions and Objects for the Present Analysis"
#' author: "Ylva Matejko"
#' date: "Mar 05, 2026"
#' ---

#' The purpose of this script is to document all functions and objects I created nd used frequently throughout the process of analysing the TIMSS data for my thesis.
#' No need to run the script; all relevant objects are stored in the folder "Objects" and sourced from there.

library(ggplot2)

#'------------------------------------------------------------------------------
#'
#'### Function that counts the number of unplanned missing values per country

#' Codes:
#' 6 & 96 = Not Reached;
#' 9 & 99 = Omitted or Invalid (Non-Codable) Response;
#' NA = Missing-by-design;
#' All other numbers = Valid Response.

analyse_missings <- function (df) { #This function counts the category codes per country
  
  item_cols <- setdiff(names(df), "IDCNTRY")
  
  # split df per country
  by_country <- split(df[item_cols],df[["IDCNTRY"]])
  
  # count incidentally missing via category codes
  missings <- lapply(by_country, function(d){
    v <- as.numeric(as.character(unlist(d, use.names = FALSE))) # creates a vector with all values recorded per country
    v_rec <- ifelse(v %in% c(6,96), "Not_Reached", # recodes the vector using the three categories
                    ifelse(v %in% c(9,99), "Omitted_Invalid",
                           ifelse(!is.na(v),      "Valid_Response", NA)))
    v_rec <- v_rec[!is.na(v_rec)] # removes NAs from recoded vector because they were not administered
    
    round(100 * prop.table(table(v_rec)), 2) # computes percentages per category per country
    
  })
  res <- do.call(rbind, missings) # binds the country codes and percentage values into a new table
  data.frame(IDCNTRY = names(missings), res, row.names = NULL, check.names = FALSE)
}

saveRDS(analyse_missings, file = paste0(getwd(), "/YZM-Thesis/Objects/analyse_missings.rds"))

#'------------------------------------------------------------------------------
#'
#'### Vectors for categorising item groups
#'
#' All of these are based on the Item Information Excel Sheet appended in the TIMSS 2023 Technical Report (Fishbein et al., 2025)

# Objects to store the variable names for MC and CR items

MC_items <- c("SE72002", "SE72021", "SE72063", "SE72066", "SE72234", "SE72251", "SE72349", "SE72363", "SE62024A", "SE62099", 
              "SE62106", "SE62132", "SE62153", "SE62190", "SE62205", "SE82250", "SE82347", "SQ72S05", "SQ72S07C", "SQ72S13A", 
              "SE82092", "SE82280", "SE82020", "SE82050", "SE82143", "SE82203", "SE82224", "SE82233", "SE82264", "SE72038", 
              "SE72120", "SE72168", "SE72205", "SE72280B", "SE72329", "SE72370", "SE72460", "SE72901", "SE82042", "SE82043", 
              "SE82070", "SE82073", "SE82108", "SE82137", "SE82144", "SE82335", "SE62091A", "SE62091B", "SE62097", "SE62180", 
              "SE62235", "SE62246", "SE82007", "SE82027", "SE82045", "SE82080", "SE82138", "SE82170", "SE82225", "SE82281", 
              "SE82304", "SE82307", "SE82721", "SE82103", "SE62037", "SE62089", "SE62119", "SE62177", "SE62186", "SE62247", 
              "SE62279", "SE72031", "SE72032", "SE72116", "SE72123", "SE72231", "SE72720", "SE72011", "SE72049", "SE72091", 
              "SE72140", "SE72209", "SE72249", "SE72303", "SE72323", "SE72368")

CR_items <- c("SE72141B", "SE72345", "SE62018", "SE62095", "SQ82T02A", "SQ72S13B", "SE82060", "SE72000", "SE72523", "SE62101", 
              "SE62243", "SE82263", "SE62093", "SE72005", "SE72033", "SE72920", "SE72016", "SE72210", "SE72082", "SE72102", 
              "SE72141A", "SE72284", "SE72403", "SE72921", "SE62024B", "SE62050", "SE62064", "SE62143", "SE62163", "SE62276", 
              "SE82025", "SE82131", "SE82171", "SE82273A", "SE82273B", "SE82275", "SE82343", "SE82902", "SQ82T01", "SQ82T02B", 
              "SQ82T03A", "SQ82T03B", "SQ82T04", "SQ72S01", "SQ72S02", "SQ72S03", "SQ72S04", "SQ72S06", "SQ72S08", "SQ72S09", 
              "SQ72S10", "SQ72S11", "SQ72S12", "SE82008", "SE82021", "SE82068A", "SE82068B", "SE82071", "SE82099", "SE82104", 
              "SE82172", "SE82175", "SE82205", "SE82262", "SE82323", "SE82325", "SE82341", "SE82001", "SE82040", "SE82064", 
              "SE82147", "SE82223", "SE82327", "SE82340", "SE72078", "SE72143", "SE72280A", "SE72293", "SE82002", "SE82200A", 
              "SE82200B", "SE82244", "SE82271", "SE82346A", "SE82346B", "SE62022", "SE62042", "SE62047", "SE62056", "SE62100", 
              "SE62128", "SE62250", "SE82005", "SE82075", "SE82333", "SE82077", "SE82146", "SE82246", "SE82331", "SQ82L01", 
              "SQ82L02A", "SQ82L02B", "SQ82L04A", "SQ82L04B", "SQ82L05", "SQ82L07A", "SQ82L07B", "SQ82L09A", "SQ82L09B", 
              "SE62006", "SE62033", "SE62067", "SE62112", "SE62211A", "SE62211B", "SE62242", "SE72086", "SE72261", "SE72294", 
              "SE72348", "SE72440", "SE72074", "SE72109", "SE72132", "SE72451", "SE72905")

saveRDS(MC_items, file = paste0(getwd(), "/YZM-Thesis/Objects/MC_items.rds"))
saveRDS(CR_items, file = paste0(getwd(), "/YZM-Thesis/Objects/CR_items.rds"))

#'------------------------------------------------------------------------------
#'
#'### Style object for ggplots

style <- theme_bw() +
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(size=12,  family="serif"), 
        axis.text.y = element_text(size=12,  family="serif"),
        axis.title = element_text(size=12,  family="serif"),
        strip.text = element_text(size=12,  family="serif"),
        panel.grid.minor = element_blank())

saveRDS(style, file = paste0(getwd(), "/YZM-Thesis/Objects/style.rds"))

#' ### References
#' 
#' Fishbein, B., Taneva, M., & Kowolik, K. (2025). TIMSS 2023 User Guide for the International Database. Boston College, TIMSS & PIRLS International Study Center. https://timss2023.org/data
#' 
