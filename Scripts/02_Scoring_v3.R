
#' ---
#' title: "Scoring of TIMSS 2023 Grade 8 Achievement Data -  Scoring Omitted Items for Modified Laplace Smoothing"
#' author: "Ylva Matejko"
#' date: "Mar 21, 2025"
#' ---

#' This script creates a TIMSS 2023 grade 8 response dataset where omitted items are treated as not administered. 
#' It saves the resulting data.frame as GRADE8DATA_COMPLETE_v3_SCR.Rdata.
#' 
#' The scoring algorithm corresponds to the one suggested in the TIMSS 2023 International Database User Guide (Fishbein et al., 2025), with minor changes to the desired file names 
#' to achieve the desired handling of item omissions. 
#' 
#' This script does not differ substantially from 02_Scoring_BSASCRM8_v1.R, since the applied scoring algorithm is the same. However, the resulting data file will differ from
#' GRADE8DATA_COMPLETE_v1_SCR.Rdata because it has additional Missingness Indicator Variables that are computed before scoring. 

library(haven)

#' ### Load data

load(paste0(getwd(), "/YZM-Thesis/Data/GRADE8DATA_ORIG.Rdata"))

#' ### Preparation of the dataset for Scoring
#' 
#' This section of the script adjusts the file GRADE8DATA_ORIG.Rdata to avoid errors in the scoring process due to the data format.

GRADE8DATA_COMPLETE <- GRADE8DATA_ORIG

GRADE8DATA_COMPLETE[] <- lapply(GRADE8DATA_COMPLETE, function(col) { # adjusting the format of each column one-by-one
  if (inherits(col, "haven_labelled")) as.numeric(col) else col
})

#' ### Creation of a missingness indicator variable 
#'  
#' Before the item omission codes are overridden with scores, they have to be counted per person and stored in an additional variable. 

# A function that checks each row of items for omissions (codes 9 and 99) and creates missingness indicators
count_omissions <- function(data, items) { # input: dataset and a vector with the variable names
  all_na <- which(rowSums(!is.na(data[, items])) == 0) # unit nonresponse will be reflected in the final missingness indicators
  result <- as.integer(rowSums(data[, items] == 9 | data[, items] == 99, na.rm = TRUE) == 0) # check if there are any omissions, returns 1 if there are none and 0 if there are any
  result[all_na] <- NA # overrides the cases with unit nonresponse
  result
}

MC_items <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/MC_items.rds"))
CR_items <- readRDS(file = paste0(getwd(), "/YZM-Thesis/Objects/CR_items.rds"))

# Using the function to create missingness indicator variables
GRADE8DATA_COMPLETE$completeness_MC <- count_omissions(GRADE8DATA_COMPLETE, MC_items) # completeness of MC items  
GRADE8DATA_COMPLETE$completeness_CR <- count_omissions(GRADE8DATA_COMPLETE, CR_items) # completeness of CR items

# some checks
summary(GRADE8DATA_COMPLETE[,1147:1148]) # some notable differences in omissions between MC and CR

table(GRADE8DATA_COMPLETE$completeness_MC, GRADE8DATA_COMPLETE$completeness_CR, # crosstable of both indicator variables
      dnn = c("MC_complete", "CR_complete"))

cor(GRADE8DATA_COMPLETE$completeness_MC, GRADE8DATA_COMPLETE$completeness_CR, use = "complete.obs") # correlation of the indicator variables

# Save file
save(GRADE8DATA_COMPLETE, 
     file = paste0(getwd(), "/YZM-Thesis/Data/GRADE8DATA_COMPLETE.Rdata"))
#' 
#' -----------------------------------------------------------------------------
#' 
#' ### R Program to Score the TIMSS 2023 8th Grade Items (Fishbein et al., 2025)

###############################################################################
#                                                                             #                                                                            
#             R  Program  to  Score  the  TIMSS  2023  8th  Grade  Items      #
#                             TIMSS 2023  User  Guide                         #
#                                                                             #                                                                               
#             For  use  with  all TIMSS  2023  files                          #
#                                                                             #                                                                               
###############################################################################
library(tidyverse)

scoreit <- function(data, item, type, right, NR, OM, other){
  
  #  Code  for  multiple-choice  items
  if  (toupper(type)=="MC"){
    data <- data %>% mutate_at(vars(item), ~case_when ( . == right ~ 1,   # Item Key
                                                        . == NR    ~ NaN, # Not Reached
                                                        . == OM    ~ NaN, # Omitted
                                                        is.na(.)   ~ NaN, # Not Administered
                                                        . == other ~ 0,   # Other Missing
                                                        TRUE       ~ 0))
  }
  
  #  Code  for  constructed-response  items
  if  (toupper(type)=="CR"){
    data <- data %>% mutate_at(vars(item), ~case_when ( . ==2 | (. >= 20 & . < 30)  ~ 2, # Full Credit
                                                        . == 1 | (. >= 10 & . < 20)  ~ 1, # Full or Partial Credit
                                                        . == 0 | (. >= 70 & . < 80)  ~ 0, # No Credit
                                                        . %in% NR ~ NaN,    # Not Reached
                                                        . %in% OM ~ NaN,    # Omitted
                                                        is.na(.) ~ NaN,   # Not Administered
                                                        . %in% other ~ 0,   # Other Missing
                                                        TRUE       ~ 0))
  }
  return(data)
}

doit <- function(indir=getwd(), outdir=getwd(), infile=""){
  output_file <- paste0(outdir,infile,"_SCR_v3.Rdata") # file name adjusted
  input_file <- paste0(indir,infile,".Rdata")
  
  #  Get  the  student  achievement  data
  load(input_file)
  data<-eval(as.symbol(infile))
  
  #  Score  multiple-choice  items  with  A  key
  #  Digital  items
  Aright <- c('ME72055B', 'ME72055D', 'ME72055F', 'ME72090',  'ME82808',  'ME62095',  'ME62076',  
              'ME82623A', 'ME82623C', 'ME82623F', 'ME62146',  'ME62242',  'ME82314B', 'ME82314C', 
              'ME72178B', 'ME72178C', 'ME72178E', 'ME72020C', 'ME72027',  'ME72164A', 'ME72164D', 
              'ME72080',  'ME72081A', 'ME72081C', 'ME72140B', 'ME72140D', 'ME72140F', 'ME72192',  
              'ME72125',  'ME72232A', 'ME72232D', 
              'SE72251',  'SE72345A', 'SE72345F', 'SE72349',  'SE62018B', 'SE62018E', 'SE62205',  
              'SE62024A', 'SE82025B', 'SE82025D', 'SE82275B', 'SE82343A', 'SE82347',  'SQ82T01B', 
              'SQ82T01D', 'SQ72S07A', 'SQ72S07B', 'SQ72S09C', 'SE82008B', 'SE82008D', 'SE82008H', 
              'SE82175A', 'SE82175D', 'SE82175E', 'SE82104C', 'SE82172A', 'SE82172C', 'SE82205B', 
              'SE82205D', 'SE82341A', 'SE82341C', 'SE82064B', 'SE82040A', 'SE82040D', 'SE82040E', 
              'SE82264',  'SE82340B', 'SE82340D', 'SE82340E', 'SE82327A', 'SE82327B', 'SE82327D', 
              'SE72460',  'SE72038',  'SE72143A', 'SE82144',  'SE82200AB','SE82200BC','SE82244C', 
              'SE82271D', 'SE62097',  'SE62101D', 'SE62047A', 'SE62047C', 'SE62042A', 'SE62042D', 
              'SE62235',  'SE62180',  'SE62022A', 'SE62022B', 'SE62022C', 'SE62243C', 'SE82027',  
              'SE82005A', 'SE82005C', 'SE82281',  'SE82225',  'SE82307',  'SE82721',  'SQ82L01D', 
              'SQ82L01E', 'SE82077B', 'SE82077C', 'SE82246A', 'SE82246D', 'SE62279',  'SE62037',  
              'SE62242A', 'SE62242E', 'SE72116',  'SE72261A', 'SE72261B', 'SE72261D', 'SE72720',  
              'SE72011',  'SE72209')
  
  #  Paper  items
  Aright <- c(Aright,
              'MP72027',  'MP72164A', 'MP72164D', 'MP72090',  'MP62245',  'MP72125',  'MP72232A', 
              'MP72232D', 'MP62095',  'MP62076',  'MP62146',  'MP62242',  'MP72080',  'MP72140B', 
              'MP72140D', 'MP72140F', 'MP72192',  
              'SP72251',  'SP72345A', 'SP72345F', 'SP72349',  'SP72265A', 'SP72265C', 'SP72347',  
              'SP72367',  'SP62018B', 'SP62018E', 'SP62205',  'SP62024A', 'SP72116',  'SP72261A', 
              'SP72261B', 'SP72261D', 'SP72720',  'SP62279',  'SP62037',  'SP62242A', 'SP62242E', 
              'SP72460',  'SP72038',  'SP62097',  'SP62047A', 'SP62047C', 'SP62235',  'SP62180',  
              'SP62022A', 'SP62022B', 'SP62022C', 'SP72011',  'SP72209')
  
  data <- scoreit(data, item=Aright, type="MC", right=1, NR=6, OM=9, other=7)
  
  # Items with 6 choice options
  Aright <- c('SE72403B', 'SE82002E', 'SE72033D', 'SE72905C')
  
  data <- scoreit(data, item=Aright, type="MC", right=1, NR=96, OM=99, other=90)
  
  
  #  Score  multiple-choice  items  with  B  key
  #  Digital  items
  Bright <- c('ME72055A', 'ME72055C', 'ME72055E', 'ME72222',  'ME72233',  'ME72189',  'ME72221',  
              'ME72211A', 'MQ72D04B', 'ME82512',  'ME62230',  'ME82623B', 'ME82623D', 'ME82623E', 
              'ME82402',  'ME62001',  'ME82708B', 'ME82503',  'ME82314A', 'ME82314D', 'ME82314E', 
              'ME72178A', 'ME72178D', 'ME72020D', 'ME72067',  'ME72164B', 'ME72164C', 'ME72164E', 
              'ME72223',  'ME72081B', 'ME72081D', 'ME72140A', 'ME72140C', 'ME72140E', 'ME72232B', 
              'ME72232C', 
              'SE72021',  'SE72063',  'SE72345B', 'SE72345C', 'SE62099',  'SE62018A', 'SE62018D', 
              'SE82025A', 'SE82025C', 'SE82250',  'SE82275C', 'SE82343B', 'SE82343C', 'SQ82T01A', 
              'SQ82T01C', 'SQ82T01E', 'SQ72S05',  'SQ72S07C', 'SQ72S09B', 'SE82008C', 'SE82008F', 
              'SE82008G', 'SE82175B', 'SE82175C', 'SE82104A', 'SE82172B', 'SE82172D', 'SE82280',  
              'SE82205A', 'SE82205C', 'SE82341B', 'SE82323B', 'SE82020',  'SE82064C', 'SE82040B', 
              'SE82040C', 'SE82340A', 'SE82340C', 'SE82233',  'SE72901',  'SE72120',  'SE72143D', 
              'SE72280B', 'SE82070',  'SE82200AC','SE82200BA','SE82244A', 'SE82271B', 'SE62091A', 
              'SE62101B', 'SE62047B', 'SE62042B', 'SE62042C', 'SE62022D', 'SE62243D', 'SE82045',  
              'SE82005D', 'SE82170',  'SE82304',  'SQ82L01A', 'SQ82L01F', 'SE82077A', 'SE82103',  
              'SE82246B', 'SE82246C', 'SE62247',  'SE62186',  'SE62242B', 'SE62242D', 'SE72123',  
              'SE72261C', 'SE72261E', 'SE72049',  'SE72323')
  
  #  Paper  items
  Bright <- c(Bright,
              'MP72067',  'MP72164B', 'MP72164C', 'MP72164E', 'MP72222',  'MP72233',  'MP62329',  
              'MP62212',  'MP62284',  'MP72232B', 'MP72232C', 'MP62230',  'MP72189',  'MP72221',  
              'MP72211A', 'MP62001',  'MP72223',  'MP72140A', 'MP72140C', 'MP72140E', 
              'SP72021',  'SP72063',  'SP72345B', 'SP72345C', 'SP72443',  'SP72137',  'SP72265B', 
              'SP72265D', 'SP72265E', 'SP62099',  'SP62018A', 'SP62018D', 'SP72123',  'SP72261C', 
              'SP72261E', 'SP62247',  'SP62186',  'SP62242B', 'SP62242D', 'SP72901',  'SP72120',  
              'SP72280B', 'SP62091A', 'SP62047B', 'SP62022D', 'SP72049',  'SP72323')
  
  data <- scoreit(data, item=Bright, type="MC", right=2, NR=6, OM=9, other=7)
  
  # Items with 6 choice options
  Bright <- c('SE82021A', 'SE82001D', 'SE72000C', 'SE82002A', 'SE72033A', 'SE72905A')
  
  data <- scoreit(data, item=Bright, type="MC", right=2, NR=96, OM=99, other=90)
  
  
  #  Score  multiple-choice  items  with  C  key
  #  Digital  items
  Cright <- c('ME72188',  'ME72043',  'ME72150',  'MQ82C01',  'ME82617',  'ME62194',  'ME62067',  
              'ME62120',  'ME82510',  'ME72020A', 'ME72020B', 'ME72005',  'ME72154',  'ME72022',  
              'ME72013',  
              'SE72066',  'SE72345D', 'SE72345E', 'SE72345G', 'SE72363',  'SE62132',  'SE62153',  
              'SE62018C', 'SE82275A', 'SQ72S09A', 'SE82092',  'SE82008A', 'SE82008E', 'SE82104B', 
              'SE82104D', 'SE82341D', 'SE82341E', 'SE82323A', 'SE82064D', 'SE82050',  'SE82224',  
              'SE82203',  'SE82327C', 'SE72143B', 'SE72205',  'SE72370',  'SE82108',  'SE82137',  
              'SE82200AA','SE82200BB','SE82271C', 'SE82335',  'SE62091B', 'SE62101C', 'SE62246',  
              'SE62243A', 'SE82007',  'SE82005B', 'SE82005E', 'SQ82L01B', 'SQ82L01C', 'SE62119',  
              'SE72032',  'SE72231',  'SE72091',  'SE72140',  'SE72368')
  
  #  Paper  items
  Cright <- c(Cright,
              'MP72188',  'MP62115',  'MP72022',  'MP72013',  'MP62194',  'MP72043',  'MP72150',  
              'MP62067',  'MP62120',  'MP72005',  'MP72154',  
              'SP72066',  'SP72345D', 'SP72345E', 'SP72345G', 'SP72363',  'SP72462',  'SP72100',  
              'SP62132',  'SP62153',  'SP62018C', 'SP72032',  'SP72231',  'SP62119',  'SP72205',  
              'SP72370',  'SP62091B', 'SP62246',  'SP72091',  'SP72140',  'SP72368')
  
  data <- scoreit(data=data, item=Cright, type="MC", right=3, NR=6, OM=9, other=7)
  
  # Items with 6 choice options
  Cright <- c('SE82021C', 'SE82001B', 'SE72000B', 'SE82002C', 'SE72033E', 'SE72905D')
  
  data <- scoreit(data, item=Cright, type="MC", right=3, NR=96, OM=99, other=90)
  
  
  #  Score  multiple-choice  items  with  D  key
  #  Digital  items
  Dright <- c('ME72172',  'ME72220',  'ME82727',  'ME62271',  'ME62171',  'ME62320',  'ME62341',  
              'ME72234',  'ME72083B', 'ME72038',  'ME72237',  
              'SE72002',  'SE72234',  'SE62106',  'SE62190',  'SE82323C', 'SE82064A', 'SE82143',  
              'SE72143C', 'SE72168',  'SE72329',  'SE82042',  'SE82043',  'SE82073',  'SE82244B', 
              'SE82271A', 'SE62101A', 'SE62243B', 'SE82080',  'SE82138',  'SE62089',  'SE62177',  
              'SE72031',  'SE72249',  'SE72303')
  
  #  Paper  items
  Dright <- c(Dright,
              'MP72234',  'MP72083B', 'MP72172',  'MP62350',  'MP72237',  'MP62271',  'MP62171',  
              'MP62320',  'MP72220',  'MP62341',  
              'SP72002',  'SP72234',  'SP72070',  'SP72024',  'SP62106',  'SP62190',  'SP72031',  
              'SP72220',  'SP62089',  'SP62177',  'SP72168',  'SP72249',  'SP72303')
  
  data <- scoreit(data=data, item=Dright, type="MC", right=4, NR=6, OM=9, other=7)
  
  # Items with 6 choice options
  Dright <- c('SE72403A', 'SQ72S13A', 'SE82021D', 'SE82001A', 'SE72000E')
  
  data <- scoreit(data, item=Dright, type="MC", right=4, NR=96, OM=99, other=90)
  
  
  #  Score  multiple-choice  items  with  E  key
  #  Digital  items
  Eright <- c('SE72403D', 'SE82001C', 'SE72000A', 'SE82002D', 'SE72033B', 'SE72905B')
  
  data <- scoreit(data=data, item=Eright, type="MC", right=5, NR=96, OM=99, other=90)
  
  #  Score  multiple-choice  items  with  F  key
  #  Digital  items
  Fright <- c('SE72403C', 'SE82021B', 'SE82001E', 'SE72000D', 'SE82002B', 'SE72033C')
  
  data <- scoreit(data=data, item=Fright, type="MC", right=6, NR=96, OM=99, other=90)
  
  #  Score  constructed-response  items
  #  Digital  items
  constr <- c('ME72002',  'ME72035',  'ME72055',  'ME72106A', 'ME72106B', 'ME72106C', 'ME72128A', 
              'ME72128B', 'ME72119',  'ME72153A', 'ME72153B', 'ME72001',  'ME72019',  'ME72024',  
              'ME72225',  'ME72225A', 'ME72225B', 'ME72110A', 'ME72110B', 'ME72139',  'ME72229',  
              'ME72171',  'ME82214A', 'ME82214B', 'MQ82N01A', 'MQ82N01B', 'MQ82N02',  'MQ82N03',  
              'MQ82N04',  'MQ82N07',  'MQ82N08A', 'MQ82N08B', 'ME82711',  'ME82111',  'ME82320',  
              'ME82707',  'ME82802',  'MQ72D01',  'MQ72D02A', 'MQ72D02AA','MQ72D02AB','MQ72D02AC',
              'MQ72D02B', 'MQ72D03',  'MQ72D04',  'MQ72D04A', 'MQ72D05',  'MQ72D06A', 'MQ72D06B', 
              'MQ72D07A', 'MQ72D07B', 'MQ72D08A', 'MQ72D08B', 'MQ82C02',  'MQ82C03',  'MQ82C04',  
              'MQ82C05',  'ME82116',  'ME82322',  'ME82199A', 'ME82199B', 'ME82723',  'ME82106',  
              'ME82807',  'ME82309',  'ME82724',  'ME82403',  'ME82205',  'ME82201',  'ME82310',  
              'ME82112',  'ME82203',  'ME82404',  'ME82206',  'ME82791',  'ME82103',  'ME62152',  
              'ME62215',  'ME62215A', 'ME62215B', 'ME62143',  'ME62030',  'ME62301',  'ME62296',  
              'ME82104',  'ME82109',  'ME82702',  'ME82702A', 'ME82702B', 'ME82702C', 'ME82623',  
              'ME82715',  'ME82790',  'ME82101',  'ME82601',  'ME82502',  'ME82502A', 'ME82502B', 
              'ME82410',  'ME82725',  'ME82315',  'ME62214',  'ME62154',  'ME62250A', 'ME62250B', 
              'ME62170',  'ME62170A', 'ME62170B', 'ME62192',  'ME62072',  'ME82613',  'ME82708A', 
              'ME82216',  'ME82105',  'ME82405',  'ME82792',  'ME82612',  'ME82313',  'ME82407',  
              'ME82312A', 'ME82312B', 'ME82420',  'ME82726',  'ME82804',  'ME82207',  'ME82219',  
              'ME82415',  'ME82314',  'ME82608',  'ME82119',  'ME82611',  'ME82411',  'ME82610',  
              'ME82801',  'ME82511',  'ME82603',  'ME72178',  'ME72020',  'ME72052',  'ME72052A', 
              'ME72052B', 'ME72083A', 'ME72108A', 'ME72108B', 'ME72181',  'ME72126',  'ME72164',  
              'ME72185A', 'ME72185B', 'ME72021',  'ME72026',  'ME72041A', 'ME72041B', 'ME72094',  
              'ME72059',  'ME72081',  'ME72140',  'ME72120',  'ME72131',  'ME72147',  'ME72161',  
              'ME72187',  'ME72045',  'ME72049',  'ME72069',  'ME72074',  'ME72095',  'ME72095A', 
              'ME72095B', 'ME72109',  'ME72196',  'ME72232',  'ME72206',  
              'SE72403',  'SE72082',  'SE72102',  'SE72141A', 'SE72141B', 'SE72921',  'SE72284',  
              'SE72345',  'SE62095',  'SE62064',  'SE62163',  'SE62018',  'SE62143',  'SE62276',  
              'SE62050',  'SE62024B', 'SE82902',  'SE82025',  'SE82273A', 'SE82273B', 'SE82131',  
              'SE82171',  'SE82275',  'SE82343',  'SQ82T01',  'SQ82T02A', 'SQ82T02AA','SQ82T02AB',
              'SQ82T02B', 'SQ82T03A', 'SQ82T03B', 'SQ82T04',  'SQ72S01',  'SQ72S02',  'SQ72S03',  
              'SQ72S04',  'SQ72S06',  'SQ72S08',  'SQ72S09',  'SQ72S10',  'SQ72S11',  'SQ72S12',  
              'SQ72S13B', 'SE82068A', 'SE82068B', 'SE82008',  'SE82021',  'SE82071',  'SE82175',  
              'SE82104',  'SE82172',  'SE82205',  'SE82262',  'SE82341',  'SE82323',  'SE82099',  
              'SE82325',  'SE82064',  'SE82001',  'SE82040',  'SE82060',  'SE82060A', 'SE82060B', 
              'SE82147',  'SE82223',  'SE82340',  'SE82327',  'SE72078',  'SE72000',  'SE72143',  
              'SE72523',  'SE72293',  'SE72280A', 'SE82002',  'SE82200A', 'SE82200B', 'SE82244',  
              'SE82271',  'SE82346A', 'SE82346B', 'SE62100',  'SE62101',  'SE62128',  'SE62047',  
              'SE62042',  'SE62250',  'SE62056',  'SE62022',  'SE62243',  'SE82075',  'SE82005',  
              'SE82263',  'SE82263A', 'SE82263B', 'SE82333',  'SQ82L01',  'SQ82L02A', 'SQ82L02B', 
              'SQ82L04A', 'SQ82L04B', 'SQ82L05',  'SQ82L07A', 'SQ82L07B', 'SQ82L09A', 'SQ82L09B', 
              'SE82077',  'SE82146',  'SE82246',  'SE82331',  'SE62112',  'SE62093',  'SE62006',  
              'SE62006A', 'SE62006B', 'SE62006C', 'SE62067',  'SE62211A', 'SE62211B', 'SE62033',  
              'SE62242',  'SE72033',  'SE72440',  'SE72086',  'SE72005',  'SE72920',  'SE72294',  
              'SE72261',  'SE72348',  'SE72905',  'SE72016',  'SE72016A', 'SE72016B', 'SE72451',  
              'SE72074',  'SE72109',  'SE72132',  'SE72210')
  
  #  Paper  items
  constr <- c(constr,
              'MP72178',  'MP72020',  'MP72052',  'MP72052A', 'MP72052B', 'MP72083A', 'MP72108A', 
              'MP72108B', 'MP72181',  'MP72126',  'MP72164',  'MP72185A', 'MP72185B', 'MP72002',  
              'MP72035',  'MP72055',  'MP72106A', 'MP72106B', 'MP72106C', 'MP72128A', 'MP72128B', 
              'MP72119',  'MP72153A', 'MP72153B', 'MP62151',  'MP62346',  'MP62056',  'MP62317',  
              'MP62078',  'MP62287',  'MP62345A', 'MP72187',  'MP72045',  'MP72049',  'MP72069',  
              'MP72074',  'MP72095',  'MP72095A', 'MP72095B', 'MP72109',  'MP72196',  'MP72232',  
              'MP72206',  'MP62152',  'MP62215',  'MP62143',  'MP62030',  'MP62301',  'MP62344',  
              'MP62296',  'MP72001',  'MP72019',  'MP72024',  'MP72225',  'MP72225A', 'MP72225B', 
              'MP72110A', 'MP72110B', 'MP72139',  'MP72229',  'MP72171',  'MP62214',  'MP62154',  
              'MP62250A', 'MP62250B', 'MP62170',  'MP62192',  'MP62072',  'MP72021',  'MP72026',  
              'MP72041A', 'MP72041B', 'MP72094',  'MP72059',  'MP72081',  'MP72140',  'MP72120',  
              'MP72131',  'MP72147',  'MP72161',  
              'SP72082',  'SP72102',  'SP72141A', 'SP72141B', 'SP72921',  'SP72284',  'SP72345',  
              'SP72400',  'SP72903',  'SP72145',  'SP72298',  'SP72215',  'SP72260',  'SP72265',  
              'SP72351',  'SP62095',  'SP62064',  'SP62163',  'SP62018',  'SP62143',  'SP62276',  
              'SP62050',  'SP62024B', 'SP72033',  'SP72440',  'SP72086',  'SP72005',  'SP72920',  
              'SP72294',  'SP72261',  'SP72348',  'SP62112',  'SP62093',  'SP62006',  'SP62067',  
              'SP62211B', 'SP62211A', 'SP62033',  'SP62242',  'SP72078',  'SP72000',  'SP72143',  
              'SP72523',  'SP72293',  'SP72280A', 'SP62100',  'SP62101',  'SP62128',  'SP62047',  
              'SP62042',  'SP62250',  'SP62056',  'SP62022',  'SP62243',  'SP72905',  'SP72016',  
              'SP72016A', 'SP72016B', 'SP72451',  'SP72074',  'SP72109',  'SP72132',  'SP72210')
  
  data <- scoreit(data=data, item=constr, type="CR", right="", NR=c(6,96), OM=c(9,99), other=c(7,90))
  
  # Save file
  env <- new.env()
  data_name <- paste0(infile,"_SCR_v3") # file name adjusted
  env[[data_name]] <- data
  save(list=c(data_name),envir=env,file=output_file)
}

doit(indir =  paste0(getwd(), "/YZM-Thesis/Data/"), # adjust if necessary
     outdir = paste0(getwd(), "/YZM-Thesis/Data/"), 
     infile = "GRADE8DATA_COMPLETE")

#' ### References
#' 
#' Fishbein, B., Taneva, M., & Kowolik, K. (2025). TIMSS 2023 User Guide for the International Database. Boston College, TIMSS & PIRLS International Study Center. https://timss2023.org/data
#' 
