# pipeline.R --------------------------------------------------------------
# This script is used to run the RAP for the PCHC publication


# 1. install required packages --------------------------------------------
req_pkgs <-
  c(
    "dplyr",
    "stringr",
    "data.table",
    "yaml",
    "openxlsx",
    "rmarkdown",
    "highcharter",
    "lubridate",
    "dbplyr",
    "tidyr",
    "readxl",
    "DT"
  )

#utils::install.packages(req_pkgs, dependencies = TRUE)
#
# devtools::install_github("nhsbsa-data-analytics/pchcR",
#                          auth_token = Sys.getenv("GITHUB_PAT"))
#
#devtools::install_github("nhsbsa-data-analytics/nhsbsaR")

invisible(lapply(c(req_pkgs,  "nhsbsaR", "pchcR"), library, character.only = TRUE))

# 2. set options ----------------------------------------------------------
pchc_options()

# 3. build fact table if new data available -------------------
con <- con_nhsbsa(
  dsn = "FBS_8192k",
  driver = "Oracle in OraClient19Home1",
  "DWCP",
  username = rstudioapi::showPrompt(title = "Username", message = "Username"),
  password = rstudioapi::askForPassword()
)

#only run if need to build new fact table
#pchcR::create_fact(con, to = max_month)

# 4. extract data tables from fact table -----------------------------------------

#dwh tables
table_1_dwh <- table_1_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_2_dwh <- table_2_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_3_dwh <- table_3_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_4_dwh <- table_4_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_5_dwh <- table_5_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_6_dwh <- table_6_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_7_dwh <- table_7_dwh(con) |> filter(`Financial Year` != "	2016/2017")
table_8_dwh <- table_8_dwh(con) |> filter(`Financial Year` != "	2016/2017")


# disconnect from DWH
DBI::dbDisconnect(con)

#scmd tables
scmd_icb_bnf_data <- scmd_icb_bnf_data("Y:\\Official Stats\\PCHC\\data\\ICB_BNF_2022.xlsx")

scmd_national_monthly <- read_excel("Y:\\Official Stats\\PCHC\\data\\NATIONAL_MONTHLY_2022.xlsx")

table_1_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  )

table_2_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  )

table_3_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prev_year_COST = lag(COST,1 )
  ) |>
  dplyr::mutate(
    `Hospital prescribing issued within hospitals (%)` =
      (COST - prev_year_COST) / prev_year_COST * 100
  ) |>
  stats::na.omit() |>
  dplyr::select(1,4)

table_4_scmd <- scmd_national_monthly |>
  rename(
    YEAR_MONTH = 1,
    COST = 2
  ) |>
  mutate(
    YEAR_MONTH = paste0(
      substr(
        as.character(
          YEAR_MONTH
          ),
        1,
        4
        ),
      substr(
        as.character(
          YEAR_MONTH
        ),
        6,
        7
      )
    )
  )

table_5_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR,
    BNF_CHAPTER
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  )

table_6_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR,
    BNF_CHAPTER,
    BNF_SECTION
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  )

table_7_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR,
    ICB
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    ICB = trimws(ICB),
    ICB = case_when(
      ICB == "Unknown ICB" ~ "UNKNOWN ICB",
      TRUE ~ ICB
    )
  )

table_8_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR,
    ICB,
    BNF_CHAPTER,
    BNF_SECTION
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    ICB = trimws(ICB),
    ICB = case_when(
      ICB == "Unknown ICB" ~ "UNKNOWN ICB",
      TRUE ~ ICB
    )
  )


# 5. data manipulation ----------------------------------------------------

#table 1
table_1 <- table_1_dwh |>
  left_join(
    table_1_scmd,
    by = c("Financial Year" = "FINANCIAL_YEAR")
  ) |>
  select(
    1, 5, 3, 4, 2
  ) |>
  rename(
    "Hospital prescribing issued within hospitals (GBP)" = 2
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  )

#table 2
table_2 <- table_2_dwh |>
  left_join(
    table_2_scmd,
    by = c("Financial Year" = "FINANCIAL_YEAR")
  ) |>
  select(
    1, 5, 3, 4, 2
  ) |>
  rename(
    "Hospital prescribing issued within hospitals (GBP)" = 2
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  )

#table 3
table_3 <- table_3_dwh |>
  left_join(
    table_3_scmd,
    by = c("FINANCIAL_YEAR" = "FINANCIAL_YEAR")
  ) |>
  select(
    1, 5, 3, 4, 2
  ) |>
  rename(
    "Financial Year" = 1
  ) 


#table 4  
table_4 <- table_4_dwh |>
  mutate(
    `Year Month` = as.character(`Year Month`)
  ) |>
  left_join(
    table_4_scmd,
    by = c("Year Month" = "YEAR_MONTH")
  ) |>
  select(
    1, 5, 3, 4, 2
  ) |>
  rename(
    "Hospital prescribing issued within hospitals (GBP)" = 2
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  )

#table 5
table_5 <- table_5_scmd |>
  left_join(
    table_5_dwh,
    by = c(
      "FINANCIAL_YEAR" = "Financial Year",
      "BNF_CHAPTER" = "BNF Chapter Code"
      )
  ) |>
  select(
    1,2,4,3,6,7,5
  ) |>
  mutate(
    BNF_CHAPTER = case_when(
      BNF_CHAPTER == "-None-" ~ "Undefined",
      TRUE ~ BNF_CHAPTER
    ),
    `BNF Chapter Name` = case_when(
      is.na(`BNF Chapter Name`) ~ "Undefined",
      TRUE ~ `BNF Chapter Name`
    )
  ) |>
  arrange(
    FINANCIAL_YEAR,
    BNF_CHAPTER == "Undefined"
  ) |>
  rename(
    "Financial Year" = 1,
    "BNF Chapter Code" = 2,
    "Hospital prescribing issued within hospitals (GBP)" = 4
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  ) 

table_5[is.na(table_4)] = 0

#table 6
table_6 <- table_6_scmd |>
  full_join(
    table_6_dwh,
    by = c(
      "FINANCIAL_YEAR" = "Financial Year",
      "BNF_SECTION" = "BNF Section Code"
    )
  )

scmd_section_lookup <- scmd_icb_bnf_data |>
  select(
    BNF,
    BNF_SECTION
  ) |>
  distinct() |>
  mutate(BNF = gsub(".*- ", "", BNF))

bnf_chapter_lookup <- table_6 |>
  select(
    `BNF Chapter Code`,
    `BNF Chapter Name`
  ) |>
  distinct()

table_6_na <- table_6 |>
  filter(
    is.na(
      `BNF Chapter Code`
    )
  ) |>
  select(
    1,2,3,4,8,9,10
  ) |>
  left_join(
    scmd_section_lookup,
    by = c(
      "BNF_SECTION" = "BNF_SECTION"
    )
  ) |>
  left_join(
    bnf_chapter_lookup,
    by = c(
      "BNF_CHAPTER" = "BNF Chapter Code"
    )
  ) |>
  select(
    1, 2, 9, 3, 8, 4, 6, 7, 5
  ) |>
  rename(
    "Financial Year" = 1,
    "BNF Chapter Code" = 2,
    "BNF Chapter Name" = 3,
    "BNF Section Code" = 4,
    "BNF Section Name" = 5,
    "Hospital prescribing issued within hospitals (GBP)" = 6,
    "Dental prescribing dispensed in the community (GBP)" = 7,
    "Hospital prescribing dispensed in the community (GBP)" = 8,
    "Primary care prescribing dispensed in the community (GBP)" = 9
  )

table_6_non_na <- table_6 |>
  filter(
    !is.na(
      `BNF Chapter Code`
    )
  ) |>
  select(
    1, 2, 6, 3, 7, 4, 9, 10, 8
  ) |>
  rename(
    "Financial Year" = 1,
    "BNF Chapter Code" = 2,
    "BNF Chapter Name" = 3,
    "BNF Section Code" = 4,
    "BNF Section Name" = 5,
    "Hospital prescribing issued within hospitals (GBP)" = 6,
    "Dental prescribing dispensed in the community (GBP)" = 7,
    "Hospital prescribing dispensed in the community (GBP)" = 8,
    "Primary care prescribing dispensed in the community (GBP)" = 9
  )

table_6 <- table_6_non_na |>
  bind_rows(
    table_6_na
  ) |>
  mutate(
    `BNF Chapter Code` = case_when(
      `BNF Chapter Code` == "-None-" ~ "Undefined",
      TRUE ~ `BNF Chapter Code`
    ),
    `BNF Chapter Name` = case_when(
      is.na(`BNF Chapter Name`) ~ "Undefined",
      TRUE ~ `BNF Chapter Name` 
    ),
    `BNF Section Code` = case_when(
      `BNF Section Code` == "-None-character(0)" ~ "Undefined",
      TRUE ~ `BNF Section Code`
    ),
    `BNF Section Name` = case_when(
      `BNF Section Name` == "-None-" ~ "Undefined",
      TRUE ~ `BNF Section Name`
    ),
    `BNF Chapter Code` = case_when(
      `BNF Chapter Code` != "Undefined" ~ substr(`BNF Section`, 1, 2),
      is.na(`BNF Chapter Code`) ~ substr(`BNF Section`, 1, 2),
      TRUE ~ `BNF Chapter Code`
     )
    )|>
  arrange(
    `Financial Year`,
    `BNF Chapter Code`,
    `BNF Section Code`
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  ) 

table_6[is.na(table_6)] = 0

#table 7
table_7 <- table_7_dwh |>
  left_join(
    table_7_scmd,
    by = c(
      "Financial Year" = "FINANCIAL_YEAR",
      "ICB" = "ICB"
    )
  ) |>
  select(
    1,2,3,7,5,6,4
  ) |>
  rename(
    "Hospital prescribing issued within hospitals (GBP)" = 4
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  ) 

table_7[is.na(table_7)] = 0

# get stp population
ons_stp_pop <- function() {
  
  #create temp file to download xlsx file into
  
  temp <- tempfile()
  
  stp_url <- utils::download.file(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fclinicalcommissioninggroupmidyearpopulationestimates%2fmid2020sape23dt6a/sape23dt6amid2020ccg2021estimatesunformatted.xlsx",
                                  temp,
                                  mode = "wb")
  
  #read xlsx population file
  stp_pop <- readxl::read_xlsx(temp,
                               sheet = 4,
                               range = "A9:G114",
                               col_names = c("CCG_CD","CCG_NM","STP21_CD","STP21_NM",
                                             "REGION_CD","REGION_NM","POP"), 
                               skip = 8)
  
  ods_lookup = "https://opendata.arcgis.com/api/v3/datasets/a458c272484743aa9caa25619ccbe1ac_0/downloads/data?format=csv&spatialRefId=4326"
  
  ods <- data.table::fread(ods_lookup)
  
  #join population data to ods lookup
  
  df <- stp_pop |> 
    dplyr::left_join(select(ods,CCG21CD,STP21CDH,STP21NM), by = c("CCG_CD" = "CCG21CD")) |> 
    dplyr::group_by(STP21_NM,STP21CDH,STP21_CD) |> 
    dplyr::summarise(POP = sum(POP)) |> 
    ungroup()
  
  return(df)
  
}
stp_pop <- ons_stp_pop() |>
  select(
    STP21CDH, POP
  )

table_7 <- table_7 |>
  left_join(
    stp_pop,
    by = c(
      "ICB Code" = "STP21CDH"
    )
  ) |>
  rename(
    "Population" = 9
  ) |>
  mutate(
    `Average Cost Per Capita (GBP)` = `Total (GBP)` / Population
  )

#table 8
table_7_raw <- table_7_scmd |>
  full_join(
    table_7_dwh,
    by = c(
      "FINANCIAL_YEAR" = "Financial Year",
      "BNF_SECTION" = "BNF Section",
      "ICB" = "ICB"
    )
  ) |>
  select(
   -`BNF Chapter Description`
  ) |>
  left_join(
    bnf_chapter_lookup,
    by = c(
      "BNF_CHAPTER" = "BNF Chapter"
    )
  ) |>
  mutate(
    BNF_CHAPTER = case_when(
      is.na(BNF_CHAPTER) ~ `BNF Chapter`,
      TRUE ~ BNF_CHAPTER
    )
  ) |>
  select(
    1, 3, 12, 4, 7, 8, 2, 5, 10, 11, 9
  ) |>
  mutate(
    BNF_CHAPTER = case_when(
      BNF_CHAPTER == "-None-" ~ "Undefined",
      TRUE ~ BNF_CHAPTER
    ),
    `BNF Chapter Description` = case_when(
      is.na(`BNF Chapter Description`) ~ "Undefined",
      TRUE ~ `BNF Chapter Description` 
    ),
    BNF_SECTION = case_when(
      BNF_SECTION == "-None-character(0)" ~ "Undefined",
      TRUE ~ BNF_SECTION
    ),
    BNF_CHAPTER = case_when(
      BNF_CHAPTER != "Undefined" ~ substr(BNF_SECTION, 1, 2),
      is.na(BNF_CHAPTER) ~ substr(BNF_SECTION, 1, 2),
      TRUE ~ BNF_CHAPTER
    )
  )

table_7_non_na <- table_7_raw |>
  filter(
    !is.na(`BNF Section Description`)
  ) |>
  left_join(
    bnf_chapter_lookup, 
    by = c(
      "BNF_CHAPTER" = "BNF Chapter"
    )
  ) |>
  select(
    1,2,12,4,5,6,7,8,9,10,11
  ) |>
  rename(
    "Financial Year" = 1,
    "BNF Chapter" = 2,
    "BNF Chapter Description" = 3,
    "BNF Section" = 4,
    "BNF Section Description" = 5,
    "ICB Code" = 6,
    "ICB" = 7,
    "Hospital prescribing issued within hospitals (GBP)" = 8,
    "Dental prescribing dispensed in the community (GBP)" = 9,
    "Hospital prescribing dispensed in the community (GBP)" = 10,
    "Primary care prescribing dispensed in the community (GBP)" = 11
  )

icb_code_lookup <- table_6_dwh |>
  select(
    `ICB Code`,
    ICB
  ) |>
  distinct()
 
table_7_na <- table_7_raw |>
  filter(
    is.na(`BNF Section Description`)
  ) |>
  left_join(
    scmd_section_lookup,
    by = c(
      "BNF_SECTION" = "BNF_SECTION"
    )
  ) |>
  left_join(
    icb_code_lookup,
    by = c(
      "ICB" = "ICB"
    )
  ) %>%
  select(
    1,2,3,4,12,13,7,8,9,10,11
  ) %>%
  mutate(
    BNF = case_when(
      is.na(BNF) ~ "Undefined",
      TRUE ~ BNF
    )
  )|>
  rename(
    "Financial Year" = 1,
    "BNF Chapter" = 2,
    "BNF Chapter Description" = 3,
    "BNF Section" = 4,
    "BNF Section Description" = 5,
    "ICB Code" = 6,
    "ICB" = 7,
    "Hospital prescribing issued within hospitals (GBP)" = 8,
    "Dental prescribing dispensed in the community (GBP)" = 9,
    "Hospital prescribing dispensed in the community (GBP)" = 10,
    "Primary care prescribing dispensed in the community (GBP)" = 11
  )

table_7 <- table_7_non_na |>
  bind_rows(
    table_7_na
  ) |>
  arrange(
    `Financial Year`,
    `BNF Chapter`,
    `BNF Section`,
    `ICB Code`
  ) %>%
  arrange(
    `Financial Year`,
    `BNF Chapter`,
    `BNF Section`,
    `ICB Code` == "-"
  )|>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  ) %>%
  mutate(
    #fix bnf section desc differences from scmd to dwh
    `BNF Section Description` = case_when(
      `BNF Section` == "1803" ~ "X-Ray contrast media",
      `BNF Section` == "1901" ~ "Alcohol, wines and spirits",
      `BNF Section` == "1904" ~ "Single substances",
      `BNF Section` == "1905" ~ "Other preparations",
      `BNF Section` == "1908" ~ "Colouring, flavouring and sweetening agents",
      `BNF Section` == "1909" ~ "Disinfectants, preservatives and sterilising agents",
      `BNF Section` == "1915" ~ "Other gases",
      `BNF Section` == "2001" ~ "Absorbent Cottons",
      `BNF Section` == "2134" ~ "Vaginal PH Correction Products",
      `BNF Section` == "2144" ~ "Debrisoft pad 13cm x 20cm",
      TRUE ~ `BNF Section Description`
    )
  )

table_7[is.na(table_7)] = 0

section <- table_7 %>%
  group_by(
    `Financial Year`,
    `BNF Chapter`,
    `BNF Chapter Description`,
    `BNF Section`,
    `BNF Section Description`
  ) %>%
  summarise(
    `Hospital prescribing issued within hospitals (GBP)` = sum(`Hospital prescribing issued within hospitals (GBP)`),
    `Dental prescribing dispensed in the community (GBP)` = sum(`Dental prescribing dispensed in the community (GBP)`),
    `Hospital prescribing dispensed in the community (GBP)` = sum(`Hospital prescribing dispensed in the community (GBP)`),
    `Primary care prescribing dispensed in the community (GBP)` = sum(`Primary care prescribing dispensed in the community (GBP)`)
  )


# 6. write data to .xlsx --------------------------------------------------

# FY Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(

)

wb <- create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(

)

meta_descs <-
  c(
    
  )

create_metadata(wb,
                meta_fields,
                meta_descs)

#### Patient identification
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
  ),
  pi_data_annual,
  30
)

#left align columns A
format_data(wb,
            "Patient_Identification",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Patient_Identification",
            c("B"),
            "right",
            "0.00")

# 7. automate narratives --------------------------------------------------

# 8. render markdowns ------------------------------------------------------

rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "html_document",
                  output_file = paste0("outputs/hrt_",
                                       gsub(" ", "_", ltst_month_tidy),
                                       "_v001.html"))


rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "word_document",
                  output_file = paste0("outputs/hrt_",
                                       gsub(" ", "_", ltst_month_tidy),
                                       "_v001.docx"))

rmarkdown::render("hrt-background.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/hrt-background-info-methodology-v001.html")

rmarkdown::render("hrt-background.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/hrt-background-info-methodology-v001.docx")

