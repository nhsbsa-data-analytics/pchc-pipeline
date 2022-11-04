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

devtools::install_github(auth_token = Sys.getenv("GITHUB_PAT"), repo = "nhsbsa-data-analytics/pchcR")

library(pchcR)

# 2. set options ----------------------------------------------------------
pchc_options()

# 3. build fact table if new data available -------------------
con <- con_nhsbsa(
  dsn = "FBS_8192k",
  driver = "Oracle in OraClient19Home1",
  "DWCP"
  # username = rstudioapi::showPrompt(title = "Username", message = "Username"),
  # password = rstudioapi::askForPassword()
)

#only run if need to build new fact table
# pchcR::create_fact(con, to = 202203L)

# 4. extract data tables from fact table -----------------------------------------

#dwh tables
table_1_dwh <- table_1_dwh(con)
table_2_dwh <- table_2_dwh(con)
table_3_dwh <- table_3_dwh(con)
table_4_dwh <- table_4_dwh(con)
table_5_dwh <- table_5_dwh(con)
table_6_dwh <- table_6_dwh(con)
table_7_dwh <- table_7_dwh(con)
table_8_dwh <- table_8_dwh(con)


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

table_5[is.na(table_5)] = 0

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
      `BNF Chapter Code` != "Undefined" ~ substr(`BNF Section Code`, 1, 2),
      is.na(`BNF Chapter Code`) ~ substr(`BNF Section Code`, 1, 2),
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
table_8_raw <- table_8_scmd |>
  full_join(
    table_8_dwh,
    by = c(
      "FINANCIAL_YEAR" = "Financial Year",
      "BNF_SECTION" = "BNF Section Code",
      "ICB" = "ICB"
    )
  ) |>
  select(
   -`BNF Chapter Name`
  ) |>
  left_join(
    bnf_chapter_lookup,
    by = c(
      "BNF_CHAPTER" = "BNF Chapter Code"
    )
  ) |>
  mutate(
    BNF_CHAPTER = case_when(
      is.na(BNF_CHAPTER) ~ `BNF Chapter Code`,
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
    `BNF Chapter Name` = case_when(
      is.na(`BNF Chapter Name`) ~ "Undefined",
      TRUE ~ `BNF Chapter Name` 
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

table_8_non_na <- table_8_raw |>
  filter(
    !is.na(`BNF Section Name`)
  ) |>
  left_join(
    bnf_chapter_lookup, 
    by = c(
      "BNF_CHAPTER" = "BNF Chapter Code"
    )
  ) |>
  select(
    1,2,12,4,5,6,7,8,9,10,11
  ) |>
  rename(
    "Financial Year" = 1,
    "BNF Chapter Code" = 2,
    "BNF Chapter Name" = 3,
    "BNF Section Code" = 4,
    "BNF Section Name" = 5,
    "ICB Code" = 6,
    "ICB" = 7,
    "Hospital prescribing issued within hospitals (GBP)" = 8,
    "Dental prescribing dispensed in the community (GBP)" = 9,
    "Hospital prescribing dispensed in the community (GBP)" = 10,
    "Primary care prescribing dispensed in the community (GBP)" = 11
  )

icb_code_lookup <- table_7_dwh |>
  select(
    `ICB Code`,
    ICB
  ) |>
  distinct()
 
table_8_na <- table_8_raw |>
  filter(
    is.na(`BNF Section Name`)
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
    "BNF Chapter Code" = 2,
    "BNF Chapter Name" = 3,
    "BNF Section Code" = 4,
    "BNF Section Name" = 5,
    "ICB Code" = 6,
    "ICB" = 7,
    "Hospital prescribing issued within hospitals (GBP)" = 8,
    "Dental prescribing dispensed in the community (GBP)" = 9,
    "Hospital prescribing dispensed in the community (GBP)" = 10,
    "Primary care prescribing dispensed in the community (GBP)" = 11
  )

table_8 <- table_8_non_na |>
  bind_rows(
    table_8_na
  ) |>
  arrange(
    `Financial Year`,
    `BNF Chapter Code`,
    `BNF Section Code`,
    `ICB Code`
  ) %>%
  arrange(
    `Financial Year`,
    `BNF Chapter Code`,
    `BNF Section Code`,
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
    `BNF Section Name` = case_when(
      `BNF Section Code` == "1803" ~ "X-Ray contrast media",
      `BNF Section Code` == "1901" ~ "Alcohol, wines and spirits",
      `BNF Section Code` == "1904" ~ "Single substances",
      `BNF Section Code` == "1905" ~ "Other preparations",
      `BNF Section Code` == "1908" ~ "Colouring, flavouring and sweetening agents",
      `BNF Section Code` == "1909" ~ "Disinfectants, preservatives and sterilising agents",
      `BNF Section Code` == "1915" ~ "Other gases",
      `BNF Section Code` == "2001" ~ "Absorbent Cottons",
      `BNF Section Code` == "2134" ~ "Vaginal PH Correction Products",
      `BNF Section Code` == "2144" ~ "Debrisoft pad 13cm x 20cm",
      TRUE ~ `BNF Section Name`
    )
  )

table_8[is.na(table_8)] = 0

# 6. write data to .xlsx --------------------------------------------------

# FY Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "Table_1",
  "Table_2",
  "Table_3",
  "Table_4",
  "Table_5",
  "Table_6",
  "Table_7",
  "Table_8"
)

wb <- create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
   "BNF Chapter Code",
   "BNF Chapter Name",
   "BNF Section Code",
   "BNF Section Name",
   "Cost",
   "Dental prescribing dispensed in the community (GBP)",
   "Financial Year",
   "Hospital prescribing dispensed in the community (GBP)",
   "Hospital prescribing issued within hospitals (GBP)",
   "Population",
   "Primary care prescribing dispensed in the community (GBP)",
   "ICB Code",
   "ICB",
   "Undefined data"
)

meta_descs <-
  c(
    "The unique code used to refer to the British National Formulary (BNF) chapter.",
    "The name given to a British National Formulary (BNF) chapter. This is the broadest grouping of the BNF therapeutical classification system.",
    "The unique code used to refer to the British National Formulary (BNF) section.",
    "The name given to a British National Formulary (BNF) section. This is the next broadest grouping of the BNF therapeutical classification system after chapter.",
    "There are many costs incurred when a dispensing contractor fulfils a prescription. In table 1 the costs in primary care represent the actual costs. Actual cost is the cost recharged to NHS commissioners for the provision of drugs, appliances, and medical devices by dispensing contractors. In tables 2 to 8 the costs in primary care represent the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors. In secondary care they are the actual costs paid (including applicable VAT) for drugs, dressing, appliances, and medical devices which have been issued and used in NHS hospitals in England.",
    "Total costs for prescriptions issued by dental practitioners that have been dispensed in the community in England, Scotland, Wales, and the Channel Islands.",
    "The financial year to which the data belongs.",
    "Total costs for prescriptions issued by Hospitals in England that have been dispensed in the community in England, Scotland, Wales, and the Channel Islands.",
    "Actual costs (including applicable VAT) for medicines issued in hospitals in England that have been dispensed via the hospital pharmacy, homecare companies and outsourced out-patient pharmacy partnerships.",
    "The population figures by ICB come from the latest ONS population by ICB dataset at https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fclinicalcommissioninggroupmidyearpopulationestimates%2fmid2020sape23dt6a/sape23dt6amid2020ccg2021estimatesunformatted.xlsx",
    "Total costs for prescriptions issued by GP practices and community prescribers in England that have been dispensed in the community in England, Scotland, Wales, and the Channel Islands.",
    "The unique code used to refer to an ICB.",
    "The name give to the Integrated Care Board (ICB) that a prescribing organisation belongs to. This is based upon NHSBSA administrative records, not geographical boundaries and more closely reflect the operational organisation of practices than other geographical data sources.",
    "'Undefined' is used to indicate data that costs can not be allocated to either and BNF Chapter, BNF Section or an ICB."
  )

create_metadata(wb,
                meta_fields,
                meta_descs)

#### table 1
# write data to sheet
write_sheet(
  wb,
  "Table_1",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 1: Total costs in each setting by financial year using actual costs in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are actual costs, the cost recharged to NHS commissioners for the provision of drugs, appliances, and medical devices by dispensing contractors."
    ),
  table_1,
  18
)

#left align columns A
format_data(wb,
            "Table_1",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_1",
            c("B", "C", "D", "E", "F"),
            "right",
            "#,##0.00")

#### table 2
# write data to sheet
write_sheet(
  wb,
  "Table_2",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 2: Total costs in each setting by financial year using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_2,
  18
)

#left align columns A
format_data(wb,
            "Table_2",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_2",
            c("B", "C", "D", "E", "F"),
            "right",
            "#,##0.00")

#### table 3
# write data to sheet
write_sheet(
  wb,
  "Table_3",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 3: Percentage change in costs in each setting by financial year using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_3,
  18
)

#left align columns A
format_data(wb,
            "Table_3",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_3",
            c("B", "C", "D", "E"),
            "right",
            "0.00")

#### table 4
# write data to sheet
write_sheet(
  wb,
  "Table_4",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 4: Total costs in each setting by month, April 2017 to March 2022 using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_4,
  18
)

#left align columns A
format_data(wb,
            "Table_4",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_4",
            c("B", "C", "D", "E", "F"),
            "right",
            "#,##0.00")

#### table 5
# write data to sheet
write_sheet(
  wb,
  "Table_5",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 5: Total costs in each setting by BNF Chapter and financial year using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_5,
  18
)

#left align columns A
format_data(wb,
            "Table_5",
            c("A", "B", "C"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_5",
            c("D", "E", "F", "G", "H"),
            "right",
            "#,##0.00")

#### table 6
# write data to sheet
write_sheet(
  wb,
  "Table_6",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 6: Total costs in each setting by BNF Section and financial year using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_6,
  18
)

#left align columns A
format_data(wb,
            "Table_6",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_6",
            c("F", "G", "H", "I", "J"),
            "right",
            "#,##0.00")

#### table 7
# write data to sheet
write_sheet(
  wb,
  "Table_7",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 7: Total costs in each setting by Integrated Care Board (ICB) and financial year using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_7,
  18
)

#left align columns A
format_data(wb,
            "Table_7",
            c("A", "B", "C"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_7",
            c("D", "E", "F", "G", "H", "J"),
            "right",
            "#,##0.00")

#right align columns and round to 2 DP
format_data(wb,
            "Table_7",
            c("I"),
            "right",
            "#,##0")

#### table 8
# write data to sheet
write_sheet(
  wb,
  "Table_8",
  paste0(
    "Prescribing Costs in Hospitals and the Community - England 2017/18 - 2021/22 - ",
    "Table 8: Total costs in each setting by BNF Section, ICB and financial year using Net Ingredient Costs (NIC) in primary care"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Data is sourced from NHSBSA Data & Insight Data Warehouse and RX-Info Define",
    "3. Primary care costs in this table are the basic price of the item. This is sometimes called the ‘Net Ingredient Cost’ (NIC). This also known as reimbursement of costs to dispensing contractors."
  ),
  table_8,
  18
)

#left align columns A
format_data(wb,
            "Table_8",
            c("A", "B", "C", "D", "E", "F", "G"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Table_8",
            c("H", "I", "J", "K", "L"),
            "right",
            "#,##0.00")



#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       "outputs/pchc-2022-summary-tables-v001.xlsx",
                       overwrite = TRUE)

# 7. automate narratives --------------------------------------------------

# 8. render markdowns ------------------------------------------------------

rmarkdown::render("pres-costs-hosp-comm-v1.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/pchc-2021-2022-narrative-v001.html")


rmarkdown::render("pres-costs-hosp-comm-v1.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/pchc-2021-2022-narrative-v001.docx")

rmarkdown::render("pchc-background.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/pchc_2022_background_information_methodology_v001.html")


rmarkdown::render("pchc-background.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/pchc_2022_background_information_methodology_v001.docx")


