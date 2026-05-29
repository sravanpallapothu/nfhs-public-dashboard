# =============================================================================
# NFHS-6 COMPENDIUM EXTRACTOR
# Language: R
#
# WHAT THIS SCRIPT DOES:
#   Reads the NFHS-6 state factsheet compendium (single PDF containing all
#   states/UTs), extracts NFHS-6 and NFHS-5 Total values for every indicator,
#   and writes a CSV ready to append to the existing dashboard Google Sheet.
#
# OUTPUT COLUMNS:
#   Indicator | Geography | Geo Level | Round | Value | Parent State | Domain | Direction
#   (Same schema as the NFHS-5 output — rows can be directly appended)
#
# KEY DIFFERENCES FROM THE NFHS-5 SCRIPT:
#   1. Single PDF instead of a folder of per-state PDFs.
#      The compendium is parsed by grouping pages that share the same
#      "X - Key Indicators" header into one state block.
#   2. Round labels are "NFHS-6" (second-to-last column) and "NFHS-5" (last).
#      The column structure is otherwise identical:
#        Urban | Rural | NFHS-6 Total | NFHS-5 Total
#   3. Three geography names differ from NFHS-5 spelling and are normalised
#      via GEO_NAME_FIX so dashboard joins work correctly.
#
# INSTALL (run once):
#   install.packages(c("pdftools", "tidyverse"))
# =============================================================================

library(pdftools)
library(tidyverse)


# =============================================================================
# CONFIG — update these two paths before running
# =============================================================================

COMPENDIUM_PDF <- "C:X/NFHS_6_Factsheets.pdf"
OUTPUT_FILE    <- "C:X/nfhs_all_data.csv"


# =============================================================================
# GEOGRAPHY NAME NORMALISATION
#
# NFHS-6 uses different spellings for three geographies vs the existing
# dashboard data (which follows NFHS-5 conventions). This crosswalk ensures
# the new rows join correctly on Geography in the Google Sheet.
#
# Andaman and Nicobar Islands → Andaman & Nicobar Islands  (NFHS-5 uses &)
# Jammu and Kashmir           → Jammu & Kashmir             (NFHS-5 uses &)
# NCT of Delhi                → NCT Delhi                   (NFHS-5 drops "of")
# =============================================================================

GEO_NAME_FIX <- c(
  "Andaman and Nicobar Islands" = "Andaman & Nicobar Islands",
  "Jammu and Kashmir"           = "Jammu & Kashmir",
  "NCT of Delhi"                = "NCT Delhi"
)

normalise_geo <- function(name) {
  if (name %in% names(GEO_NAME_FIX)) GEO_NAME_FIX[[name]] else name
}


# =============================================================================
# DOMAIN + DIRECTION LOOKUP TABLE
#
# Identical to the NFHS-5 script with three additions for NFHS-6 new indicators:
#   "hepatitis b"           — new vaccination indicator (indicator 52)
#   "rotavirus"             — rotavirus vaccine coverage (indicator 53)
#   "solid or semi-solid"   — complementary feeding (indicator 65)
#
# Order matters: more specific keywords must appear before general ones.
# =============================================================================

indicator_meta <- tribble(
  ~keyword,                                ~domain,                ~direction,

  # ── MATERNAL HEALTH ──────────────────────────────────────────────────────
  "institutional births",                  "Maternal health",      "Higher is better",
  "caesarean section",                     "Maternal health",      "Lower is better",
  "antenatal care",                        "Maternal health",      "Higher is better",
  "antenatal check-up",                    "Maternal health",      "Higher is better",
  "postnatal care",                        "Maternal health",      "Higher is better",
  "iron folic acid",                       "Maternal health",      "Higher is better",
  "skilled health personnel",              "Maternal health",      "Higher is better",
  "neonatal tetanus",                      "Maternal health",      "Higher is better",
  "mother and child protection",           "Maternal health",      "Higher is better",
  "out-of-pocket expenditure",             "Maternal health",      "Lower is better",

  # ── CHILD HEALTH ─────────────────────────────────────────────────────────
  "neonatal mortality",                    "Child health",         "Lower is better",
  "infant mortality",                      "Child health",         "Lower is better",
  "under-five mortality",                  "Child health",         "Lower is better",
  "fully vaccinated",                      "Child health",         "Higher is better",
  "bcg",                                   "Child health",         "Higher is better",
  "polio",                                 "Child health",         "Higher is better",
  "penta",                                 "Child health",         "Higher is better",
  "measles",                               "Child health",         "Higher is better",
  "hepatitis b",                           "Child health",         "Higher is better",  # NFHS-6 new
  "rotavirus",                             "Child health",         "Higher is better",  # NFHS-6 new
  "vitamin a",                             "Child health",         "Higher is better",
  "diarrhoea",                             "Child health",         "Lower is better",
  "oral rehydration",                      "Child health",         "Higher is better",
  "acute respiratory",                     "Child health",         "Lower is better",
  "zinc",                                  "Child health",         "Higher is better",
  "vaccinations in a public",              "Child health",         "Higher is better",
  "vaccinations in a private",             "Child health",         "Lower is better",

  # ── NUTRITION ────────────────────────────────────────────────────────────
  "stunted",                               "Nutrition",            "Lower is better",
  "wasted",                                "Nutrition",            "Lower is better",
  "underweight",                           "Nutrition",            "Lower is better",
  "overweight",                            "Nutrition",            "Lower is better",
  "solid or semi-solid",                   "Nutrition",            "Higher is better",  # NFHS-6 new
  "exclusively breastfed",                 "Nutrition",            "Higher is better",
  "breastfed within one hour",             "Nutrition",            "Higher is better",
  "adequate diet",                         "Nutrition",            "Higher is better",
  "breastfeeding",                         "Nutrition",            "Higher is better",

  # ── FAMILY PLANNING ──────────────────────────────────────────────────────
  "unmet need",                            "Family planning",      "Lower is better",
  "modern method",                         "Family planning",      "Higher is better",
  "family planning",                       "Family planning",      "Higher is better",
  "sterilization",                         "Family planning",      "Higher is better",
  "contraceptive",                         "Family planning",      "Higher is better",
  "condom",                                "Family planning",      "Higher is better",
  "iud",                                   "Family planning",      "Higher is better",
  "injectables",                           "Family planning",      "Higher is better",

  # ── WASH ─────────────────────────────────────────────────────────────────
  "drinking-water",                        "WASH",                 "Higher is better",
  "sanitation",                            "WASH",                 "Higher is better",
  "clean fuel",                            "WASH",                 "Higher is better",
  "electricity",                           "WASH",                 "Higher is better",
  "iodized salt",                          "WASH",                 "Higher is better",

  # ── NCDs (Non-Communicable Diseases) ─────────────────────────────────────
  "blood sugar level",                     "NCDs",                 "Lower is better",
  "elevated blood pressure",               "NCDs",                 "Lower is better",
  "mildly elevated",                       "NCDs",                 "Lower is better",
  "moderately or severely",                "NCDs",                 "Lower is better",
  "taking medicine to control blood",      "NCDs",                 "Lower is better",

  # ── WOMEN'S HEALTH ───────────────────────────────────────────────────────
  "anaemic",                               "Women's health",       "Lower is better",
  "anaemia",                               "Women's health",       "Lower is better",
  "body mass index",                       "Women's health",       "Lower is better",
  "blood pressure",                        "Women's health",       "Lower is better",
  "blood sugar",                           "Women's health",       "Lower is better",
  "high risk waist",                       "Women's health",       "Lower is better",
  "screening test for cervical",           "Women's health",       "Higher is better",
  "breast examination",                    "Women's health",       "Higher is better",
  "oral cavity examination",               "Women's health",       "Higher is better",
  "cervical cancer",                       "Women's health",       "Higher is better",
  "breast cancer",                         "Women's health",       "Higher is better",
  "oral cancer",                           "Women's health",       "Higher is better",

  # ── EDUCATION ────────────────────────────────────────────────────────────
  "attended pre-primary",                  "Education",            "Higher is better",
  "attended pre-school",                   "Education",            "Higher is better",
  "attended school",                       "Education",            "Higher is better",
  "literate",                              "Education",            "Higher is better",
  "schooling",                             "Education",            "Higher is better",
  "internet",                              "Education",            "Higher is better",

  # ── WOMEN'S EMPOWERMENT ──────────────────────────────────────────────────
  "bank or savings account",               "Women's empowerment",  "Higher is better",
  "mobile phone",                          "Women's empowerment",  "Higher is better",
  "household decisions",                   "Women's empowerment",  "Higher is better",
  "hygienic methods",                      "Women's empowerment",  "Higher is better",
  "owning a house",                        "Women's empowerment",  "Higher is better",
  "worked in the last 12 months",          "Women's empowerment",  "Higher is better",
  "paid in cash",                          "Women's empowerment",  "Higher is better",
  "health insurance",                      "Women's empowerment",  "Higher is better",

  # ── DEMOGRAPHY ───────────────────────────────────────────────────────────
  "total fertility rate",                  "Demography",           "Lower is better",
  "third or higher order",                 "Demography",           "Lower is better",
  "married before age 18",                 "Demography",           "Lower is better",
  "married before age 21",                 "Demography",           "Lower is better",
  "adolescent fertility",                  "Demography",           "Lower is better",
  "already mothers or pregnant",           "Demography",           "Lower is better",
  "sex ratio",                             "Demography",           "Higher is better",
  "birth was registered",                  "Demography",           "Higher is better",
  "deaths in the last 3 years registered", "Demography",           "Higher is better",
  "below age 15",                          "Demography",           "Higher is better",
  "death registration",                    "Demography",           "Higher is better",
  "disability",                            "Demography",           "Lower is better",
  "pre-primary school",                    "Education",            "Higher is better",

  # ── GENDER & VIOLENCE ────────────────────────────────────────────────────
  "ever experienced spousal",              "Gender & violence",    "Lower is better",
  "physical violence during any pregnancy","Gender & violence",    "Lower is better",
  "sexual violence by age",               "Gender & violence",    "Lower is better",
  "spousal violence",                      "Gender & violence",    "Lower is better",
  "sexual violence",                       "Gender & violence",    "Lower is better",
  "physical violence",                     "Gender & violence",    "Lower is better",

  # ── HIV/AIDS ─────────────────────────────────────────────────────────────
  "hiv",                                   "HIV/AIDS",             "Higher is better",

  # ── LIFESTYLE ────────────────────────────────────────────────────────────
  "tobacco",                               "Lifestyle",            "Lower is better",
  "alcohol",                               "Lifestyle",            "Lower is better"
)


# =============================================================================
# SHARED HELPER FUNCTIONS (unchanged from NFHS-5 script)
# =============================================================================

lookup_meta <- function(indicator_name) {
  name_lower <- str_to_lower(indicator_name)
  for (i in seq_len(nrow(indicator_meta))) {
    if (str_detect(name_lower, fixed(indicator_meta$keyword[i]))) {
      return(tibble(domain    = indicator_meta$domain[i],
                    direction = indicator_meta$direction[i]))
    }
  }
  tibble(domain = "Unknown", direction = "Higher is better")
}

clean_name <- function(x) {
  x |>
    str_trim() |>
    str_remove("^\\d+\\.\\s*") |>
    str_remove_all("[¹²³⁴⁵⁶⁷⁸⁹⁰]+") |>
    str_replace_all("(?<=[a-zA-Z])(\\d{1,2})(?=\\s|\\()", "") |>
    str_remove("(?<=\\(%\\))\\s*\\d{1,2}\\s*$") |>
    str_remove("(?<=[a-zA-Z])\\s*\\d{1,2}\\s*$") |>
    # Remove footnote numbers sitting between ) and ( e.g. "age)18 (%)" → "age) (%)"
    # These differ across rounds for the same indicator, breaking cross-round joins.
    str_remove_all("\\)\\d{1,2}(?=\\s*\\()") |>
    str_squish()
}

to_num <- function(x) {
  x <- str_trim(x)
  if (is.na(x) || x == "" || x == "*") return(NA_real_)
  if (str_to_lower(x) %in% c("na", "n/a", "-", "—")) return(NA_real_)
  x <- str_remove_all(x, "[\\(\\)]")
  x <- str_remove_all(x, ",")
  m <- str_extract(x, "[0-9]+\\.?[0-9]*")
  if (is.na(m)) NA_real_ else as.numeric(m)
}

is_skip_line <- function(line) {
  line <- str_squish(line)
  if (nchar(line) == 0) return(TRUE)
  if (str_detect(line, "^\\d+\\.\\s+[A-Za-z]")) return(FALSE)

  section_headers <- c(
    "Key Indicators", "Maternity Care", "Delivery Care",
    "Child Vaccination", "Child Feeding", "Nutritional Status",
    "Anaemia among", "Blood Sugar", "Hypertension",
    "Characteristics of",
    "Population and Household",
    "Marriage and Fertility",
    "Infant and Child Mortality",
    "Current Use of Family",
    "Unmet Need for Family",
    "Quality of Family",
    "Treatment of Childhood",
    "Screening for Cancer",
    "Knowledge of HIV",
    "Women.s Empowerment",
    "Gender Based", "Tobacco Use",
    "International Institute",
    "Ministry of Health",
    "Suggested citation",
    "NATIONAL FAMILY HEALTH",
    "For additional information",
    "MAY 2026",
    "Table of Contents", "TABLE OF CONTENTS",
    "Introduction",
    "Appendix"
  )
  if (any(str_detect(line, section_headers))) return(TRUE)

  if (str_detect(line, "^(Urban|Rural|Total|Indicators|Note:|LHV\\s*=|na\\s*=)")) return(TRUE)
  if (str_detect(line, "^\\*\\s*Percentage")) return(TRUE)
  if (str_detect(line, "^\\(\\s*\\)\\s*Based")) return(TRUE)
  if (str_detect(line, "^NFHS-[0-9]\\s+\\(")) return(TRUE)
  if (str_detect(line, "^\\(20[0-9]{2}-[0-9]{2}\\)$")) return(TRUE)
  if (str_detect(line, "^\\(2023-24\\)")) return(TRUE)
  if (str_detect(line, "^\\(2019-2[01]\\)")) return(TRUE)

  if (str_detect(line, "^\\d{1,2}$")) return(TRUE)
  footnote_starts <- c(
    "^[0-9]+\\s+Piped", "^[0-9]+\\s+Flush", "^[0-9]+\\s+Electr",
    "^[0-9]+\\s+Refers", "^[0-9]+\\s+Equiv", "^[0-9]+\\s+Based",
    "^[0-9]+\\s+Among", "^[0-9]+\\s+Random", "^[0-9]+\\s+Since",
    "^[0-9]+\\s+Includes", "^[0-9]+\\s+Vaccinated",
    "^[0-9]+\\s+Any\\s+method", "^[0-9]+\\s+Unmet\\s+need",
    "^[0-9]+\\s+Not\\s+including", "^[0-9]+\\s+Haemoglobin",
    "^[0-9]+\\s+Breastfed", "^[0-9]+\\s+Locally",
    "^[0-9]+\\s+Below", "^[0-9]+\\s+Above", "^[0-9]+\\s+Excludes",
    "^[0-9]+\\s+Doctor", "^[0-9]+\\s+Comprehensive",
    "^[0-9]+\\s+Decisions", "^[0-9]+\\s+Spousal",
    "^[0-9]+\\s+Women\\s+who\\s+are\\s+classified",
    "^[0-9]+\\s+Hepatitis",   # new NFHS-6 footnote
    "^[0-9]+\\s+Rotavirus",   # new NFHS-6 footnote
    "^[0-9]+\\s+Children\\s+who\\s+received",  # new NFHS-6 footnote
    "^[0-9]+\\s+Measured",    # new NFHS-6 footnote (blood glucose method)
    "^[0-9]+\\s+An\\s+adequate",  # new NFHS-6 footnote
    "^[0-9]+\\s+According",   # new NFHS-6 footnote
    "^[0-9]+\\s+Defined",     # new NFHS-6 footnote
    "^[0-9]+\\s+Current"      # new NFHS-6 footnote
  )
  if (any(str_detect(line, footnote_starts))) return(TRUE)

  if (str_detect(line, "^[\\u00b7\\u22c5\\u2022]")) return(TRUE)
  if (str_detect(line, "^\\(\\s*\\)")) return(TRUE)

  FALSE
}

INDICATOR_REGEX <- paste0(
  "^((?:\\d+\\.\\s+)?[A-Za-z].+?)\\s{2,}",
  "((?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+))*)\\s*$"
)

CONTINUATION_REGEX <- paste0(
  "^\\s+(.+?)\\s{2,}",
  "((?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+))*)\\s*$"
)

process_lines <- function(lines, geo_name, geo_level, parent_state,
                          round_label, records, pending_name) {
  for (line in lines) {
    line_sq <- str_squish(line)
    if (is_skip_line(line_sq)) next

    m <- str_match(str_trim(line), INDICATOR_REGEX)

    if (!is.na(pending_name) && str_detect(line, "^\\s+")) {
      m2 <- str_match(line, CONTINUATION_REGEX)
      if (!is.na(m2[1, 1])) m <- m2
    } else if (is.na(m[1, 1]) && !is.na(pending_name)) {
      m2 <- str_match(line, CONTINUATION_REGEX)
      if (!is.na(m2[1, 1])) m <- m2
    }

    if (is.na(m[1, 1])) {
      if (str_detect(line_sq, "^\\d+\\.\\s+[A-Za-z]")) {
        pending_name <- line_sq
      } else if (!is.na(pending_name) && nchar(line_sq) > 3 &&
                 !str_detect(line_sq, "^[0-9]") &&
                 !str_detect(line_sq, "^(Vaccinated|Piped|Flush|Electr|Refers|Equiv|Based|Among|Random|Since|Includes|Any method|Unmet need|Not including|Haemoglobin|Breastfed|Locally|Below|Above|Excludes|Doctor|Comprehensive|Decisions|Spousal|Women who are classified|Hepatitis|Rotavirus|Children who received|Measured|An adequate|According|Defined|Current)")) {
        pending_name <- str_c(pending_name, " ", line_sq)
      }
      next
    }

    raw_name  <- str_squish(m[1, 2])
    vals_part <- str_squish(m[1, 3])

    if (!is.na(pending_name)) {
      raw_name     <- str_c(pending_name, " ", raw_name)
      pending_name <- NA_character_
    }

    val_tokens <- str_split(vals_part, "\\s+")[[1]]
    val_tokens <- val_tokens[nchar(val_tokens) > 0]
    if (length(val_tokens) < 1) next

    # Second-to-last token = NFHS-6 Total; last token = NFHS-5 Total
    nfhs6_val <- if (length(val_tokens) >= 2) to_num(val_tokens[length(val_tokens) - 1]) else to_num(val_tokens[1])
    nfhs5_val <- if (length(val_tokens) >= 2) to_num(val_tokens[length(val_tokens)])     else NA_real_

    if (is.na(nfhs6_val) && is.na(nfhs5_val)) next

    indicator_name <- clean_name(raw_name)
    if (nchar(indicator_name) < 6) next

    meta <- lookup_meta(indicator_name)

    if (!is.na(nfhs6_val)) {
      records <- c(records, list(tibble(
        Indicator      = indicator_name,
        Geography      = geo_name,
        `Geo Level`    = geo_level,
        Round          = round_label,        # "NFHS-6"
        Value          = nfhs6_val,
        `Parent State` = parent_state,
        Domain         = meta$domain,
        Direction      = meta$direction
      )))
    }
    if (!is.na(nfhs5_val)) {
      records <- c(records, list(tibble(
        Indicator      = indicator_name,
        Geography      = geo_name,
        `Geo Level`    = geo_level,
        Round          = "NFHS-5",           # previous round column
        Value          = nfhs5_val,
        `Parent State` = parent_state,
        Domain         = meta$domain,
        Direction      = meta$direction
      )))
    }
  }
  list(records = records, pending_name = pending_name)
}


# =============================================================================
# COMPENDIUM PARSER
#
# The NFHS-6 factsheets are published as a single PDF with all states/UTs.
# This function:
#   1. Splits the PDF into pages
#   2. Groups consecutive pages that share the same "X - Key Indicators" header
#   3. Processes each group as a single state block (same logic as parse_state_pdf)
#
# STATE NAME DETECTION:
#   Same regex as before: look for "- Key Indicators" suffix and extract
#   everything before the first dash as the geography name.
#
# PAGES TO SKIP:
#   Pages with no "- Key Indicators" header (cover, TOC, introduction,
#   annexures, appendices) are ignored automatically — they produce no
#   header match and are therefore never assigned to any state group.
# =============================================================================

extract_state_name_from_page <- function(page_text) {
  lines <- str_split(page_text, "\n")[[1]]
  header_line <- lines[str_detect(lines, "-\\s*Key Indicators")]
  if (length(header_line) == 0) return(NA_character_)
  name <- str_squish(str_extract(header_line[1], "^[^-]+"))
  if (is.na(name) || str_detect(name, ",")) return(NA_character_)  # skip district headers
  name
}

parse_compendium_pdf <- function(pdf_path) {
  message("\n📄 Reading: ", basename(pdf_path))
  pages <- tryCatch(pdf_text(pdf_path),
                    error = function(e) { message("❌ ", e$message); NULL })
  if (is.null(pages)) return(NULL)
  message("   Total pages: ", length(pages))

  # Tag each page with its geography name (NA for non-data pages)
  page_geos <- map_chr(pages, extract_state_name_from_page)

  # Group consecutive pages by geography
  geo_groups <- rle(page_geos)
  all_records <- list()
  page_idx    <- 1L

  for (g in seq_along(geo_groups$values)) {
    geo_raw   <- geo_groups$values[g]
    n_pages   <- geo_groups$lengths[g]
    page_idxs <- seq(page_idx, page_idx + n_pages - 1L)
    page_idx  <- page_idx + n_pages

    if (is.na(geo_raw)) next  # non-data pages

    geo_name  <- normalise_geo(geo_raw)
    geo_level <- if (geo_name == "India") "National" else "State"
    message("  [", geo_level, "] ", geo_name, " (", n_pages, " page(s))")

    records      <- list()
    pending_name <- NA_character_

    for (pi in page_idxs) {
      lines  <- str_split(pages[pi], "\n")[[1]]
      result <- process_lines(lines, geo_name, geo_level, "",
                              "NFHS-6", records, pending_name)
      records      <- result$records
      pending_name <- result$pending_name
    }

    if (length(records) == 0) { message("    ⚠️  No records extracted"); next }

    df <- bind_rows(records) |>
      distinct(Indicator, Geography, Round, .keep_all = TRUE)
    message("    ✅ ", nrow(df), " rows (", n_distinct(df$Indicator), " indicators)")
    all_records <- c(all_records, list(df))
  }

  if (length(all_records) == 0) { message("❌ No records at all."); return(NULL) }
  bind_rows(all_records)
}


# =============================================================================
# RUN
# =============================================================================

master_df <- parse_compendium_pdf(COMPENDIUM_PDF)

# Flag unknown domains so you can extend indicator_meta above
unknowns <- master_df |>
  filter(Domain == "Unknown") |>
  distinct(Indicator) |>
  pull(Indicator)

if (length(unknowns) > 0) {
  message("\n⚠️  ", length(unknowns), " indicator(s) with Unknown domain — add keywords above:")
  walk(unknowns, ~ message('  "', str_to_lower(str_sub(.x, 1, 60)), '",'))
} else {
  message("\n✅ All indicators matched to a domain.")
}

write_csv(master_df |> filter(Round == "NFHS-6"), OUTPUT_FILE, append = TRUE)

message("\n✅ Done!")
message("   Total rows   : ", nrow(master_df))
message("   Geographies  : ", n_distinct(master_df$Geography))
message("   Indicators   : ", n_distinct(master_df$Indicator))
message("   Rounds       : ", str_c(unique(master_df$Round), collapse = ", "))
message("   Output       : ", OUTPUT_FILE)


# =============================================================================
# DIAGNOSTIC — run after the main script to see which indicator lines are
# matched vs missed. Change DIAG_GEO to any state name to inspect it.
# =============================================================================

# DIAG_GEO  <- "Bihar"
# DIAG_PAGES <- which(map_chr(pdf_text(COMPENDIUM_PDF), extract_state_name_from_page) == DIAG_GEO)
# pages_all  <- pdf_text(COMPENDIUM_PDF)
#
# results <- list()
# for (pi in DIAG_PAGES) {
#   lines <- str_split(pages_all[pi], "\n")[[1]]
#   for (line in lines) {
#     line_sq <- str_squish(line)
#     if (!str_detect(line_sq, "^\\d+\\.\\s+[A-Za-z]")) next
#     m      <- str_match(str_trim(line), INDICATOR_REGEX)
#     status <- if (!is.na(m[1, 1])) "MATCH" else "NO MATCH"
#     results <- c(results, list(data.frame(status = status, line = line_sq)))
#   }
# }
# df_diag <- bind_rows(results)
# cat("Matched  :", sum(df_diag$status == "MATCH"),    "\n")
# cat("No match :", sum(df_diag$status == "NO MATCH"), "\n")
# df_diag |> filter(status == "NO MATCH") |> pull(line) |> walk(~ cat(.x, "\n"))
