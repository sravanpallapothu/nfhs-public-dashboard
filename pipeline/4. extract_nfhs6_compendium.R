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

COMPENDIUM_PDF <- "C:/Users/Cegis/Desktop/claude cowork/nfhs dashboard/NFHS_6_Factsheets.pdf"
OUTPUT_FILE    <- "C:/Users/Cegis/Downloads/NFHS all factsheets/nfhs_all_data.csv"


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

# \u2500\u2500 normalise_geo \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Applies the GEO_NAME_FIX crosswalk. Returns the fixed name if a match exists,
# otherwise returns the input name unchanged.
# Called on every geo_raw extracted from the compendium header before storing,
# so that Geography values in this output match the NFHS-5 data exactly
# and the dashboard join on Geography works without manual post-processing.
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

# \u2500\u2500 lookup_meta \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Scans indicator_meta top-to-bottom for the first keyword found in the name.
# Uses fixed() for literal string matching (no regex interpretation of keywords).
# Returns domain="Unknown" if nothing matches -- these are flagged at run end
# so you can add the missing keyword to indicator_meta.
# Identical to script 2. Three new keywords were added for NFHS-6 indicators:
#   "hepatitis b"         - Hepatitis B vaccine coverage (indicator 52)
#   "rotavirus"           - Rotavirus vaccine coverage (indicator 53)
#   "solid or semi-solid" - Complementary feeding (indicator 65)
#   "attended pre-school" - Pre-school attendance (NFHS-6 wording variant)
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

# \u2500\u2500 clean_name \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Cleans raw indicator name text extracted from the PDF.
# Identical logic to the NFHS-5 script (script 2) -- 12 sequential steps.
#
# THE PROBLEM THIS SOLVES:
#   pdftools extracts NFHS table text with embedded noise: leading indicator
#   numbers, Unicode superscript footnote markers, footnote digits attached in
#   4 different styles, absorbed value tokens, concatenated indicator numbers,
#   BMI unit variants, and disclaimer text. Without cleaning, the same real-world
#   indicator appears as dozens of unique strings, breaking the dashboard dropdown
#   and cross-round joins between NFHS-4, 5 and 6.
#
# STEPS 1-2 -- Structural noise
#   Step 1: Leading indicator number  "50. Institutional births" -> name only
#   Step 2: Unicode superscript markers 1,2,3 (U+00B9 etc.) -- distinct from ASCII
#
# STEPS 3-8 -- Footnote reference digits (4 attachment styles)
#   Footnote numbers differ between NFHS rounds for the same indicator
#   (e.g. footnote 11 in NFHS-5 vs 14 in NFHS-6), so they MUST be stripped
#   or the same indicator creates two separate dropdown entries.
#   Step 3: Glued to letter + comma-list  "recall11 (%)" -> "recall (%)"
#   Step 4: After (%) at end              "(%)14" -> "(%)"
#   Step 5: Trailing after a letter       "...personnel11" -> "...personnel"
#   Step 6: Between ) and ( + comma-list  "age)10, 11 (%)" -> "age) (%)"
#   Step 7: Spaced comma-list before (    "diet 10, 11 (%)" -> "diet (%)"
#   Step 8: Lone spaced digit before (    "birth 15 (%)" -> "birth (%)"
#           (?<!age) keeps "married before age 18 (%)" intact
#
# STEPS 9-12 -- Systematic PDF extraction artefacts (added after diagnostic runs)
#
#   Step 9:  Concatenated indicator numbers
#            "...vaccine (%) 64. Children age 12-23 months..." -> "...vaccine (%)"
#            Cause: PDF line compression or pending_name cascade joins two indicators.
#            Pattern \s+\d{1,3}\.\s+[A-Z].* uniquely matches indicator-number format.
#
#   Step 10: Trailing absorbed value tokens
#            "...facility (Rs.) (2,965)" -> "...facility (Rs.)"
#            Cause: 1-space column gap causes lazy regex to absorb first value.
#            Salvage handles primary case; Step 10 is a final backstop.
#            Safe: unit markers (%), (Rs.) contain non-digit chars -- never matched.
#
#   Step 11: BMI unit normalisation -> canonical "kg/m2) (%)"
#            Step 3 strips "2" inconsistently: "m2 (%)" loses the 2 but "m2) (%)"
#            does not -- producing 3 variants across states for the same indicator.
#
#   Step 12: Disclaimer text absorbed after (%)
#            "...blood pressure (%) Practices (fed with other milk...)" (7 states)
#            Cause: page-bottom footnote text absorbed via pending_name cascade.
#            Fix: strip everything after the first (%).
#            Safe: every valid NFHS indicator name ends at its (%) unit marker.
clean_name <- function(x) {
  x |>
    str_trim() |>
    str_remove("^\\d+\\.\\s*") |>
    str_remove_all("[¹²³⁴⁵⁶⁷⁸⁹⁰]+") |>
    # Step 3: remove inline footnote digits glued to a letter, before space or (
    # Extended to handle comma-separated lists: "recall10, 11 (%)" -> "recall (%)"
    str_replace_all("(?<=[a-zA-Z])(\\d{1,2})(?:\\s*,\\s*\\d{1,2})*(?=\\s|\\()", "") |>
    str_remove("(?<=\\(%\\))\\s*\\d{1,2}\\s*$") |>
    str_remove("(?<=[a-zA-Z])\\s*\\d{1,2}\\s*$") |>
    # Step 6: remove footnote(s) between ) and ( e.g. "age)18 (%)" or "age)10, 11 (%)"
    str_remove_all("\\)\\d{1,2}(?:\\s*,\\s*\\d{1,2})*(?=\\s*\\()") |>
    # Step 7: remove spaced comma-list footnote before ( e.g. "diet 10, 11 (%)" -> "diet (%)"
    str_remove_all("\\s\\d{1,2}(?:\\s*,\\s*\\d{1,2})+(?=\\s*\\()") |>
    # Step 8: remove lone spaced footnote before ( e.g. "birth 15 (%)" -> "birth (%)"
    # (?<!age) preserves meaningful age thresholds: "age 18 (%)" is left untouched
    str_remove_all("(?<!age)\\s\\d{1,2}(?=\\s*\\()") |>
    # Step 9: strip concatenated indicator numbers e.g. "...vaccine (%) 64. Children..."
    str_remove("\\s+\\d{1,3}\\.\\s+[A-Z].*") |>
    # Step 10: strip trailing absorbed value tokens e.g. "...per 1,000 males) (1,111)"
    str_remove("\\s+(?:\\([\\d\\.,]+\\)|[\\d\\.,]+)(?:\\s+(?:\\([\\d\\.,]+\\)|[\\d\\.,]+))*\\s*$") |>
    # Step 11: normalise BMI unit variants to canonical "kg/m2) (%)"
    str_replace_all("kg/m\\s*2?\\s*\\)\\s*\\(%\\)", "kg/m2) (%)") |>
    str_replace_all("kg/m\\s*2?\\s*\\(%\\)",        "kg/m2) (%)") |>
    # Step 12: strip footnote/disclaimer text absorbed after the unit marker (%)
    # e.g. "...blood pressure (%) Practices (fed with other milk...)" -> "...blood pressure (%)"
    str_replace("(\\(%\\)).*$", "\\1") |>
    str_squish()
}

# \u2500\u2500 to_num \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Converts a raw string token from the PDF to a numeric value.
# Handles four non-standard representations used in NFHS fact sheets:
#   "*"     -> fewer than 25 unweighted cases -- not reportable -> NA
#   "na"    -> indicator not available in that NFHS round -> NA
#   "(3.7)" -> parenthesised: valid estimate flagged for small sample size.
#              Strip parentheses and use the number -- the value IS valid.
#   "3,511" -> comma-formatted large numbers (out-of-pocket Rs., sex ratios).
#              Strip commas before parsing.
# Identical to the NFHS-5 script version.
to_num <- function(x) {
  x <- str_trim(x)
  if (is.na(x) || x == "" || x == "*") return(NA_real_)
  if (str_to_lower(x) %in% c("na", "n/a", "-", "—")) return(NA_real_)
  x <- str_remove_all(x, "[\\(\\)]")
  x <- str_remove_all(x, ",")
  m <- str_extract(x, "[0-9]+\\.?[0-9]*")
  if (is.na(m)) NA_real_ else as.numeric(m)
}

# \u2500\u2500 is_skip_line \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Returns TRUE for lines that should be ignored during extraction.
# Applied to str_squish(line). Five rules, checked in order:
#
# Rule 1 (sacred override): numbered indicator lines are NEVER skipped.
#   Prevents section-header keywords from killing real indicators -- e.g.
#   "Blood Sugar" as a skip phrase would also kill indicators 86-97 which
#   all contain those words. Checked first so no later rule can override it.
#
# Rule 2: Section header phrases -- bold category headings in NFHS tables.
#   NFHS-6 additions vs script 2: "Suggested citation", "NATIONAL FAMILY
#   HEALTH", "For additional information", "MAY 2026", "Table of Contents",
#   "Introduction", "Appendix" -- these appear in the compendium front matter
#   and annexures. Without them, these lines fall through to Case A and
#   corrupt the next real indicator via the pending_name mechanism.
#
# Rule 3: Column headers and round/year label lines.
#   NFHS-6 additions: "(2023-24)" and "(2019-21)" -- the compendium prints
#   NFHS round year labels as standalone lines between state blocks.
#
# Rule 4: Footnote lines (bare number, or number + known keyword).
#   NFHS-6 additions vs script 2: Hepatitis, Rotavirus, "Children who received",
#   Measured, "An adequate", According, Defined, Current -- new footnotes.
#
# Rule 5: Footnote continuation lines (bullet points, empty bracket lines).
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

# \u2500\u2500 INDICATOR_REGEX / CONTINUATION_REGEX \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Both identical to script 2. Applied to str_trim(line), NOT str_squish,
# to preserve the 2+ space column gaps that separate names from values.
# (str_squish would collapse those gaps to single spaces, breaking the split.)
#
# INDICATOR_REGEX splits one table line into:
#   Group 1: indicator name -- lazy .+? stops at the FIRST 2+ space gap
#   Group 2: value tokens -- * | na | parenthesised number | plain number
#
# CONTINUATION_REGEX: same but anchored with leading \s+.
# Used when pending_name is active -- continuation lines like
# "      Diastolic >=90 mm of Hg) (%)" must be joined to the pending name.
#
# CHANGED vs original: \([\d\.,]+\) -- comma added (was \([\d\.]+\)).
# WHY: some states have parenthesised large numbers e.g. "(3,511)" for
# out-of-pocket expenditure. Without the comma these failed as value tokens,
# the whole line fell to Case A -> pending_name -> cascade -> 34-word garbage.
# Same comma fix applied to salvage_pat inside process_lines().
INDICATOR_REGEX <- paste0(
  "^((?:\\d+\\.\\s+)?[A-Za-z].+?)\\s{2,}",
  "((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
)

CONTINUATION_REGEX <- paste0(
  "^\\s+(.+?)\\s{2,}",
  "((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
)

# \u2500\u2500 process_lines \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Inner extraction loop. Same structure as script 2 with two key differences.
#
# DIFFERENCE 1 -- Only NFHS-6 values are stored (not NFHS-5):
#   The compendium has 4 columns: Urban | Rural | NFHS-6 Total | NFHS-5 Total.
#   Only NFHS-6 Total (second-to-last token) is extracted. The NFHS-5 comparison
#   column is ignored -- script 2 already provides NFHS-4/5 data from per-state
#   PDFs. Extracting NFHS-5 from both scripts would create duplicate rows.
#
# DIFFERENCE 2 -- No truncation check (deliberately absent):
#   Script 2 has a check after salvage: if raw_name does not end with ")" it
#   stores the name as pending_name and skips the row (next), expecting the
#   continuation line to deliver the full name + values. This is safe in
#   script 2 because each per-state PDF is processed independently and the
#   continuation always follows on the same or next page.
#
#   In this script the truncation check was tested and caused ~600 rows of
#   data loss across two production runs:
#     "Average out-of-pocket expenditure..."  -> 0 states  (completely gone)
#     "Children born at home...24 hours"      -> 0 states  (completely gone)
#     "Mothers who received postnatal care"   -> 1 state   (35 lost)
#     "Mildly elevated blood pressure"        -> 9 states  (27 lost)
#   Root cause: in the multi-state compendium some state sections have layouts
#   where the continuation does not satisfy CONTINUATION_REGEX (different page,
#   no leading spaces, intervening skip lines). The `next` call discards the
#   row permanently. When a wrong indicator eventually absorbs pending_name via
#   Case C, that row is stored with garbled data and the original is gone.
#   Decision: no truncation check in script 4. Some NFHS-6 names remain truncated
#   for states where the PDF wraps differently. Steps 9-12 in clean_name() and
#   the JS normalisation in index.html handle residual name cleanup.
#
# WRAPPED INDICATOR NAMES -- pending_name (same as script 2):
#   Case A: starts with indicator number, no values -> store as pending_name
#   Case B: no number, no values -- name continuation -> append to pending_name
#   Case C: has values + pending_name -> prepend pending_name to raw_name
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

    # VALUE SALVAGE -- strips an absorbed first-column value from raw_name.
    # WHY NEEDED: when only 1 space separates the indicator name from the Urban
    # column, the lazy .+? in INDICATOR_REGEX absorbs that value into the name,
    # creating one unique garbage name per state. Without salvage, "blood pressure
    # (%) 22.3" for Karnataka and "blood pressure (%) 19.8" for Maharashtra would
    # both be stored as separate indicators. Salvage strips the trailing tokens
    # from raw_name and prepends to vals_part; "second-to-last" still gives
    # the correct NFHS-6 Total.
    # e.g. raw_name="...control blood 12.9"  vals_part="10.4 10.8 na"
    #   -> raw_name="...control blood"        vals_part="12.9 10.4 10.8 na"
    # CHANGED: salvage_pat uses \([\d\.,]+\) -- comma added to catch large
    # parenthesised values like "(3,511)" that \([\d\.]+\) previously missed.
    salvage_pat <- "\\s+((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
    salvaged    <- str_match(raw_name, salvage_pat)
    if (!is.na(salvaged[1, 1])) {
      raw_name  <- str_remove(raw_name, salvage_pat)
      vals_part <- str_squish(str_c(salvaged[1, 2], " ", vals_part))
    }

    # DUAL-INDICATOR SPLIT -------------------------------------------------------
    # Same pattern as script 2: some states in the NFHS-6 compendium place two
    # consecutive indicators on one physical line (only 1 space between them).
    # INDICATOR_REGEX captures both names; vals_part = indicator N+1's values.
    # Store indicator N+1 correctly; skip indicator N (no recoverable values).
    dual_match <- str_match(raw_name, "\\s+(\\d{1,3}\\.\\s+[A-Za-z].+)$")
    if (!is.na(dual_match[1, 1])) {
      second_name <- clean_name(dual_match[1, 2])
      if (nchar(second_name) >= 6) {
        s_tokens <- str_split(str_squish(vals_part), "\\s+")[[1]]
        s_tokens <- s_tokens[nchar(s_tokens) > 0]
        if (length(s_tokens) >= 1) {
          s6 <- if (length(s_tokens) >= 2) to_num(s_tokens[length(s_tokens) - 1]) else to_num(s_tokens[1])
          s_meta <- lookup_meta(second_name)
          if (!is.na(s6))
            records <- c(records, list(tibble(
              Indicator = second_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = round_label, Value = s6, `Parent State` = parent_state,
              Domain = s_meta$domain, Direction = s_meta$direction)))
        }
      }
      pending_name <- NA_character_
      next  # first indicator has no recoverable values on this line
    }
    # -------------------------------------------------------------------------

    val_tokens <- str_split(vals_part, "\\s+")[[1]]
    val_tokens <- val_tokens[nchar(val_tokens) > 0]
    if (length(val_tokens) < 1) next

    # Column order: Urban | Rural | NFHS-6 Total | NFHS-5 Total
    # Second-to-last = NFHS-6 Total; last = NFHS-5 comparison.
    # Only NFHS-6 Total is extracted. The NFHS-5 column is intentionally skipped --
    # script 2 is the authoritative source for NFHS-4 and NFHS-5 values.
    # Writing NFHS-5 from both scripts would create duplicate rows in the master sheet.
    nfhs6_val <- if (length(val_tokens) >= 2) to_num(val_tokens[length(val_tokens) - 1]) else to_num(val_tokens[1])

    if (is.na(nfhs6_val)) next

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

# \u2500\u2500 extract_state_name_from_page \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Returns the geography name from a single page's header line, or NA.
# Called by parse_compendium_pdf on every page to tag it with its state.
# Returns NA for: non-data pages (no header), and any page whose header
# contains a comma -- those are district headers ("Jaisalmer, Rajasthan")
# which should not appear in a state compendium but are guarded against.
# The returned name is the raw compendium spelling (not yet normalised);
# normalise_geo() is applied later in parse_compendium_pdf.
extract_state_name_from_page <- function(page_text) {
  lines <- str_split(page_text, "\n")[[1]]
  header_line <- lines[str_detect(lines, "-\\s*Key Indicators")]
  if (length(header_line) == 0) return(NA_character_)
  name <- str_squish(str_extract(header_line[1], "^[^-]+"))
  if (is.na(name) || str_detect(name, ",")) return(NA_character_)  # skip district headers
  name
}

# \u2500\u2500 parse_compendium_pdf \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Reads the NFHS-6 compendium PDF and returns one row per state per indicator.
#
# WORKFLOW:
#   1. pdf_text() extracts all pages as raw text strings.
#   2. map_chr() calls extract_state_name_from_page() on every page to produce
#      a character vector the same length as pages (NA for non-data pages).
#   3. rle() (run-length encoding) groups consecutive same-named pages into
#      state blocks -- e.g. Bihar pages 12-14 form one group, Goa pages 15-16
#      form another. This handles states that span multiple pages naturally.
#   4. Each block is processed by process_lines() with pending_name threaded
#      across pages within the block so wrapped indicator names join correctly.
#   5. bind_rows() + distinct() deduplicates within each state block.
#      (Same reason as script 2 -- NFHS fact sheets sometimes reprint the
#       first row of a table at the top of the next page.)
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
#
# OUTPUT STRATEGY:
#   This script writes in two ways:
#   1. nfhs6_only.csv  -- NFHS-6 rows only, for review and QA before merging.
#      Use this to run the low-coverage diagnostic and check indicator counts.
#   2. OUTPUT_FILE (append=TRUE) -- appends NFHS-6 rows directly to the master
#      CSV that script 2 wrote. The schema is identical so no realignment needed.
#
# IMPORTANT: run script 2 first (or ensure OUTPUT_FILE already contains the
# NFHS-4 and NFHS-5 rows). append=TRUE adds to an existing file; if OUTPUT_FILE
# doesn't exist yet write_csv will create it, but you'll be missing NFHS-4/5 data.
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

master_df <- master_df |> arrange(Indicator)

# =============================================================================
# FRAGMENT FILTER
# Removes extraction artefacts -- indicator name variants and continuation
# fragments that appear in very few geographies AND look incomplete.
# BOTH conditions must be true to drop a row -- this is deliberate:
#
# Condition 1: fewer than 5 geographies.
#   Real NFHS-6 indicators appear in most or all 36 states. Artefacts typically
#   appear in 1-4 states where the compendium PDF layout caused a mis-extraction.
#
# Condition 2: name looks incomplete -- EITHER:
#   (a) Does not end with ")" -- missing unit marker like (%), (Rs.), etc.
#   (b) Fewer than 5 words -- too short to be a real indicator.
#
# The AND logic preserves legitimate low-coverage indicators:
#   e.g. "Children age 5 years who attended pre-primary school during 2020-21 (%)"
#   appears only for 1 district (Bhind, COVID-year anomaly) but ends with ")"
#   and has 14 words -- condition 2 is FALSE so it is correctly kept.
# =============================================================================
geo_counts <- master_df |> count(Indicator, name = "n_geos")

fragments <- geo_counts |>
  filter(
    n_geos < 5,
    !str_detect(Indicator, "\\)\\s*$") |
    str_count(Indicator, "\\S+") < 5
  ) |>
  pull(Indicator)

if (length(fragments) > 0) {
  message("\n\U0001f9f9 Removing ", length(fragments),
          " extraction fragment(s) (< 5 geos AND incomplete name):")
  walk(fragments, ~ message('   "', str_sub(.x, 1, 70), '"'))
  master_df <- master_df |> filter(!Indicator %in% fragments)
}

# Export NFHS-6 only rows for review and QA.
# Use this file to run the low-coverage diagnostic before appending to master.
write_csv(master_df, "nfhs6_only.csv")

# Append NFHS-6 rows to the master CSV (already contains NFHS-4 and NFHS-5).
# append=TRUE adds rows without repeating the header line. The column order
# must exactly match script 2's output -- both use the same 8-column schema:
# Indicator | Geography | Geo Level | Round | Value | Parent State | Domain | Direction
write_csv(master_df, OUTPUT_FILE, append = TRUE)

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
