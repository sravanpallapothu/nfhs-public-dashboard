# =============================================================================
# NFHS MASTER EXTRACTOR
# Language: R
#
# WHAT THIS SCRIPT DOES:
#   Reads all state AND district NFHS-5 fact sheet PDFs from two folders,
#   extracts indicator values, and writes a single combined CSV ready for
#   Google Sheets and the dashboard.
#
# OUTPUT COLUMNS:
#   Indicator | Geography | Geo Level | Round | Value | Parent State | Domain | Direction
#
# WHY ONE SCRIPT FOR BOTH STATE AND DISTRICT:
#   State and district fact sheets share the same table structure and the same
#   indicator names. The only differences are:
#     1. Header format (state vs district+state)
#     2. Number of value columns (4 for states, 2 for districts)
#     3. District sheets have parenthesised values (3.7) and asterisks *
#   These differences are handled by the shared helpers below.
#   The core line-matching logic is written once in process_lines() and
#   called by both parsers — no duplication.
#
# FOLDER STRUCTURE EXPECTED:
#   STATE_FOLDER    → one PDF per state   e.g. OF43_BR.pdf
#   DISTRICT_FOLDER → one PDF per district e.g. BR_Patna.pdf
#
# INSTALL (run once):
#   install.packages(c("pdftools", "tidyverse"))
# =============================================================================

library(pdftools)   # reads PDF files and extracts raw text per page
library(tidyverse)  # dplyr, stringr, purrr, readr, tibble


# =============================================================================
# CONFIG
# =============================================================================

STATE_FOLDER    <- "C:/Users/X/Downloads/NFHS all factsheets/state"
DISTRICT_FOLDER <- "C:/Users/X/Downloads/NFHS all factsheets/districts"
OUTPUT_FILE     <- "C:/Users/X/Downloads/NFHS all factsheets/nfhs_all_data.csv"


# =============================================================================
# DOMAIN + DIRECTION LOOKUP TABLE
#
# PURPOSE:
#   The PDF does not contain domain or direction information — we infer it
#   from the indicator name using partial keyword matching.
#
# HOW MATCHING WORKS:
#   For each extracted indicator name, the script scans this table top-to-bottom
#   and returns the domain/direction for the FIRST keyword found anywhere in
#   the name (case-insensitive). So ORDER MATTERS — more specific keywords
#   must appear before more general ones.
#
# EXAMPLE OF WHY ORDER MATTERS:
#   "blood sugar level" must appear before "blood sugar" which must appear
#   before "blood pressure" — otherwise "blood sugar level - high" would
#   incorrectly match "blood pressure" (since "blood" appears in both).
#   Similarly "elevated blood pressure" (NCD indicator) must appear before
#   "blood pressure" (Women's health indicator) to get the right domain.
#
# DISTRICT-SPECIFIC INDICATOR:
#   "third or higher order" covers indicator 17 in district sheets:
#   "Births in the 5 years preceding the survey that are third or higher order (%)"
#   This indicator does not appear in state sheets.
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
  # These MUST appear before the more general "blood sugar" / "blood pressure"
  # keywords below, otherwise NCD indicators (99-110 in state sheets,
  # 86-97 in district sheets) would be misclassified as Women's health.
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
  # "attended pre-primary" before "attended school" — both contain "attended"
  # so the more specific one must come first
  "attended pre-primary",                  "Education",            "Higher is better",
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
  "third or higher order",                 "Demography",           "Lower is better",  # district sheets only
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
  # More specific phrases first — "ever experienced spousal" and
  # "physical violence during any pregnancy" are more specific than the
  # general "spousal violence" / "physical violence" keywords below them
  "ever experienced spousal",              "Gender & violence",    "Lower is better",
  "physical violence during any pregnancy","Gender & violence",    "Lower is better",
  "sexual violence by age",                "Gender & violence",    "Lower is better",
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
# SHARED HELPER FUNCTIONS
# All functions below are used by both the state and district parsers.
# =============================================================================

# ── lookup_meta ───────────────────────────────────────────────────────────────
# Scans indicator_meta top-to-bottom for the first keyword found in the name.
# Uses fixed() for literal string matching (no regex interpretation of keywords).
# Returns "Unknown" if nothing matches — unknown indicators are flagged at end.
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


# ── clean_name ────────────────────────────────────────────────────────────────
# Cleans raw indicator name text extracted from the PDF.
#
# Step 1: str_remove("^\\d+\\.\\s*")
#   Removes the leading indicator number e.g. "50. " from "50. Institutional births"
#   Pattern: one or more digits, literal dot, optional whitespace
#
# Step 2: str_remove_all("[¹²³⁴⁵⁶⁷⁸⁹⁰]+")
#   Removes Unicode superscript characters that appear as footnote markers
#   in NFHS PDFs e.g. "drinking-water source¹" → "drinking-water source"
#   These are DIFFERENT characters from regular digits (Unicode U+00B9 etc.)
#   so they need their own removal step, not the digit-removal step below
#
# Step 3: str_remove_all("(?<=[a-zA-Z\\)\\s])\\d{1,2}\\s*$")
#   Removes trailing 1-2 digit footnote reference numbers at the END of names
#   e.g. "skilled health personnel10" → "skilled health personnel"
#   The lookbehind (?<=[a-zA-Z\\)\\s]) ensures we ONLY strip digits that
#   follow a letter, closing bracket, or space.
#   WHY THIS MATTERS: a naive \\d+\\s*$ would also strip legitimate numbers
#   from names like "age 15-49 years" (removes "49") or "BMI <18.5" (removes "5")
#   The lookbehind prevents this — it only strips digits that come after text
#
# Step 4: str_squish()
#   Collapses multiple internal spaces and trims leading/trailing whitespace.
#   Needed because removing superscripts sometimes leaves double spaces.
clean_name <- function(x) {
  x |>
    str_trim() |>
    # Step 1: remove leading indicator number e.g. "50. "
    str_remove("^\\d+\\.\\s*") |>
    # Step 2: remove Unicode superscript footnote markers e.g. \u00b9\u00b2\u00b3
    str_remove_all("[\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u2070]+") |>
    # Step 3: remove INLINE footnote numbers embedded in word before "(%)" or space
    # e.g. "only12 (%)" -> "only (%)", "recall11 (%)" -> "recall (%)"
    # e.g. "sugar level23 (%)" -> "sugar level (%)"
    # Pattern: letter immediately followed by 1-2 digits then space or (
    # Negative lookbehind for digit prevents stripping from "12-23", "15-49"
    str_replace_all("(?<=[a-zA-Z])(\\d{1,2})(?=\\s|\\()", "") |>
    # Step 4: remove trailing footnote number after (%) e.g. "... (%)11" -> "... (%)"
    str_remove("(?<=\\(%\\))\\s*\\d{1,2}\\s*$") |>
    # Step 5: remove trailing footnote after plain letter/word at end of string
    # NOT after digit — protects "12-23", "15-49", "BMI <18.5" etc.
    str_remove("(?<=[a-zA-Z])\\s*\\d{1,2}\\s*$") |>
    str_squish()
}


# ── to_num ────────────────────────────────────────────────────────────────────
# Converts a raw string token from the PDF to a numeric value.
#
# WHY SPECIAL HANDLING IS NEEDED:
#   NFHS fact sheets contain several non-standard value representations:
#
#   "*"      → asterisk means fewer than 25 unweighted cases — not reportable
#              Returns NA. Only appears in DISTRICT sheets (small sample sizes).
#
#   "na"     → not available — indicator was not collected in that round (usually NFHS-4)
#              Returns NA. Appears in BOTH state and district sheets.
#
#   "(3.7)"  → parenthesised value — valid number but flagged as small sample size
#              Appears only in DISTRICT sheets.
#              We strip the parentheses and extract the number — the value IS valid,
#              it's just a warning that the estimate has wider confidence intervals.
#
#   "1,090"  → comma-formatted large numbers (e.g. sex ratios, out-of-pocket costs)
#              We strip commas before parsing.
to_num <- function(x) {
  x <- str_trim(x)
  if (is.na(x) || x == "" || x == "*") return(NA_real_)
  if (str_to_lower(x) %in% c("na", "n/a", "-", "—")) return(NA_real_)
  x <- str_remove_all(x, "[\\(\\)]")   # strip parentheses: (3.7) → 3.7
  x <- str_remove_all(x, ",")          # strip commas: 1,090 → 1090
  m <- str_extract(x, "[0-9]+\\.?[0-9]*")
  if (is.na(m)) NA_real_ else as.numeric(m)
}


# ── is_skip_line ──────────────────────────────────────────────────────────────
# Decides whether a line should be ignored entirely.
# Applied to the SQUISHED version of each line (line_sq).
#
# WHY WE NEED RULES RATHER THAN A SIMPLE KEYWORD LIST:
#   A flat keyword skip list caused valid indicators to be wrongly dropped.
#   For example, putting "Blood Sugar" in the skip list (to catch the section
#   header "Blood Sugar Level among Adults") also killed indicators 99-110
#   which all contain "blood sugar" in their names.
#   The solution is RULE 1: any line starting with an indicator number is
#   sacred and can NEVER be skipped, regardless of what words it contains.
#
# RULE 1 — Sacred override: numbered indicator lines are never skipped
#   Pattern: "^\\d+\\.\\s+[A-Za-z]"
#   Matches: "99. Blood sugar level - high..." / "105. Mildly elevated..."
#   WHY FIRST: this rule must be checked before all others so that no
#   section-header keyword can accidentally kill a real indicator line.
#
# RULE 2 — Section header lines (no leading number, known phrases)
#   These are the bold category headings in NFHS tables e.g.:
#   "Maternity Care (for last birth in the 5 years before the survey)"
#   "Characteristics of Adults (age 15-49 years)"
#   The patterns are written broadly enough to match both state variants
#   ("Characteristics of Adults") and district variants ("Characteristics of Women")
#   using "Characteristics of" as the shared stem.
#
# RULE 3 — Column header rows and round label rows
#   "Urban Rural Total Total" — the column labels row
#   "Total Total" — district sheet column labels (no Urban/Rural)
#   "NFHS-5 (2019-20)" — the round year header row
#   "(2019-20)" — year label that sometimes appears on its own line
#   "na = Not available" — footnote abbreviation line at page bottom
#   "* Percentage not shown" — district sheet small-sample footnote
#
# RULE 4 — Footnote lines
#   NFHS fact sheets have numbered footnotes at the bottom of each page.
#   Two types:
#   Type A: bare number on its own line e.g. just "1" or "22" — these are
#           superscript footnote numbers that pdftools extracts as separate lines
#   Type B: number followed by the footnote text e.g. "1 Piped water into..."
#           Each keyword below anchors to a specific NFHS footnote type.
#           WHY NOT JUST MATCH "^[0-9]+"?  Because that would also skip
#           valid indicators starting with their number e.g. "1. Female population..."
#           — but Rule 1 above saves those before we reach Rule 4.
#
# RULE 5 — Footnote continuation lines
#   The "Unmet need" footnote spans multiple lines with bullet points:
#   "· At risk of becoming pregnant, not using contraception..."
#   "( ) Based on 25-49 unweighted cases" — district sheet note
is_skip_line <- function(line) {
  line <- str_squish(line)
  if (nchar(line) == 0) return(TRUE)
  
  # RULE 1: numbered indicator lines are NEVER skipped (most important rule)
  if (str_detect(line, "^\\d+\\.\\s+[A-Za-z]")) return(FALSE)
  
  # RULE 2: section header phrases
  section_headers <- c(
    "Key Indicators", "Maternity Care", "Delivery Care",
    "Child Vaccination", "Child Feeding", "Nutritional Status",
    "Anaemia among", "Blood Sugar", "Hypertension",
    "Characteristics of",       # covers "of Adults" and "of Women" variants
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
    "Ministry of Health"
  )
  if (any(str_detect(line, section_headers))) return(TRUE)
  
  # RULE 3: column headers, round labels, abbreviation lines
  if (str_detect(line, "^(Urban|Rural|Total|Indicators|Note:|LHV\\s*=|na\\s*=)")) return(TRUE)
  if (str_detect(line, "^\\*\\s*Percentage")) return(TRUE)          # district: "* Percentage not shown"
  if (str_detect(line, "^\\(\\s*\\)\\s*Based")) return(TRUE)        # district: "( ) Based on 25-49..."
  if (str_detect(line, "^NFHS-[0-9]\\s+\\(")) return(TRUE)          # "NFHS-5 (2019-20)"
  if (str_detect(line, "^\\(20[0-9]{2}-[0-9]{2}\\)$")) return(TRUE) # "(2019-20)" on own line
  
  # RULE 4: footnote lines — bare number or number + known footnote keyword
  if (str_detect(line, "^\\d{1,2}$")) return(TRUE)  # bare superscript number
  footnote_starts <- c(
    "^[0-9]+\\s+Piped",         # footnote 1:  improved drinking water definition
    "^[0-9]+\\s+Flush",         # footnote 2:  improved sanitation definition
    "^[0-9]+\\s+Electr",        # footnote 3:  clean fuel definition
    "^[0-9]+\\s+Refers",        # footnote 4:  literacy definition
    "^[0-9]+\\s+Equiv",         # footnote 5:  adolescent fertility rate definition
    "^[0-9]+\\s+Based",         # footnote 8:  side effects / current method
    "^[0-9]+\\s+Among",         # footnote 22: haemoglobin/anaemia definition
    "^[0-9]+\\s+Random",        # footnote 23: blood glucose measurement note
    "^[0-9]+\\s+Since",         # footnote 14: rotavirus note
    "^[0-9]+\\s+Includes",      # footnote 9:  neonatal tetanus injections
    "^[0-9]+\\s+Vaccinated",    # footnotes 11/12: vaccination card definition
    "^[0-9]+\\s+Any\\s+method", # footnote 6:  any method definition
    "^[0-9]+\\s+Unmet\\s+need", # footnote 7:  unmet need definition
    "^[0-9]+\\s+Not\\s+including",          # footnote 13: polio at birth exclusion
    "^[0-9]+\\s+Haemoglobin",               # haemoglobin unit note
    "^[0-9]+\\s+Breastfed",                 # district: breastfeeding definition
    "^[0-9]+\\s+Locally",                   # district: hygienic protection definition
    "^[0-9]+\\s+Below",                     # district: stunting/wasting WHO standard
    "^[0-9]+\\s+Above",                     # district: overweight WHO standard
    "^[0-9]+\\s+Excludes",                  # district: BMI exclusion note
    "^[0-9]+\\s+Doctor",                    # district: skilled personnel definition
    "^[0-9]+\\s+Comprehensive",             # footnote 24: HIV knowledge definition
    "^[0-9]+\\s+Decisions",                 # footnote 25: household decisions definition
    "^[0-9]+\\s+Spousal",                   # footnote 27: spousal violence definition
    "^[0-9]+\\s+Women\\s+who\\s+are\\s+classified"  # infecund women footnote
  )
  if (any(str_detect(line, footnote_starts))) return(TRUE)
  
  # RULE 5: footnote continuation lines (bullets and empty bracket lines)
  if (str_detect(line, "^[\\·\\·•]")) return(TRUE)
  if (str_detect(line, "^\\(\\s*\\)")) return(TRUE)
  
  FALSE
}


# ── INDICATOR_REGEX ───────────────────────────────────────────────────────────
# The main regex applied to each line to split it into:
#   Group 1: indicator name
#   Group 2: all value tokens
#
# APPLIED TO str_trim(line) NOT str_squish(line).
# WHY: str_squish collapses ALL whitespace to single spaces, destroying the
# column gaps (2+ spaces) that separate names from numbers. str_trim only
# removes leading/trailing whitespace, preserving internal column gaps.
# This was a critical bug fix — without trim (not squish), lines with leading
# spaces like " 101. Blood sugar..." failed to match because "^" anchors to
# position 1 and a leading space pushed the digit to position 2.
#
# PATTERN BREAKDOWN:
#   ^                          start of line (after leading space trim)
#   (                          CAPTURE GROUP 1: indicator name
#     (?:\\d+\\.\\s+)?         optional leading number e.g. "50. "
#                              non-capturing group: \\d+ digits, \\. dot, \\s+ spaces
#     [A-Za-z]                 name MUST start with a letter — prevents matching
#                              pure-number footnote lines that slipped past is_skip_line
#     .+?                      any characters, LAZY — stops at the FIRST occurrence
#                              of 2+ spaces rather than the last
#   )                          end group 1
#   \\s{2,}                    TWO OR MORE consecutive spaces = column separator
#                              Single spaces appear within names: "age 15-49 years"
#                              2+ spaces only appear between table columns
#                              This is the key split point between name and values
#   (                          CAPTURE GROUP 2: all value tokens
#     (?:\\*|na|              first token is one of:
#       \\([\\d\\.]+\\)|       - parenthesised number e.g. (3.7) — district small sample
#       [\\d\\.,]+)            - asterisk (small sample → NA via to_num)
#                              - "na" (not available → NA via to_num)
#                              - plain number e.g. 88.6 or 1,090
#     (?:\\s+                  zero or more additional tokens (same four types)
#       (?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+)
#     )*
#   )                          end group 2
#   \\s*$                      optional trailing whitespace, end of line
#
# WHY EXPLICIT "na" IN THE PATTERN:
#   Without "na" in the pattern, lines like "90. Women who have high risk...  60.3  na"
#   would fail to match because "na" at the end doesn't fit [\\d\\.,]+
#   This caused ALL indicators where NFHS-4 = "na" to be silently dropped.
#   Adding (?:na|...) as an explicit alternative fixed this and recovered
#   ~54 additional indicators per state.
INDICATOR_REGEX <- paste0(
  # Group 1: indicator name — starts with optional number, then a letter
  # (or Unicode char like ≥ ≤ on continuation lines — handled via pending_name)
  "^((?:\\d+\\.\\s+)?[A-Za-z].+?)\\s{2,}",
  # Group 2: value tokens — *, na, (3.7), or plain numbers like 88.6 or 1,090
  "((?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+))*)\\s*$"
)

# Separate regex for continuation lines that start with spaces + any character
# (including ≥ ≤) followed by values — used when pending_name is set
CONTINUATION_REGEX <- paste0(
  "^\\s+(.+?)\\s{2,}",
  "((?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.]+\\)|[\\d\\.,]+))*)\\s*$"
)


# ── process_lines ─────────────────────────────────────────────────────────────
# The shared inner extraction loop — processes all lines from one PDF page.
# Called by both parse_state_pdf() and parse_district_pdf().
#
# WHY SHARED: state and district sheets differ only in number of value columns
# and value token types. Both differences are handled here transparently:
#   - "take last two tokens" works for both 4-column (state) and 2-column (district)
#   - to_num() handles *, na, (3.7) and plain numbers for both sheet types
#
# WRAPPED INDICATOR NAMES:
#   Some indicator names are too long to fit on one line in the PDF and wrap
#   onto a second line. The values appear on the continuation line. Example:
#
#   Line 1: "45. Registered pregnancies for which the mother received a"
#   Line 2: "    Mother and Child Protection (MCP) card (%)   85.0  89.5  79.9"
#
#   We handle this with pending_name:
#   Case A: Line starts with indicator number but has no values (no regex match)
#           → store as pending_name, wait for the continuation line
#   Case B: Line has no number and no values — pure name continuation
#           → append to pending_name
#           Guard: must not start with a digit, to prevent bare footnote
#           numbers (e.g. "1", "22") from being appended to a pending name
#   Case C: Line has values (regex matches)
#           → if pending_name exists, prepend it to raw_name before processing
#
# PARAMETERS:
#   lines        — character vector of raw lines from one page
#   geo_name     — "Bihar" for state, "Jaisalmer" for district
#   geo_level    — "State" or "District"
#   parent_state — "" for state rows, "Rajasthan" for district rows
#   round_label  — always "NFHS-5" (the second-to-last token value)
#   records      — accumulated list of tibbles from previous pages
#   pending_name — any partially-read indicator name carried from previous line
#
# RETURNS: list(records, pending_name)
#   pending_name is returned so it carries correctly across page boundaries
process_lines <- function(lines, geo_name, geo_level, parent_state,
                          round_label, records, pending_name) {
  for (line in lines) {
    line_sq <- str_squish(line)   # squished version for skip detection only
    if (is_skip_line(line_sq)) next
    
    # Apply regex to str_trim(line) — NOT squished, to preserve column gaps
    m <- str_match(str_trim(line), INDICATOR_REGEX)

    # If pending_name exists and the line starts with leading spaces (i.e. it is
    # a continuation line), always use CONTINUATION_REGEX instead of the main regex.
    # This prevents continuation lines like "Diastolic ≥90 mm of Hg) (%)" from
    # being treated as new standalone indicators — they must be joined to pending_name.
    if (!is.na(pending_name) && str_detect(line, "^\\s+")) {
      m2 <- str_match(line, CONTINUATION_REGEX)
      if (!is.na(m2[1, 1])) m <- m2
    } else if (is.na(m[1, 1]) && !is.na(pending_name)) {
      # Fallback: no match on trimmed line, try continuation regex anyway
      m2 <- str_match(line, CONTINUATION_REGEX)
      if (!is.na(m2[1, 1])) m <- m2
    }
    
    if (is.na(m[1, 1])) {
      # No match — check if this is part of a wrapped indicator name
      if (str_detect(line_sq, "^\\d+\\.\\s+[A-Za-z]")) {
        # Case A: starts with indicator number but no values → new pending name
        pending_name <- line_sq
      } else if (!is.na(pending_name) && nchar(line_sq) > 3 &&
                 !str_detect(line_sq, "^[0-9]") &&
                 !str_detect(line_sq, "^(Vaccinated|Piped|Flush|Electr|Refers|Equiv|Based|Among|Random|Since|Includes|Any method|Unmet need|Not including|Haemoglobin|Breastfed|Locally|Below|Above|Excludes|Doctor|Comprehensive|Decisions|Spousal|Women who are classified)")) {
        # Case B: continuation text (no number, no values) → append to pending
        # Guards:
        #   !str_detect(line_sq, "^[0-9]")  — prevents bare footnote numbers
        #   !str_detect(line_sq, "^(Vaccinated|...)")  — prevents footnote text
        #     lines (e.g. "Vaccinated based on vaccination card only") from being
        #     appended to a pending indicator name, which caused garbled names
        #     like "Children 12-23 (%)months fully vaccinated..."
        pending_name <- str_c(pending_name, " ", line_sq)
      }
      next
    }
    
    # Regex matched — extract name and values
    raw_name  <- str_squish(m[1, 2])   # squish name part now (safe after splitting)
    vals_part <- str_squish(m[1, 3])   # squish values part
    
    # Case C: if a pending name exists, this matched line is the continuation
    # that finally contains the values. Prepend the stored name.
    if (!is.na(pending_name)) {
      raw_name     <- str_c(pending_name, " ", raw_name)
      pending_name <- NA_character_   # consumed — reset for next indicator
    }
    
    # Split value tokens and take last two as NFHS-5 and NFHS-4 Total.
    # WHY LAST TWO ALWAYS WORKS:
    #   State sheets:    [Urban] [Rural] [NFHS-5 Total] [NFHS-4 Total] → last 2
    #   District sheets: [NFHS-5 Total] [NFHS-4 Total]                → last 2
    #   Single value:    [NFHS-5 Total]                                → only 1
    # Taking the last two handles all three cases correctly.
    val_tokens <- str_split(vals_part, "\\s+")[[1]]
    val_tokens <- val_tokens[nchar(val_tokens) > 0]
    if (length(val_tokens) < 1) next
    
    nfhs5_val <- if (length(val_tokens) >= 2) to_num(val_tokens[length(val_tokens) - 1]) else to_num(val_tokens[1])
    nfhs4_val <- if (length(val_tokens) >= 2) to_num(val_tokens[length(val_tokens)])     else NA_real_
    
    # Skip only if BOTH values are NA — a row with one valid value is still useful
    if (is.na(nfhs5_val) && is.na(nfhs4_val)) next
    
    indicator_name <- clean_name(raw_name)
    if (nchar(indicator_name) < 6) next   # discard artefacts (very short strings)
    
    meta <- lookup_meta(indicator_name)
    
    # Add one row per round — only if that round's value is not NA
    # This correctly handles indicators where NFHS-4 = "na" (new in NFHS-5)
    if (!is.na(nfhs5_val)) {
      records <- c(records, list(tibble(
        Indicator      = indicator_name,
        Geography      = geo_name,
        `Geo Level`    = geo_level,
        Round          = round_label,
        Value          = nfhs5_val,
        `Parent State` = parent_state,
        Domain         = meta$domain,
        Direction      = meta$direction
      )))
    }
    if (!is.na(nfhs4_val)) {
      records <- c(records, list(tibble(
        Indicator      = indicator_name,
        Geography      = geo_name,
        `Geo Level`    = geo_level,
        Round          = "NFHS-4",
        Value          = nfhs4_val,
        `Parent State` = parent_state,
        Domain         = meta$domain,
        Direction      = meta$direction
      )))
    }
  }
  list(records = records, pending_name = pending_name)
}


# =============================================================================
# STATE PARSER
#
# HEADER FORMAT: "Bihar - Key Indicators"
# VALUE COLUMNS: Urban | Rural | NFHS-5 Total | NFHS-4 Total (4 columns)
# Taking the last two always gives NFHS-5 Total and NFHS-4 Total.
#
# SKIP LOGIC:
#   Skips national/compendium files that may have been placed in the state folder
#   by checking if the extracted name matches known national patterns.
#   Also skips files with a comma in the header (district files accidentally
#   placed in the state folder) — district headers look like "Jaisalmer, Rajasthan".
# =============================================================================

extract_state_name <- function(lines) {
  # Every data page in a state fact sheet has a repeated header line like:
  # "Bihar - Key Indicators"
  # We match on "-\\s*Key Indicators" as the fixed suffix, then extract
  # everything before the first dash as the state name.
  header_line <- lines[str_detect(lines, "-\\s*Key Indicators")]
  if (length(header_line) == 0) return(NA_character_)
  str_squish(str_extract(header_line[1], "^[^-]+"))
}

parse_state_pdf <- function(pdf_path) {
  message("  [State] ", basename(pdf_path))
  
  pages <- tryCatch(pdf_text(pdf_path),
                    error = function(e) { message("  ❌ ", e$message); NULL })
  if (is.null(pages)) return(NULL)
  
  # Scan all pages for the state name header (it repeats on every data page)
  state_name <- NA_character_
  for (pg in pages) {
    state_name <- extract_state_name(str_split(pg, "\n")[[1]])
    if (!is.na(state_name)) break
  }
  
  # Fallback: extract state code from filename e.g. "OF43_BR.pdf" → "BR"
  # (Full state name is unknown without the header, so we use the code)
  if (is.na(state_name)) {
    state_name <- basename(pdf_path) |>
      str_remove_all("(?i)(OF43_|\\.pdf$)") |>
      str_squish()
    message("  ⚠️  Header not found, using filename code: ", state_name)
  }
  
  # Skip national/compendium PDFs that landed in the state folder
  # e.g. "India_National_Fact_Sheet.pdf" → state_name = "India..."
  # EXCEPTION: if the file is the India national factsheet, parse it with
  # Geography = "India" and Geo Level = "National" instead of skipping.
  if (str_detect(state_name, "(?i)^(india|nfhs|all|compendium)")) {
    if (str_detect(state_name, "(?i)^india")) {
      message("  \u2139\ufe0f  India factsheet detected — parsing as Geography='India', Geo Level='National'")
      geo_name_override  <- "India"
      geo_level_override <- "National"
    } else {
      message("  \u23ed\ufe0f  Skipping non-state file: ", state_name)
      return(NULL)
    }
  } else {
    geo_name_override  <- state_name
    geo_level_override <- "State"
  }
  
  # Skip district PDFs accidentally placed in the state folder
  # District headers contain a comma: "Jaisalmer, Rajasthan - Key Indicators"
  # → extracted "state_name" would be "Jaisalmer, Rajasthan"
  if (str_detect(state_name, ",")) {
    message("  ⏭  Skipping district file in state folder: ", state_name)
    return(NULL)
  }
  
  records      <- list()
  pending_name <- NA_character_
  # Skip page 1 (cover image) and last page (contact/IIPS details)
  data_pages   <- pages[seq(2, max(2, length(pages) - 1))]
  
  for (page_text in data_pages) {
    lines  <- str_split(page_text, "\n")[[1]]
    result <- process_lines(lines, geo_name_override, geo_level_override, "",
                            "NFHS-5", records, pending_name)
    records      <- result$records
    pending_name <- result$pending_name   # carry across page boundaries
  }
  
  if (length(records) == 0) { message("  ⚠️  No records."); return(NULL) }
  
  df <- bind_rows(records) |>
    distinct(Indicator, Geography, Round, .keep_all = TRUE)
  message("  ✅ ", nrow(df), " rows (", n_distinct(df$Indicator), " indicators)")
  df
}


# =============================================================================
# DISTRICT PARSER
#
# HEADER FORMAT: "Jaisalmer, Rajasthan - Key Indicators"
#   The comma separates district name from parent state — this is how we
#   distinguish district headers from state headers (no comma in state headers).
#
# VALUE COLUMNS: NFHS-5 Total | NFHS-4 Total (only 2 columns — no Urban/Rural)
#   "Taking last two tokens" still works correctly — with only 2 tokens,
#   last two = both tokens = NFHS-5 and NFHS-4.
#
# DISTRICT-SPECIFIC VALUE TYPES (handled by to_num):
#   (3.7)  → valid estimate with small sample size — strip brackets, use value
#   *      → fewer than 25 unweighted cases — not reportable, returns NA
#
# PARENT STATE:
#   Extracted from the header and stored in the "Parent State" column.
#   This enables the dashboard's hierarchical drill-down (pick state → see districts).
#   Falls back to the state code from the filename e.g. "RJ" from "RJ_Jaisalmer.pdf"
# =============================================================================

extract_district_and_state <- function(lines) {
  header_line <- lines[str_detect(lines, "-\\s*Key Indicators")]
  if (length(header_line) == 0) return(NULL)
  geo_part <- str_squish(str_extract(header_line[1], "^[^-]+"))
  # A comma in the geo part = district sheet ("Jaisalmer, Rajasthan")
  # No comma = state sheet — return NULL so caller knows this isn't a district
  if (is.na(geo_part) || !str_detect(geo_part, ",")) return(NULL)
  parts <- str_split(geo_part, ",\\s*")[[1]]
  list(district = str_squish(parts[1]), state = str_squish(parts[2]))
}

parse_district_pdf <- function(pdf_path) {
  message("  [District] ", basename(pdf_path))
  
  pages <- tryCatch(pdf_text(pdf_path),
                    error = function(e) { message("  ❌ ", e$message); NULL })
  if (is.null(pages)) return(NULL)
  
  geo <- NULL
  for (pg in pages) {
    geo <- extract_district_and_state(str_split(pg, "\n")[[1]])
    if (!is.null(geo)) break
  }
  
  # Fallback: parse from filename e.g. "RJ_Jaisalmer.pdf"
  # Split on first underscore: state code = "RJ", district = "Jaisalmer"
  # Multi-word districts: "RJ_Sawai_Madhopur.pdf" → "Sawai Madhopur"
  if (is.null(geo)) {
    fname  <- basename(pdf_path) |> str_remove_all("(?i)\\.pdf$")
    parts  <- str_split(fname, "_", n = 2)[[1]]
    geo    <- list(
      state    = if (length(parts) >= 1) str_squish(parts[1]) else "Unknown",
      district = if (length(parts) >= 2) str_replace_all(str_squish(parts[2]), "_", " ") |>
        str_to_title() else "Unknown"
    )
    message("  ⚠️  Header not found, using filename: ", geo$district, " (", geo$state, ")")
  }
  
  records      <- list()
  pending_name <- NA_character_
  data_pages   <- pages[seq(2, max(2, length(pages) - 1))]
  
  for (page_text in data_pages) {
    lines  <- str_split(page_text, "\n")[[1]]
    result <- process_lines(lines, geo$district, "District", geo$state,
                            "NFHS-5", records, pending_name)
    records      <- result$records
    pending_name <- result$pending_name
  }
  
  if (length(records) == 0) { message("  ⚠️  No records."); return(NULL) }
  
  df <- bind_rows(records) |>
    distinct(Indicator, Geography, Round, .keep_all = TRUE)
  message("  ✅ ", nrow(df), " rows (", n_distinct(df$Indicator), " indicators)")
  df
}


# =============================================================================
# RUN — extract states, extract districts, append, write
# =============================================================================

# ── STATES ────────────────────────────────────────────────────────────────────
# list.files with pattern = "\\.pdf$" picks up ALL PDFs in the folder.
# No filename filter needed — parse_state_pdf() handles skipping non-state files.
state_files <- list.files(STATE_FOLDER, pattern = "\\.pdf$",
                          full.names = TRUE, ignore.case = TRUE)
message("\n📂 Found ", length(state_files), " state PDF(s)\n")

state_results <- map(state_files, parse_state_pdf) |> compact()
state_df      <- if (length(state_results) > 0) bind_rows(state_results) else tibble()
message("\n✅ States: ", nrow(state_df), " rows extracted\n")

# ── DISTRICTS ─────────────────────────────────────────────────────────────────
district_files <- list.files(DISTRICT_FOLDER, pattern = "\\.pdf$",
                             full.names = TRUE, ignore.case = TRUE)
message("\n📂 Found ", length(district_files), " district PDF(s)\n")

district_results <- map(district_files, parse_district_pdf) |> compact()
district_df      <- if (length(district_results) > 0) bind_rows(district_results) else tibble()
message("\n✅ Districts: ", nrow(district_df), " rows extracted\n")

# ── APPEND AND WRITE ──────────────────────────────────────────────────────────
# bind_rows stacks the two tibbles vertically.
# Column structure is identical so no alignment issues.
# write_csv outputs NA as blank cells — correct for Google Sheets.
master_df <- bind_rows(state_df, district_df)

# Flag any indicators where no keyword matched — add them to indicator_meta above
unknowns <- master_df |> filter(Domain == "Unknown") |>
  distinct(Indicator) |> pull(Indicator)
if (length(unknowns) > 0) {
  message("⚠️  ", length(unknowns), " indicator(s) with unknown domain:")
  walk(unknowns, ~ message('  "', str_to_lower(str_sub(.x, 1, 55)), '",'))
}

write_csv(master_df, OUTPUT_FILE)

message("\n✅ Done!")
message("   Total rows   : ", nrow(master_df))
message("   State rows   : ", nrow(state_df))
message("   District rows: ", nrow(district_df))
message("   Geo levels   : ", str_c(unique(master_df$`Geo Level`), collapse = ", "))
message("   States       : ", n_distinct(master_df$Geography[master_df$`Geo Level` == "State"]))
message("   Districts    : ", n_distinct(master_df$Geography[master_df$`Geo Level` == "District"]))
message("   Indicators   : ", n_distinct(master_df$Indicator))
message("   Output       : ", OUTPUT_FILE)

# =============================================================================
# DIAGNOSTIC — run this after the main script to see exactly which indicator
# lines are being matched vs missed by the regex.
#
# HOW TO USE:
#   1. Run the main script above first (so all functions are loaded)
#   2. Change DIAG_PDF to any PDF you want to inspect
#   3. Run this block — it prints:
#      MATCHED   : lines the regex successfully extracted values from
#      NO MATCH  : numbered indicator lines the regex failed on (your missing indicators)
#      RAW LINES : the 2 lines around each no-match, showing the exact raw text
#                  so you can see whether it is a wrapping issue, leading space,
#                  special character, or something else
# =============================================================================

DIAG_PDF <- "C:/Users/Cegis/Downloads/NFHS all factsheets/state/OF43_BR.pdf"

pages      <- pdf_text(DIAG_PDF)
data_pages <- pages[seq(2, max(2, length(pages) - 1))]

results <- list()
for (page_text in data_pages) {
  lines <- str_split(page_text, "\n")[[1]]
  for (line in lines) {
    line_sq <- str_squish(line)
    if (nchar(line_sq) == 0) next
    
    # Only inspect lines that start with an indicator number —
    # these are the only ones we care about capturing
    if (!str_detect(line_sq, "^\\d+\\.\\s+[A-Za-z]")) next
    
    # Use INDICATOR_REGEX (the current fixed version defined in the main script)
    # NOT the old regex — the old version had a bug where [\\d\\.,na-]+
    # treated "na-" as a character range, silently failing on na values
    m      <- str_match(str_trim(line), INDICATOR_REGEX)
    status <- if (!is.na(m[1, 1])) "✅ MATCH" else "❌ NO MATCH"
    
    results <- c(results, list(data.frame(status = status, line = line_sq)))
  }
}

df <- bind_rows(results)
cat("Total indicator lines found :", nrow(df), "\n")
cat("Matched                     :", sum(df$status == "✅ MATCH"), "\n")
cat("Not matched                 :", sum(df$status == "❌ NO MATCH"), "\n\n")

cat("=== NOT MATCHED (these are your missing indicators) ===\n")
df |> filter(status == "❌ NO MATCH") |> pull(line) |> walk(~ cat(.x, "\n"))

# For each no-match, show the raw lines around it so you can see exactly
# what the PDF text looks like — particularly useful for diagnosing:
#   - leading spaces before indicator numbers e.g. " 101. Blood sugar..."
#   - wrapped names where values appear on the next line
#   - special characters like ≥ or superscripts in the middle of names
cat("\n=== RAW LINES AROUND EACH NO-MATCH ===\n")
no_match_numbers <- df |>
  filter(status == "❌ NO MATCH") |>
  pull(line) |>
  str_extract("^\\d+")

for (page_text in data_pages) {
  lines <- str_split(page_text, "\n")[[1]]
  for (i in seq_along(lines)) {
    sq <- str_squish(lines[i])
    num <- str_extract(sq, "^\\d+")
    if (!is.na(num) && num %in% no_match_numbers) {
      cat(sprintf("\n[%d] RAW: '%s'\n", i,   lines[i]))
      cat(sprintf("[%d] RAW: '%s'\n",   i+1, if (i+1 <= length(lines)) lines[i+1] else ""))
      cat(sprintf("[%d] RAW: '%s'\n",   i+2, if (i+2 <= length(lines)) lines[i+2] else ""))
    }
  }
}