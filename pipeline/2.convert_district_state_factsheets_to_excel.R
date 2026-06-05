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

STATE_FOLDER    <- "C:/Users/Cegis/Downloads/NFHS all factsheets/state"
DISTRICT_FOLDER <- "C:/Users/Cegis/Downloads/NFHS all factsheets/districts"
OUTPUT_FILE     <- "C:/Users/Cegis/Downloads/NFHS all factsheets/nfhs_all_data.csv"


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
# Runs 12 sequential normalisation steps, each targeting a distinct noise type.
#
# THE PROBLEM THIS SOLVES:
#   pdftools extracts NFHS table text with various kinds of embedded noise.
#   Without cleaning, the same real-world indicator gets stored under many
#   different strings (e.g. "recall11 (%)" vs "recall14 (%)" vs "recall (%)"),
#   causing the dashboard dropdown to explode and cross-round joins to fail.
#   Each step below was added after diagnosing a specific failure mode.
#
# STEPS 1-2 — Structural noise present on every line
#   Step 1: Leading indicator number     "50. Institutional births" → name only
#   Step 2: Unicode superscript markers  "source¹" → "source"
#           (¹²³ are Unicode U+00B9 etc., distinct from ASCII digits — need own step)
#
# STEPS 3-8 — Footnote reference digits (4 distinct attachment styles)
#   NFHS PDFs attach small reference numbers to indicator names that link to
#   page-bottom footnotes. The number differs between rounds for the same indicator
#   (e.g. footnote 11 in NFHS-5 vs footnote 14 in NFHS-6), so they MUST be stripped.
#   Four styles exist; a single regex cannot handle all without collateral damage:
#
#   Step 3: Glued to letter, optional comma-list before ( or space
#           "recall11 (%)" → "recall (%)"    "diet10, 11 (%)" → "diet (%)"
#           Lookahead (?=\\s|\\() prevents stripping mid-name numbers like "12-23".
#   Step 4: After (%) at end of string     "... (%)11" → "... (%)"
#   Step 5: Trailing after a letter        "...personnel10" → "...personnel"
#           NOT after a digit — protects "15-49", "12-23", "BMI <18.5"
#   Step 6: Between ) and ( with optional comma-list
#           "age)18 (%)" → "age) (%)"    "age)10, 11 (%)" → "age) (%)"
#   Step 7: Spaced comma-list before (    "diet 10, 11 (%)" → "diet (%)"
#           The + quantifier (≥2 numbers) lets lone spaced digits fall to Step 8.
#   Step 8: Lone spaced digit before (    "birth 15 (%)" → "birth (%)"
#           Negative lookbehind (?<!age) preserves "married before age 18 (%)".
#
# STEPS 9-12 — Systematic PDF extraction artefacts (added after diagnostic runs)
#   These were discovered by running the low-coverage diagnostic, grouping the
#   30 problem indicators by root cause, and tracing each back to the extraction.
#
#   Step 9: Concatenated indicator numbers
#           e.g. "...vaccination card only (%) 46. Children age 12-23 months..."
#           Cause: PDF line compression places two indicators on one extracted line,
#           OR the pending_name mechanism joins a truncated name with the next one.
#           Pattern \\s+\\d{1,3}\\.\\s+[A-Z].* is uniquely the indicator-number
#           format and never appears inside any legitimate NFHS indicator name.
#
#   Step 10: Trailing absorbed value tokens
#           e.g. "...facility (Rs.) (2,965)" → "...facility (Rs.)"
#           e.g. "...per 1,000 males) (1,111)" → "...per 1,000 males)"
#           Cause: 1-space column gap causes lazy regex to absorb the first value
#           into the name. The salvage in process_lines() handles the primary case;
#           Step 10 is a final backstop after Case C prepending.
#           Safe: unit markers (%), (Rs.), (per 1,000...) contain non-digit chars so
#           \\([\\d\\.,]+\\) never matches them.
#
#   Step 11: BMI unit variant normalisation → canonical "kg/m2) (%)"
#           Cause: Step 3 strips "2" from "kg/m2" only when a space follows it, not
#           when a closing paren does — producing 3 variants across state PDFs:
#           "kg/m (%)" / "kg/m2 (%)" / "kg/m 2) (%)". All → "kg/m2) (%)".
#
#   Step 12: Disclaimer text absorbed after the unit marker (%)
#           e.g. "...blood pressure (%) Practices (fed with other milk...)" (7 states)
#           Cause: page-bottom footnote/disclaimer text was absorbed via the
#           pending_name cascade, producing a 100+ word garbage name string.
#           Fix: strip everything after the first (%).
#           Safe: every valid NFHS indicator name ends at its (%) unit marker.
clean_name <- function(x) {
  x |>
    str_trim() |>
    # Step 1: remove leading indicator number e.g. "50. "
    str_remove("^\\d+\\.\\s*") |>
    # Step 2: remove Unicode superscript footnote markers e.g. \u00b9\u00b2\u00b3
    str_remove_all("[\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u2070]+") |>
    # Step 3: remove inline footnote digits glued to a letter, before space or (
    # Extended to handle comma-separated lists: "recall10, 11 (%)" -> "recall (%)"
    # e.g. "only12 (%)" -> "only (%)", "sugar level23 (%)" -> "sugar level (%)"
    str_replace_all("(?<=[a-zA-Z])(\\d{1,2})(?:\\s*,\\s*\\d{1,2})*(?=\\s|\\()", "") |>
    # Step 4: remove trailing footnote number after (%) e.g. "... (%)11" -> "... (%)"
    str_remove("(?<=\\(%\\))\\s*\\d{1,2}\\s*$") |>
    # Step 5: remove trailing footnote after plain letter/word at end of string
    # NOT after digit — protects "12-23", "15-49", "BMI <18.5" etc.
    str_remove("(?<=[a-zA-Z])\\s*\\d{1,2}\\s*$") |>
    # Step 6: remove footnote(s) between ) and ( e.g. "age)18 (%)" or "age)10, 11 (%)"
    str_remove_all("\\)\\d{1,2}(?:\\s*,\\s*\\d{1,2})*(?=\\s*\\()") |>
    # Step 7: remove spaced comma-list footnote before ( e.g. "diet 10, 11 (%)" -> "diet (%)"
    # The + requires at least one comma group, so lone spaced numbers are handled by step 8
    str_remove_all("\\s\\d{1,2}(?:\\s*,\\s*\\d{1,2})+(?=\\s*\\()") |>
    # Step 8: remove lone spaced footnote before ( e.g. "birth 15 (%)" -> "birth (%)"
    # (?<!age) preserves meaningful age thresholds: "age 18 (%)" is left untouched
    str_remove_all("(?<!age)\\s\\d{1,2}(?=\\s*\\()") |>
    # Step 9: strip concatenated indicator numbers absorbed from PDF line compression
    # e.g. "...vaccine (MCV) (%) 64. Children age 12-23 months..." -> "...vaccine (MCV) (%)"
    # Matches " N. UppercaseLetter..." which is uniquely the indicator number format
    str_remove("\\s+\\d{1,3}\\.\\s+[A-Z].*") |>
    # Step 10: strip trailing absorbed value tokens (parenthesized or plain numbers)
    # e.g. "...facility (Rs.) (2,965)" -> "...facility (Rs.)"
    # e.g. "...per 1,000 males) (1,111)" -> "...per 1,000 males)"
    # Safe: unit markers like (%), (Rs.), (per 1,000...) contain non-digit characters
    # so they are never matched by \\([\\d\\.,]+\\)
    str_remove("\\s+(?:\\([\\d\\.,]+\\)|[\\d\\.,]+)(?:\\s+(?:\\([\\d\\.,]+\\)|[\\d\\.,]+))*\\s*$") |>
    # Step 11: normalise BMI unit variants to canonical "kg/m2) (%)"
    # Step 3 inconsistently strips the "2" from "kg/m2" depending on whether a
    # space or closing paren follows — producing "kg/m (%)", "kg/m2 (%)",
    # "kg/m 2) (%)" across different state PDFs for the same indicator.
    str_replace_all("kg/m\\s*2?\\s*\\)\\s*\\(%\\)", "kg/m2) (%)") |>
    str_replace_all("kg/m\\s*2?\\s*\\(%\\)",        "kg/m2) (%)") |>
    # Step 12: strip footnote/disclaimer text absorbed after the unit marker (%)
    # e.g. "...blood pressure (%) Practices (fed with other milk...)" -> "...blood pressure (%)"
    # Safe: (%) always ends a complete indicator name; anything after is absorption artefact
    str_replace("(\\(%\\)).*$", "\\1") |>
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


# ── merge_wrapped_lines (INDIA NATIONAL PDF ONLY) ─────────────────────────────
# Reassembles indicator rows that the India national factsheet splits across
# THREE physical lines, recovering ~28 long-named indicators that were otherwise
# lost (maternity care, child vaccination, child illness, NCDs, gender/violence).
#
# THE ACTUAL PDF STRUCTURE (verified against the real India_National_Fact_Sheet.pdf):
#   When a long indicator name wraps in the left column of the table, pdftools
#   emits THREE separate lines, with the value columns on their OWN line in the
#   middle (they are vertically positioned between the two name fragments):
#       line i   : "45. Registered pregnancies...Mother and Child Protection (MCP)"  <- name part 1
#       line i+1 : "                       94.9 96.3 95.9 89.3"                       <- VALUES (own line)
#       line i+2 : "card (%)"                                                         <- name part 2
#   The streaming pending_name / CONTINUATION_REGEX machinery cannot handle this
#   (CONTINUATION_REGEX expects "<name> <2+ spaces> <values>" on one line; here
#   the values sit on a line by themselves between two name fragments). So these
#   indicators were dropped for India only — state PDFs do not split this way.
#
#   NOTE: single-line indicators (e.g. "50. Institutional births (%)  93.8 ...")
#   are unaffected — they already carry a 2-space gap and are parsed normally.
#   This is why India previously extracted ~103 (all the single-line indicators)
#   and was missing exactly the ~28 wrapped ones.
#
# THE FIX — a gather pass run ONLY on the India PDF (see parse_state_pdf):
#   When a line starts an indicator ("<number>. <Letter>") but does NOT already
#   end with a value token, treat it as wrapped and gather the following lines:
#     - a VALUES-ONLY line (matches values_re) becomes the value columns
#     - a non-numeric text line becomes the name continuation
#   Stop as soon as we have BOTH the values and a name continuation that ends the
#   name with ")", OR when we hit the next indicator / a skip line / a blank.
#   Emit one reassembled line: "<name1> <name2>  <values>"  (2-space join so
#   INDICATOR_REGEX still sees a proper gap before the values).
#
# WHY THE STOP CONDITIONS ARE SAFE:
#   - Stopping at the next indicator number prevents running into the next row.
#   - Stopping at is_skip_line() prevents absorbing section headers ("Delivery
#     Care", "Women", "Men") or page-bottom footnotes into the indicator.
#   - Requiring the name to end with ")" before stopping means a mid-name
#     parenthetical like "(MCP)" on line 1 does not trigger an early stop — the
#     gather correctly waits for the real unit marker "(%)" on the continuation.
#   - A values line is captured only ONCE (vals_acc == "" guard), and footnote
#     lines such as "9", "10Doctor...", "15Based..." start with a digit and are
#     never treated as name continuations.
#
# ISOLATION: invoked only on the India branch of parse_state_pdf(). State and
# district parsing never call it, so their behaviour is completely unchanged.
merge_wrapped_lines <- function(lines) {
  out <- character(0)
  # A "values-only" line consists entirely of value tokens (*, na, (3.7), 94.9, 1,090)
  values_re <- "^(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*$"
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    trimmed          <- str_trim(lines[i])
    starts_indicator <- str_detect(trimmed, "^\\d+\\.\\s+[A-Za-z]")
    has_values       <- str_detect(trimmed, "(\\d|na|\\*)\\s*$")

    if (starts_indicator && !has_values) {
      # Wrapped indicator — gather the value line and name continuation that follow.
      name_acc <- trimmed
      vals_acc <- ""
      n_cont   <- 0L
      j <- i + 1L
      while (j <= n) {
        cand <- str_trim(lines[j])
        if (str_detect(cand, "^\\d+\\.\\s+[A-Za-z]")) break          # next indicator
        if (nchar(cand) == 0 || is_skip_line(str_squish(cand))) break # blank / header / footnote
        if (str_detect(cand, values_re) && vals_acc == "") {
          vals_acc <- cand                                           # the value columns line
        } else if (!str_detect(cand, "^\\d")) {
          name_acc <- str_c(name_acc, " ", cand)                     # name continuation fragment
          n_cont   <- n_cont + 1L
        } else {
          break                                                      # unexpected numeric line
        }
        j <- j + 1L
        # Stop once the name is complete (ends with ")") and values are captured
        if (vals_acc != "" && n_cont >= 1L && str_detect(name_acc, "\\)\\s*$")) break
      }
      if (vals_acc != "") {
        out <- c(out, str_c(name_acc, "  ", vals_acc))               # reassembled row
      } else {
        out <- c(out, lines[i])                                      # no values found — leave as-is
      }
      i <- j
    } else {
      out <- c(out, lines[i])
      i <- i + 1L
    }
  }
  out
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
  # Group 2: value tokens — *, na, (3.7), (2,965), or plain numbers like 88.6 or 1,090
  # CHANGED: \\([\\d\\.,]+\\) was \\([\\d\\.]+\\) — comma added to the character class.
  # WHY: district factsheets parenthesise large numbers for small-sample cells:
  #   e.g. "(2,965)" for out-of-pocket Rs. values, "(1,111)" for sex ratios.
  # Without the comma, these tokens failed to match the value pattern, so the
  # entire line failed INDICATOR_REGEX, fell to Case A, and the next indicator's
  # line was prepended via Case C — producing 34-word concatenated garbage names.
  # The same comma fix was applied to CONTINUATION_REGEX and salvage_pat.
  "((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
)

# Separate regex for continuation lines that start with spaces + any character
# (including ≥ ≤) followed by values — used when pending_name is set
CONTINUATION_REGEX <- paste0(
  "^\\s+(.+?)\\s{2,}",
  "((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)",
  "(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
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
    
    # Value salvage: some long indicator names in state/district PDFs leave only
    # 1 space before the Urban value column (instead of the 2+ spaces the lazy
    # regex needs to split on). This causes the first value to be absorbed into
    # the indicator name, creating a unique garbage string per geography:
    #   e.g. "...blood sugar level (%) 12.9"  (Karnataka has 12.9% Urban)
    # Without salvage, every state produces a different indicator name string for
    # the same indicator — ~36 unique variants instead of 1.
    # Salvage detects and strips the trailing value token(s) from raw_name,
    # prepends them to vals_part so all columns are correctly recovered, and
    # "last two tokens" still gives the correct NFHS-5 and NFHS-4 totals.
    # e.g. raw_name="...blood sugar level23 (%) 12.9"  vals_part="10.4 10.8 na"
    #   -> raw_name="...blood sugar level23 (%)         vals_part="12.9 10.4 10.8 na"
    # CHANGED: salvage_pat now uses \\([\\d\\.,]+\\) (comma added) to also catch
    # parenthesised comma-numbers like (2,965) that the original \\([\\d\\.]+\\) missed.
    salvage_pat <- "\\s+((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
    salvaged    <- str_match(raw_name, salvage_pat)
    if (!is.na(salvaged[1, 1])) {
      raw_name  <- str_remove(raw_name, salvage_pat)
      vals_part <- str_squish(str_c(salvaged[1, 2], " ", vals_part))
    }

    # Truncation check: if the name still doesn't end with ")" it is cut off
    # mid-sentence — the PDF wrapped the indicator name across two lines and
    # the lazy regex stopped at the 2-space column gap before the values,
    # leaving the unit marker "(%" on the continuation line.
    # Store as pending_name and let the continuation complete the name.
    # SAFE for script 2: each PDF is processed independently, so pending_name
    # never bleeds across states. The continuation ALWAYS follows immediately
    # in NFHS state/district PDFs (no intervening indicators between the
    # truncated first line and its continuation).
    if (!str_detect(raw_name, "\\)\\s*$") && nchar(raw_name) > 20) {
      pending_name <- raw_name
      next
    }

    # DUAL-INDICATOR SPLIT ────────────────────────────────────────────────────
    # When raw_name contains an embedded second indicator (pattern: " N. Capital..."),
    # the concatenation arose from one of two situations:
    #
    #   A. India national PDF (or other compressed PDFs): two consecutive indicators
    #      appear on one physical line with only 1 space between them:
    #        "49. postnatal care (%) 50. Institutional births (%)  92.3  88.9"
    #      INDICATOR_REGEX stops at the 2-space gap AFTER indicator 50's name.
    #      vals_part = indicator 50's values. Indicator 49 has NO values on this line.
    #
    #   B. Truncation-check cascade in state/district PDFs: indicator N's name was
    #      truncated (didn't end with ")"), pending_name was set, then Case B appended
    #      the continuation line (including indicator N's values) to pending_name,
    #      and Case C prepended the whole thing to indicator N+1's name:
    #        raw_name = "24. Adolescent fertility rate...(per 1,000 women) 30.1 29.6 25. NNMR"
    #      Here indicator 24's values (30.1 29.6) ARE embedded in raw_name.
    #
    # FIX: always store the second indicator (vals_part = its values).
    # For case B: ALSO recover the first indicator's values from raw_name
    # by applying salvage logic to the part before the embedded second indicator.
    # For case A: first_raw has no trailing value tokens — first indicator is skipped.
    dual_match <- str_match(raw_name, "\\s+(\\d{1,3}\\.\\s+[A-Za-z].+)$")
    if (!is.na(dual_match[1, 1])) {
      # ── Attempt to recover first indicator ───────────────────────────────────
      first_raw <- str_remove(raw_name, "\\s+\\d{1,3}\\.\\s+[A-Za-z].*$")
      first_salvaged <- str_match(first_raw, salvage_pat)
      if (!is.na(first_salvaged[1, 1])) {
        first_name   <- clean_name(str_remove(first_raw, salvage_pat))
        first_tokens <- str_split(str_squish(first_salvaged[1, 2]), "\\s+")[[1]]
        first_tokens <- first_tokens[nchar(first_tokens) > 0]
        if (nchar(first_name) >= 6 && length(first_tokens) >= 1) {
          f5 <- if (length(first_tokens) >= 2) to_num(first_tokens[length(first_tokens) - 1]) else to_num(first_tokens[1])
          f4 <- if (length(first_tokens) >= 2) to_num(first_tokens[length(first_tokens)])     else NA_real_
          f_meta <- lookup_meta(first_name)
          if (!is.na(f5))
            records <- c(records, list(tibble(
              Indicator = first_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = round_label, Value = f5, `Parent State` = parent_state,
              Domain = f_meta$domain, Direction = f_meta$direction)))
          if (!is.na(f4))
            records <- c(records, list(tibble(
              Indicator = first_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = "NFHS-4", Value = f4, `Parent State` = parent_state,
              Domain = f_meta$domain, Direction = f_meta$direction)))
        }
      }
      # ── Store second indicator ────────────────────────────────────────────────
      second_name <- clean_name(dual_match[1, 2])
      if (nchar(second_name) >= 6) {
        s_tokens <- str_split(str_squish(vals_part), "\\s+")[[1]]
        s_tokens <- s_tokens[nchar(s_tokens) > 0]
        if (length(s_tokens) >= 1) {
          s5 <- if (length(s_tokens) >= 2) to_num(s_tokens[length(s_tokens) - 1]) else to_num(s_tokens[1])
          s4 <- if (length(s_tokens) >= 2) to_num(s_tokens[length(s_tokens)])     else NA_real_
          s_meta <- lookup_meta(second_name)
          if (!is.na(s5))
            records <- c(records, list(tibble(
              Indicator = second_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = round_label, Value = s5, `Parent State` = parent_state,
              Domain = s_meta$domain, Direction = s_meta$direction)))
          if (!is.na(s4))
            records <- c(records, list(tibble(
              Indicator = second_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = "NFHS-4", Value = s4, `Parent State` = parent_state,
              Domain = s_meta$domain, Direction = s_meta$direction)))
        }
      }
      pending_name <- NA_character_
      next  # both indicators handled above; skip normal processing for this line
    }
    # ────────────────────────────────────────────────────────────────────────────

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

# ── parse_state_pdf ──────────────────────────────────────────────────────────
# Reads one state PDF and returns a tibble of extracted rows, or NULL on error.
#
# WORKFLOW:
#   1. pdf_text() extracts raw text — one character string per page.
#   2. Scan pages for the "X - Key Indicators" header to get the state name.
#   3. Detect and handle two edge cases:
#      (a) India national factsheet: parse with Geography="India", Level="National"
#          rather than skipping — the India row is useful for national benchmarks.
#      (b) District PDF in wrong folder: header contains a comma ("Jaisalmer, Rajasthan")
#          → skip with a warning.
#   4. Process all pages except page 1 (cover image) with process_lines().
#      pending_name is threaded across pages so wrapped indicators that straddle a
#      page break are correctly joined.
#   5. bind_rows() + distinct() deduplicates rows by (Indicator, Geography, Round).
#      Duplicates arise when a page header repeats an indicator number from the
#      previous page (NFHS fact sheets sometimes reprint the first row of a table).
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
  # CHANGED: previously pages[seq(2, length(pages)-1)] which skipped BOTH
  # page 1 (cover image) AND the last page (assumed to be IIPS contact sheet).
  # That caused Gender & Violence, HIV/AIDS and Lifestyle indicators to be
  # silently dropped — those sections fall on the last data page of many states.
  # Now only page 1 is skipped. The last page is safe because is_skip_line()
  # already catches "International Institute" and "Ministry of Health" — the
  # IIPS contact text is filtered out line by line, not by page exclusion.
  data_pages   <- pages[seq(2, length(pages))]
  
  for (page_text in data_pages) {
    lines  <- str_split(page_text, "\n")[[1]]
    # INDIA ONLY: reassemble wrapped indicator names whose continuation lines are
    # not indented (see merge_wrapped_lines). State/district PDFs indent their
    # continuations and are handled by CONTINUATION_REGEX, so they skip this.
    if (identical(geo_name_override, "India")) {
      lines <- merge_wrapped_lines(lines)
    }
    result <- process_lines(lines, geo_name_override, geo_level_override, "",
                            "NFHS-5", records, pending_name)
    records      <- result$records
    pending_name <- result$pending_name   # carry across page boundaries
  }
  
  if (length(records) == 0) { message("  ⚠️  No records."); return(NULL) }
  
  # distinct() removes duplicate (Indicator, Geography, Round) rows.
  # Duplicates arise when NFHS fact sheets reprint the first data row of a table
  # at the top of the next page — the same indicator then appears twice for the
  # same state and round. keep_all=TRUE retains all other columns from the first
  # occurrence.
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

# ── parse_district_pdf ───────────────────────────────────────────────────────
# Reads one district PDF and returns a tibble of extracted rows, or NULL on error.
#
# KEY DIFFERENCES FROM parse_state_pdf:
#   1. Header format: "Jaisalmer, Rajasthan - Key Indicators"
#      The comma is used to distinguish district headers from state headers and
#      to split out the district name and parent state name.
#   2. Value columns: only 2 (NFHS-5 Total | NFHS-4 Total) — no Urban/Rural.
#      "Last two tokens" still works: with 2 tokens, last-1 = NFHS-5, last = NFHS-4.
#   3. to_num() handles two district-specific value types:
#      (3.7) → valid small-sample estimate (strip brackets, use number)
#      *     → <25 unweighted cases, suppress → NA
#   4. Parent State is populated from the header and stored in the output column.
#      This powers the dashboard's state → district drill-down filter.
#
# FILENAME FALLBACK (when header is missing):
#   "RJ_Jaisalmer.pdf"        → state="RJ",  district="Jaisalmer"
#   "RJ_Sawai_Madhopur.pdf"   → state="RJ",  district="Sawai Madhopur"
#   Underscores after the first are replaced with spaces and title-cased.
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
  # Same last-page fix as parse_state_pdf: only skip page 1.
  # Previously pages[seq(2, length(pages)-1)] dropped the last page and lost
  # GBV / Lifestyle indicators for districts where those fall on the final page.
  data_pages   <- pages[seq(2, length(pages))]
  
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

# =============================================================================
# FRAGMENT FILTER
# Remove extraction artefacts: indicator name variants and continuation
# fragments that appear in very few geographies AND look incomplete.
# Two conditions must BOTH be true to drop a row:
#   1. The indicator appears in fewer than 5 geographies (state + district
#      combined). Real indicators appear in most or all of the ~36 states.
#   2. The indicator name looks like a fragment — it either:
#        (a) does not end with a closing parenthesis ")" — meaning it is
#            missing its unit marker like (%), (Rs.), (per 1,000...) etc., OR
#        (b) has fewer than 5 words — obviously too short to be a real
#            indicator name.
# This deliberately KEEPS low-coverage indicators that look complete
# (e.g. a legitimate indicator missing from a few small UTs).
# =============================================================================
geo_counts <- master_df |>
  count(Indicator, name = "n_geos")

fragments <- geo_counts |>
  filter(
    n_geos < 5,
    !str_detect(Indicator, "\\)\\s*$") |   # missing unit marker
    str_count(Indicator, "\\S+") < 5        # fewer than 5 words
  ) |>
  pull(Indicator)

if (length(fragments) > 0) {
  message("\n🧹 Removing ", length(fragments),
          " extraction fragment(s) (< 5 geos AND incomplete name):")
  walk(fragments, ~ message('   "', str_sub(.x, 1, 70), '"'))
  master_df <- master_df |> filter(!Indicator %in% fragments)
}

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
data_pages <- pages[seq(2, length(pages))]  # consistent with main script

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


# =============================================================================
# LOW-COVERAGE DIAGNOSTIC
# Indicators appearing in fewer than 10 geographies are likely name variants
# (same indicator extracted under slightly different strings across PDFs) or
# continuation fragments (second half of a wrapped name stored as standalone).
# This block exports both a summary and a detail CSV showing which
# states/districts each low-coverage indicator actually appears in.
# Run this after the main extraction to guide clean_name() fixes.
# =============================================================================

diag_threshold <- 10

low_coverage_summary <- master_df |>
  count(Indicator, sort = TRUE) |>
  filter(n < diag_threshold)

low_coverage_detail <- master_df |>
  filter(Indicator %in% low_coverage_summary$Indicator) |>
  select(Indicator, Geography, `Geo Level`, Round, Value, Domain) |>
  arrange(Indicator, `Geo Level`, Geography, Round)

message("\n⚠️  ", nrow(low_coverage_summary),
        " indicator(s) appearing in fewer than ", diag_threshold, " geographies:")
print(low_coverage_summary, n = 60)

diag_path <- str_replace(OUTPUT_FILE, "\\.csv$", "_low_coverage_diag.csv")
write_csv(low_coverage_detail, diag_path)
message("\n📋 Low-coverage detail (with geography names) written to:\n   ", diag_path)