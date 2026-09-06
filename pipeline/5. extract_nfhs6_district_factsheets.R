# =============================================================================
# NFHS-6 DISTRICT EXTRACTOR
# Language: R
#
# WHAT THIS SCRIPT DOES:
#   Reads every per-state "Compendium of Fact Sheets" PDF in
#   nfhs 6 district factsheets/ (one PDF per state/UT, each bundling that
#   state's own factsheet PLUS every one of its districts — e.g. the Bihar
#   file is 168 pages), extracts ONLY the district-level pages, keeps ONLY
#   the NFHS-6 value column (NFHS-5 is discarded — district-level NFHS-5 is
#   already in the output CSV), and APPENDS those rows to the dashboard's
#   active CSV.
#
# WHY THIS FILE, NOT nfhs_all_data.csv IN Downloads OR archive/:
#   Checked 2026-09-06 — three NFHS "all data" CSVs exist across two separate
#   projects (Downloads/NFHS all factsheets/, this project's root, and this
#   project's archive/). This project's root file
#   ("nfhs_all_states .csv") is the one the dashboard's NFHS-6 state-level
#   rows actually live in (3,386 rows, all Geo Level = State/National, 0
#   District) and district-level NFHS-4/5 already lives in it too (695
#   districts). That is the file this script appends to.
#
# WHY THIS IS ALMOST IDENTICAL TO archive/3. extract_nfhs6_compendium.R:
#   That script already solved parsing THIS exact PDF family (indicator_meta
#   with the three NFHS-6-only indicators, is_skip_line with NFHS-6's new
#   footnote/disclaimer text, GEO_NAME_FIX for the three spelling mismatches
#   vs. NFHS-5). It only extracted STATE pages (its header-matcher explicitly
#   skips any header with a comma, i.e. district headers). This script is
#   that same engine pointed at the opposite: it skips headers WITHOUT a
#   comma (state pages) and keeps only headers WITH one (district pages,
#   "District, State - Key Indicators").
#
# OUTPUT COLUMNS (unchanged):
#   Indicator | Geography | Geo Level | Round | Value | Parent State | Domain | Direction
#
# INSTALL (run once):
#   install.packages(c("pdftools", "tidyverse"))
# =============================================================================

library(pdftools)
library(tidyverse)


# =============================================================================
# CONFIG
# =============================================================================

COMPENDIUM_FOLDER <- "C:/Users/Cegis/Desktop/claude cowork/nfhs dashboard/nfhs 6 district factsheets"
OUTPUT_FILE       <- "C:/Users/Cegis/Desktop/claude cowork/nfhs dashboard/nfhs_all_states .csv"
CURRENT_ROUND     <- "NFHS-6"   # only this round's column is kept; NFHS-5 on the same page is read then discarded


# =============================================================================
# GEOGRAPHY NAME NORMALISATION — identical crosswalk to the state script, so
# Parent State spelling matches the existing dataset's NFHS-5 conventions.
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
# DOMAIN + DIRECTION LOOKUP TABLE — identical to archive/3. extract_nfhs6_compendium.R,
# including its three NFHS-6-only additions (hepatitis b, rotavirus, solid or
# semi-solid food). Indicator classification doesn't change by geo level.
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
# SHARED HELPER FUNCTIONS — identical to archive/3. extract_nfhs6_compendium.R
# (which already carries the NFHS-6-specific footnote/disclaimer patterns).
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

# clean_name \u2014 upgraded from archive/3. extract_nfhs6_compendium.R's version.
# That version only stripped a SINGLE glued footnote digit (e.g. "recall11")
# and left comma-separated footnote lists untouched, producing garbage like
# "...adequate diet10, 11 (%)" in the district compendium output (confirmed
# in a Goa sandbox test run, 2026-09-06). Ported the fuller footnote-digit
# handling from 2.convert_district_state_factsheets_to_excel.R (the NFHS-5
# script), which explicitly handles comma-lists ("diet10, 11 (%)" -> "diet (%)")
# and several other footnote attachment styles. See that script's header
# comment above clean_name() for the full step-by-step rationale.
clean_name <- function(x) {
  x |>
    str_trim() |>
    str_remove("^\\d+\\.\\s*") |>
    str_remove_all("[\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u2070]+") |>
    # Glued footnote digit(s), optional comma-list, before space or (
    # e.g. "recall11 (%)" -> "recall (%)", "diet10, 11 (%)" -> "diet (%)"
    str_replace_all("(?<=[a-zA-Z])(\\d{1,2})(?:\\s*,\\s*\\d{1,2})*(?=\\s|\\()", "") |>
    str_remove("(?<=\\(%\\))\\s*\\d{1,2}\\s*$") |>
    str_remove("(?<=[a-zA-Z])\\s*\\d{1,2}\\s*$") |>
    # Footnote(s) between ) and ( e.g. "age)18 (%)" or "age)10, 11 (%)"
    str_remove_all("\\)\\d{1,2}(?:\\s*,\\s*\\d{1,2})*(?=\\s*\\()") |>
    # Spaced comma-list before ( e.g. "diet 10, 11 (%)" -> "diet (%)"
    str_remove_all("\\s\\d{1,2}(?:\\s*,\\s*\\d{1,2})+(?=\\s*\\()") |>
    # Lone spaced footnote before ( e.g. "birth 15 (%)" -> "birth (%)"
    # (?<!age) preserves meaningful age thresholds: "age 18 (%)"
    str_remove_all("(?<!age)\\s\\d{1,2}(?=\\s*\\()") |>
    str_squish()
}

to_num <- function(x) {
  x <- str_trim(x)
  if (is.na(x) || x == "" || x == "*") return(NA_real_)
  if (str_to_lower(x) %in% c("na", "n/a", "-", "—")) return(NA_real_)
  x <- str_remove_all(x, "[\\(\\)]")   # strip parentheses: (3.7) -> 3.7 (district small-sample flag)
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
    "Appendix",
    "COMPENDIUM OF FACT SHEETS",   # district compendium cover-page text
    "KEY INDICATORS OF STATE AND DISTRICTS",
    "CONTRIBUTORS", "AUGUST 2026"
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
    "^[0-9]+\\s+Hepatitis", "^[0-9]+\\s+Rotavirus",
    "^[0-9]+\\s+Children\\s+who\\s+received",
    "^[0-9]+\\s+Measured", "^[0-9]+\\s+An\\s+adequate",
    "^[0-9]+\\s+According", "^[0-9]+\\s+Defined",
    "^[0-9]+\\s+Current",
    "^\\*\\s*Percentage",                     # district: "* Percentage not shown"
    "^\\(\\s*\\)\\s*Based\\s+on\\s+25-49"     # district: small-sample note
  )
  if (any(str_detect(line, footnote_starts))) return(TRUE)

  if (str_detect(line, "^[\\u00b7\\u22c5\\u2022]")) return(TRUE)
  if (str_detect(line, "^\\(\\s*\\)")) return(TRUE)

  FALSE
}

# Comma-in-parentheses support ("(2,965)") added, matching
# 2.convert_district_state_factsheets_to_excel.R — district sheets
# parenthesise large-number small-sample cells this way.
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


# ── process_lines ─────────────────────────────────────────────────────────────
# Line-matching logic ported from archive/3. extract_nfhs6_compendium.R, PLUS
# two pieces of robustness that script never got but
# 2.convert_district_state_factsheets_to_excel.R (the NFHS-5 script) already
# proved necessary — confirmed missing here after a full 33-PDF run
# (2026-09-06) produced 547 low-coverage indicators, most of them the SAME
# real indicator (e.g. "Elevated blood pressure...") fragmented into dozens
# of variants each ending in a different absorbed number
# ("...taking medicine to 10.5", "...to 17.8", etc.):
#
#   1. VALUE SALVAGE — some indicator names leave only 1 space before the
#      Urban value column instead of the 2+ spaces INDICATOR_REGEX's lazy
#      match needs to split on. That 1-space gap lets the first value get
#      absorbed into raw_name, producing a different garbage name PER
#      DISTRICT (each district has a different Urban value). This is the
#      exact mechanism behind the "...to 10.5" / "...to 17.8" fragments.
#   2. DUAL-INDICATOR SPLIT — when two indicators end up concatenated on one
#      physical line, the "sacred name" from #1 also needs unpacking.
#
# See 2.convert_district_state_factsheets_to_excel.R's comments directly
# above its process_lines() for the full line-by-line rationale; ported
# verbatim below with only the round-handling collapsed to current-round-only.
#
# ONE further change from that NFHS-5 script: it emitted a second row for the
# previous round. This version discards that value — district-level NFHS-5
# is already in the output CSV — and writes only the current round (NFHS-6).
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

    # Value salvage — see comment block above process_lines() for why this
    # step exists. Strips a trailing value token accidentally absorbed into
    # raw_name and prepends it to vals_part so "second-to-last token" still
    # lands on the correct current-round column.
    salvage_pat <- "\\s+((?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+)(?:\\s+(?:\\*|na|\\([\\d\\.,]+\\)|[\\d\\.,]+))*)\\s*$"
    salvaged    <- str_match(raw_name, salvage_pat)
    if (!is.na(salvaged[1, 1])) {
      raw_name  <- str_remove(raw_name, salvage_pat)
      vals_part <- str_squish(str_c(salvaged[1, 2], " ", vals_part))
    }

    # Truncation check — a name not ending in ")" and longer than 20 chars is
    # cut off mid-sentence (wrapped across two lines); wait for the
    # continuation to complete it rather than emitting a fragment.
    if (!str_detect(raw_name, "\\)\\s*$") && nchar(raw_name) > 20) {
      pending_name <- raw_name
      next
    }

    # Dual-indicator split — raw_name contains an embedded second indicator
    # (" N. Capital..."), arising the same way the NFHS-5 script documents.
    # Recover the first indicator's value via salvage on the part before the
    # embedded second indicator, then store the second indicator normally.
    dual_match <- str_match(raw_name, "\\s+(\\d{1,3}\\.\\s+[A-Za-z].+)$")
    if (!is.na(dual_match[1, 1])) {
      first_raw      <- str_remove(raw_name, "\\s+\\d{1,3}\\.\\s+[A-Za-z].*$")
      first_salvaged <- str_match(first_raw, salvage_pat)
      if (!is.na(first_salvaged[1, 1])) {
        first_name   <- clean_name(str_remove(first_raw, salvage_pat))
        first_tokens <- str_split(str_squish(first_salvaged[1, 2]), "\\s+")[[1]]
        first_tokens <- first_tokens[nchar(first_tokens) > 0]
        if (nchar(first_name) >= 6 && length(first_tokens) >= 1) {
          f_cur <- if (length(first_tokens) >= 2) to_num(first_tokens[length(first_tokens) - 1]) else to_num(first_tokens[1])
          f_meta <- lookup_meta(first_name)
          if (!is.na(f_cur))
            records <- c(records, list(tibble(
              Indicator = first_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = round_label, Value = f_cur, `Parent State` = parent_state,
              Domain = f_meta$domain, Direction = f_meta$direction)))
        }
      }
      second_name <- clean_name(dual_match[1, 2])
      if (nchar(second_name) >= 6) {
        s_tokens <- str_split(str_squish(vals_part), "\\s+")[[1]]
        s_tokens <- s_tokens[nchar(s_tokens) > 0]
        if (length(s_tokens) >= 1) {
          s_cur <- if (length(s_tokens) >= 2) to_num(s_tokens[length(s_tokens) - 1]) else to_num(s_tokens[1])
          s_meta <- lookup_meta(second_name)
          if (!is.na(s_cur))
            records <- c(records, list(tibble(
              Indicator = second_name, Geography = geo_name, `Geo Level` = geo_level,
              Round = round_label, Value = s_cur, `Parent State` = parent_state,
              Domain = s_meta$domain, Direction = s_meta$direction)))
        }
      }
      pending_name <- NA_character_
      next
    }

    val_tokens <- str_split(vals_part, "\\s+")[[1]]
    val_tokens <- val_tokens[nchar(val_tokens) > 0]
    if (length(val_tokens) < 1) next

    # Second-to-last token = current round (NFHS-6) Total column.
    # Last token (NFHS-5) is read for column bookkeeping only, then discarded.
    cur_val <- if (length(val_tokens) >= 2) to_num(val_tokens[length(val_tokens) - 1]) else to_num(val_tokens[1])

    if (is.na(cur_val)) next

    indicator_name <- clean_name(raw_name)
    if (nchar(indicator_name) < 6) next

    meta <- lookup_meta(indicator_name)

    records <- c(records, list(tibble(
      Indicator      = indicator_name,
      Geography      = geo_name,
      `Geo Level`    = geo_level,
      Round          = round_label,
      Value          = cur_val,
      `Parent State` = parent_state,
      Domain         = meta$domain,
      Direction      = meta$direction
    )))
  }
  list(records = records, pending_name = pending_name)
}


# =============================================================================
# DISTRICT-PAGE COMPENDIUM PARSER
#
# Each PDF bundles one state's own factsheet pages FIRST, then every district
# in that state (same "grouping by repeated header" structure as the national
# state-compendium script). We reuse the same rle()-based page-grouping trick,
# but flip which headers we keep:
#   - header WITHOUT a comma  -> state's own page-group -> SKIPPED (already
#     extracted by archive/3. extract_nfhs6_compendium.R from the national
#     compendium)
#   - header WITH a comma ("District, State - Key Indicators") -> district
#     page-group -> KEPT
# =============================================================================

extract_district_header <- function(page_text) {
  lines <- str_split(page_text, "\n")[[1]]
  header_line <- lines[str_detect(lines, "-\\s*Key Indicators")]
  if (length(header_line) == 0) return(NA_character_)
  geo_part <- str_squish(str_extract(header_line[1], "^[^-]+"))
  if (is.na(geo_part) || !str_detect(geo_part, ",")) return(NA_character_)  # state page, not district
  geo_part
}

parse_one_state_compendium <- function(pdf_path) {
  message("\n\U0001F4C4 Reading: ", basename(pdf_path))
  pages <- tryCatch(pdf_text(pdf_path),
                    error = function(e) { message("\u274c ", e$message); NULL })
  if (is.null(pages)) return(NULL)
  message("   Total pages: ", length(pages))

  # Tag each page with its district header text (NA for state/non-data pages)
  page_geos <- map_chr(pages, extract_district_header)

  # Group consecutive pages that share the same district header
  geo_groups <- rle(page_geos)
  all_records <- list()
  page_idx    <- 1L

  for (g in seq_along(geo_groups$values)) {
    geo_raw   <- geo_groups$values[g]
    n_pages   <- geo_groups$lengths[g]
    page_idxs <- seq(page_idx, page_idx + n_pages - 1L)
    page_idx  <- page_idx + n_pages

    if (is.na(geo_raw)) next  # state page or non-data page — not our target

    parts        <- str_split(geo_raw, ",\\s*")[[1]]
    district_name <- str_squish(parts[1])
    state_name    <- normalise_geo(str_squish(parts[2]))
    message("  [District] ", district_name, ", ", state_name, " (", n_pages, " page(s))")

    records      <- list()
    pending_name <- NA_character_

    for (pi in page_idxs) {
      lines  <- str_split(pages[pi], "\n")[[1]]
      result <- process_lines(lines, district_name, "District", state_name,
                              CURRENT_ROUND, records, pending_name)
      records      <- result$records
      pending_name <- result$pending_name
    }

    if (length(records) == 0) { message("    \u26a0\ufe0f  No records extracted"); next }

    df <- bind_rows(records) |>
      distinct(Indicator, Geography, Round, .keep_all = TRUE)
    message("    \u2705 ", nrow(df), " rows (", n_distinct(df$Indicator), " indicators)")
    all_records <- c(all_records, list(df))
  }

  if (length(all_records) == 0) { message("\u274c No district records in this file."); return(NULL) }
  bind_rows(all_records)
}


# =============================================================================
# RUN — one compendium PDF per state/UT, then append district rows to the
# dashboard's active CSV.
# =============================================================================

compendium_files <- list.files(COMPENDIUM_FOLDER, pattern = "\\.pdf$",
                               full.names = TRUE, ignore.case = TRUE)
message("\U0001F4C2 Found ", length(compendium_files), " state compendium PDF(s)\n")

all_results <- map(compendium_files, parse_one_state_compendium) |> compact()

if (length(all_results) == 0) {
  stop("No district rows extracted from any PDF. Check COMPENDIUM_FOLDER (",
       COMPENDIUM_FOLDER, ") actually contains the state compendium PDFs, ",
       "and review the per-file messages above for parse failures.")
}

new_df <- bind_rows(all_results)

# ── FRAGMENT FILTER — same 2-condition rule as the other extractors: drop
# indicator-name variants/fragments seen in fewer than 5 districts AND that
# look incomplete (missing unit marker or under 5 words).
geo_counts <- new_df |> count(Indicator, name = "n_geos")
fragments <- geo_counts |>
  filter(
    n_geos < 5,
    !str_detect(Indicator, "\\)\\s*$") |
    str_count(Indicator, "\\S+") < 5
  ) |>
  pull(Indicator)

if (length(fragments) > 0) {
  message("\n\U0001F9F9 Removing ", length(fragments),
          " extraction fragment(s) (< 5 districts AND incomplete name):")
  walk(fragments, ~ message('   "', str_sub(.x, 1, 70), '"'))
  new_df <- new_df |> filter(!Indicator %in% fragments)
}

unknowns <- new_df |> filter(Domain == "Unknown") |> distinct(Indicator) |> pull(Indicator)
if (length(unknowns) > 0) {
  message("\u26a0\ufe0f  ", length(unknowns), " indicator(s) with unknown domain \u2014 add a keyword to indicator_meta above:")
  walk(unknowns, ~ message('  "', str_to_lower(str_sub(.x, 1, 55)), '",'))
}

# ── MERGE WITH EXISTING CSV ───────────────────────────────────────────────────
if (file.exists(OUTPUT_FILE)) {
  existing_df <- read_csv(OUTPUT_FILE, show_col_types = FALSE)
  # Drop any pre-existing CURRENT_ROUND rows for (Geography, Geo Level) pairs
  # we just re-parsed, so re-running this script replaces rather than
  # duplicates them. Keyed on BOTH fields together — several districts share
  # a name with their own state/UT (e.g. Chandigarh), so Geography alone
  # would risk dropping the wrong level's row.
  reparsed_keys <- new_df |> distinct(Geography, `Geo Level`)
  existing_df <- existing_df |>
    anti_join(reparsed_keys |> mutate(Round = CURRENT_ROUND),
              by = c("Geography", "Geo Level", "Round"))
  master_df <- bind_rows(existing_df, new_df)
} else {
  stop(OUTPUT_FILE, " not found \u2014 refusing to guess. Point OUTPUT_FILE at the real dashboard CSV.")
}

write_csv(master_df, OUTPUT_FILE)

message("\n\u2705 Done!")
message("   District NFHS-6 rows added : ", nrow(new_df))
message("   Distinct districts parsed  : ", n_distinct(new_df$Geography))
message("   Total rows in file         : ", nrow(master_df))
message("   Output                     : ", OUTPUT_FILE)


# =============================================================================
# LOW-COVERAGE DIAGNOSTIC (on the newly added NFHS-6 district rows) — run
# this before trusting the output. An indicator appearing in very few
# districts usually means a name-variant or a missed value format (e.g. the
# "(2,965)" comma-in-parens case called out above INDICATOR_REGEX).
# =============================================================================

diag_threshold <- 10
low_coverage_summary <- new_df |> count(Indicator, sort = TRUE) |> filter(n < diag_threshold)
message("\n\u26a0\ufe0f  ", nrow(low_coverage_summary),
        " NFHS-6 district indicator(s) appearing in fewer than ", diag_threshold, " districts:")
print(low_coverage_summary, n = 60)

diag_path <- str_replace(OUTPUT_FILE, "\\.csv$", "_nfhs6_district_low_coverage_diag.csv")
new_df |>
  filter(Indicator %in% low_coverage_summary$Indicator) |>
  select(Indicator, Geography, `Geo Level`, Round, Value, `Parent State`, Domain) |>
  arrange(Indicator, Geography, Round) |>
  write_csv(diag_path)
