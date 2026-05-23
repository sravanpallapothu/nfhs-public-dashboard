# =============================================================================
# NFHS-5 FACT SHEET PDF DOWNLOADER
# Downloads all state AND district PDFs from dhsprogram.com
#
# METHOD:
#   Uses a session cookie + HTML form POST (field: indiastatecode) to fetch
#   the DHS publications page for each state, extracts all PDF links from
#   the response, then downloads each one.
#   No browser automation needed — plain httr requests.
#
# INSTALL (run once):
#   install.packages(c("httr", "tidyverse"))
#
# HOW TO USE:
#   1. Set DOWNLOAD_DIR below
#   2. Source/run the script
#   3. State PDFs → DOWNLOAD_DIR/states/
#      District PDFs → DOWNLOAD_DIR/districts/
# =============================================================================

library(httr)
library(tidyverse)

# =============================================================================
# CONFIG
# =============================================================================

DOWNLOAD_DIR <- "C:/Users/X/Downloads/NHFS district factsheets"
PAUSE_SECS   <- 1.5   # pause between downloads — be polite to the server

BASE_URL     <- "https://www.dhsprogram.com"
PAGE_URL     <- "https://dhsprogram.com/publications/publication-OF43-Other-Fact-Sheets.cfm"

# =============================================================================
# ALL 36 STATE/UT CODES
# =============================================================================

all_states <- tribble(
  ~code, ~name,
  "AN",  "Andaman and Nicobar Islands",
  "AP",  "Andhra Pradesh",
  "AR",  "Arunachal Pradesh",
  "AS",  "Assam",
  "BR",  "Bihar",
  "CH",  "Chandigarh",
  "CG",  "Chhattisgarh",
  "DN",  "Dadra and Nagar Haveli and Daman and Diu",
  "DL",  "Delhi",
  "GA",  "Goa",
  "GJ",  "Gujarat",
  "HR",  "Haryana",
  "HP",  "Himachal Pradesh",
  "JK",  "Jammu and Kashmir",
  "JH",  "Jharkhand",
  "KA",  "Karnataka",
  "KL",  "Kerala",
  "LA",  "Ladakh",
  "LD",  "Lakshadweep",
  "MP",  "Madhya Pradesh",
  "MH",  "Maharashtra",
  "MN",  "Manipur",
  "ML",  "Meghalaya",
  "MZ",  "Mizoram",
  "NL",  "Nagaland",
  "OR",  "Odisha",
  "PY",  "Puducherry",
  "PB",  "Punjab",
  "RJ",  "Rajasthan",
  "SK",  "Sikkim",
  "TN",  "Tamil Nadu",
  "TG",  "Telangana",
  "TR",  "Tripura",
  "UP",  "Uttar Pradesh",
  "UT",  "Uttarakhand",
  "WB",  "West Bengal"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Makes a safe filename (no forbidden Windows characters)
safe_name <- function(x) str_replace_all(x, "[/\\\\:*?\"<>|]", "_")

# Downloads a single PDF. Skips if already exists.
download_pdf <- function(url, dest_path) {
  if (file.exists(dest_path)) {
    message("    ⏭  Already exists: ", basename(dest_path))
    return(TRUE)
  }
  resp <- tryCatch(
    GET(
      url,
      timeout(30),
      write_disk(dest_path, overwrite = FALSE),
      add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36")
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) {
    if (file.exists(dest_path)) file.remove(dest_path)
    message("    ❌ Failed: ", basename(dest_path))
    return(FALSE)
  }
  message("    ✅ Downloaded: ", basename(dest_path))
  Sys.sleep(PAUSE_SECS)
  TRUE
}

# =============================================================================
# STEP 1: ESTABLISH SESSION COOKIE AND READ STATE LIST FROM DROPDOWN
# The DHS site requires a session cookie — we get it by visiting the page first.
# We also read the state codes and names directly from the dropdown on the page
# so we never need to hardcode them — if DHS adds or renames a state, this
# script will automatically pick it up.
# =============================================================================

message("🌐 Fetching DHS page and reading state dropdown...")

r1 <- GET(
  PAGE_URL,
  add_headers(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36"
  )
)

session_cookies <- cookies(r1)
cookie_config   <- set_cookies(.cookies = setNames(session_cookies$value,
                                                   session_cookies$name))

# Parse the HTML and extract all <option> elements inside the state dropdown
# The dropdown has name="indiastatecode" — we read value= and text for each option
page_html <- content(r1, as = "text", encoding = "UTF-8")

# Extract all option tags from the indiastatecode select element
# Pattern captures: value="XX" and the option text (state name)
option_matches <- str_match_all(
  page_html,
  "(?s)<select[^>]*name=[\"']indiastatecode[\"'][^>]*>(.*?)</select>"
)[[1]]

if (length(option_matches) == 0 || is.na(option_matches[1, 2])) {
  stop("❌ Could not find indiastatecode dropdown in page HTML. DHS may have changed their page structure.")
}

dropdown_html <- option_matches[1, 2]

# Extract each option: value attribute and display text
options_raw <- str_match_all(
  dropdown_html,
  "<option\\s+value=[\"']([^\"']+)[\"'][^>]*>\\s*([^<]+?)\\s*</option>"
)[[1]]

# Build the states tibble from the dropdown — skip the placeholder "--- Select ---" option
all_states <- tibble(
  code = options_raw[, 2],
  name = str_squish(options_raw[, 3])
) |>
  filter(nchar(code) > 0, !str_detect(name, "(?i)select|^-"))

message("✅ Session established. Found ", nrow(all_states), " states/UTs in dropdown:")
walk(seq_len(nrow(all_states)), ~ message("   ", all_states$code[.x], " — ", all_states$name[.x]))

# =============================================================================
# STEP 2: CREATE OUTPUT FOLDERS
# =============================================================================

dir.create(file.path(DOWNLOAD_DIR, "states"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DOWNLOAD_DIR, "districts"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# STEP 3: LOOP THROUGH EVERY STATE
# For each state:
#   a) POST with indiastatecode=XX to get the page showing all PDFs for that state
#   b) Extract all PDF links from the response
#   c) Classify each link as state-level or district-level
#   d) Download each PDF with the naming convention:
#      State:    OF43_XX.pdf       e.g. OF43_AS.pdf
#      District: XX_District.pdf   e.g. AS_Dibrugarh.pdf
# =============================================================================

# Track results across all states
all_results <- tibble(
  type      = character(),
  state     = character(),
  district  = character(),
  url       = character(),
  filename  = character(),
  success   = logical()
)

message("\n📥 Starting downloads for ", nrow(all_states), " states...\n")

for (i in seq_len(nrow(all_states))) {
  code       <- all_states$code[i]
  state_name <- all_states$name[i]
  
  message("[", i, "/", nrow(all_states), "] ", state_name, " (", code, ")")
  
  # ── POST to get the page for this state ────────────────────────────────────
  resp <- tryCatch(
    POST(
      PAGE_URL,
      add_headers(
        "User-Agent"   = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36",
        "Referer"      = PAGE_URL,
        "Content-Type" = "application/x-www-form-urlencoded",
        "Origin"       = "https://dhsprogram.com"
      ),
      body   = list(indiastatecode = code),
      encode = "form",
      config = cookie_config
    ),
    error = function(e) {
      message("  ❌ POST failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(resp)) next
  resp_text <- content(resp, as = "text", encoding = "UTF-8")
  
  # ── Extract all OF43 PDF links from the response ───────────────────────────
  raw_links <- str_extract_all(
    resp_text,
    "/pubs/pdf/OF43/[^\"'<>\\s]+\\.pdf"
  )[[1]] |> unique()
  
  # Remove national/compendium links — only keep state and district links
  pdf_links <- raw_links |>
    discard(~ str_detect(.x, "India_National|Compendium"))
  
  if (length(pdf_links) == 0) {
    message("  ⚠️  No PDFs found for ", state_name)
    next
  }
  
  # ── Classify and download each link ───────────────────────────────────────
  for (link in pdf_links) {
    
    full_url  <- paste0(BASE_URL, link)
    file_stem <- str_extract(link, "[^/]+(?=\\.pdf$)")  # e.g. "AS_Dibrugarh"
    
    # State-level: matches pattern OF43.XX.pdf
    is_state_pdf <- str_detect(file_stem, paste0("^OF43\\.", code, "$"))
    
    if (is_state_pdf) {
      # State PDF: save as OF43_AS.pdf
      filename  <- paste0("OF43_", code, ".pdf")
      dest_path <- file.path(DOWNLOAD_DIR, "states", filename)
      type      <- "State"
      district  <- NA_character_
      
    } else {
      # District PDF: save as AS_Dibrugarh.pdf (exactly as named on server)
      filename  <- paste0(safe_name(file_stem), ".pdf")
      dest_path <- file.path(DOWNLOAD_DIR, "districts", filename)
      type      <- "District"
      
      # Extract district name: "AS_Dima_Hasao" → "Dima Hasao"
      district <- file_stem |>
        str_remove(paste0("^", code, "_")) |>
        str_replace_all("_", " ")
    }
    
    message("  [", type, "] ", basename(dest_path))
    success <- download_pdf(full_url, dest_path)
    
    all_results <- bind_rows(all_results, tibble(
      type     = type,
      state    = state_name,
      district = district,
      url      = full_url,
      filename = filename,
      success  = success
    ))
  }
  
  message()  # blank line between states
}

# =============================================================================
# STEP 4: SUMMARY + SAVE LINK INVENTORY
# =============================================================================

n_ok   <- sum(all_results$success,  na.rm = TRUE)
n_fail <- sum(!all_results$success, na.rm = TRUE)

message("✅ All done!")
message("   States downloaded    : ", sum(all_results$type == "State"    & all_results$success))
message("   Districts downloaded : ", sum(all_results$type == "District" & all_results$success))
message("   Failed               : ", n_fail)

# Save full inventory CSV — useful for the district parser later
inventory_path <- file.path(DOWNLOAD_DIR, "nfhs_pdf_inventory.csv")
write_csv(all_results, inventory_path)
message("   Inventory saved to   : ", inventory_path)

if (n_fail > 0) {
  message("\n❌ Failed downloads:")
  all_results |>
    filter(!success) |>
    mutate(msg = paste0("  - ", state, " / ", coalesce(district, "state"), " → ", url)) |>
    pull(msg) |>
    walk(message)
}

# Also extract and print the tribble rows for any districts found
# (useful if you want to add them to the Fallback C lookup table)
district_rows <- all_results |>
  filter(type == "District", success) |>
  mutate(
    state_code = str_extract(filename, "^[A-Z]{2,3}"),
    row        = paste0('"', state_code, '", "', state, '", "', district, '",')
  ) |>
  pull(row)

if (length(district_rows) > 0) {
  tribble_path <- file.path(DOWNLOAD_DIR, "district_tribble_rows.txt")
  writeLines(district_rows, tribble_path)
  message("\n📋 Tribble rows for Fallback C saved to: ", tribble_path)
  message("    (", length(district_rows), " district rows ready to paste into the downloader script)")
}