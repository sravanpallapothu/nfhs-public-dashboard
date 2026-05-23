# NFHS Public Dashboard

![GitHub repo size](https://img.shields.io/github/repo-size/sravanpallapothu/nhfspublicdashboard)
![GitHub contributors](https://img.shields.io/github/contributors/sravanpallapothu/nhfspublicdashboard)
![GitHub last commit](https://img.shields.io/github/last-commit/sravanpallapothu/nhfspublicdashboard)

An interactive, browser-based dashboard for exploring National Family Health Survey (NFHS) data across India — at national, state, and district levels. Built entirely in HTML, CSS, and JavaScript with no backend, no server, and no paid services.

**Live dashboard (Netlify):** [https://nfhs-dashboard.netlify.app](https://nfhs-dashboard.netlify.app)  
**Repository (GitHub):** [https://sravanpallapothu.github.io/nhfspublicdashboard/](https://sravanpallapothu.github.io/nhfspublicdashboard/)
**Link to Backend data source for dashboard (extracted from indvidual factsheets using R)**: [https://docs.google.com/spreadsheets/d/1RH9p8c-x-1GWmHf7TmeKNOlOIdTTjZms1O8DdhcH8Ug/edit?gid=836994213#gid=836994213]
(https://docs.google.com/spreadsheets/d/1RH9p8c-x-1GWmHf7TmeKNOlOIdTTjZms1O8DdhcH8Ug/edit?gid=836994213#gid=836994213)
---

## Purpose

The NFHS is India's primary source of health, nutrition, and demographic indicators at sub-national levels. While the raw factsheets are available as PDFs, there was no single tool that allowed users to:

- Compare states or districts on any indicator in one view
- Track change over time across survey rounds (NFHS-4 and NFHS-5)
- Visualise geographic patterns through choropleth maps
- Download data for further analysis

This dashboard fills that gap. It is designed for policy practitioners, researchers, and state government teams who need quick, reliable access to NFHS data without needing to open individual PDFs.

---

## Features

The dashboard has five screens, accessible from the top navigation bar.

### 1. Snapshot
Select an indicator, state, and survey round. The dashboard shows eight reference cards: the national value, the selected state's value, and the highest and lowest performing states and districts — both nationally and within the selected state. Where data exists for a previous round, each card shows a change arrow (e.g. ↑ 4.2pp) coloured green for improvement and red for deterioration, direction-aware so that a falling stunting rate correctly shows green.

### 2. Temporal Trend
Line chart showing how an indicator has changed across NFHS rounds for a selected geography. At district level, the chart overlays the parent state and national lines for context. Supports download as PNG or CSV.

### 3. Spatial Bar Chart
Ranked horizontal bar chart comparing all states (or all districts within a state) on a selected indicator. A violet bar marks the reference benchmark — the national average for state comparisons, or the state average for district comparisons — making it immediately visible how many geographies perform above or below the benchmark. Supports download as PNG or CSV.

### 4. Choropleth Map
Colour-coded map of India at three levels of granularity: India with state boundaries, India with all district boundaries, or a single state with its districts. Colours are based on standard deviation from the mean:

- **Green**: more than one SD better than average
- **Amber**: within one SD of the mean
- **Red**: more than one SD worse than average
- **Grey**: no data

Colour direction is indicator-aware — for "lower is better" indicators like stunting, low values are green. A reference value (national or state average) is shown in the map header. Supports zoom, pan, hover tooltips, PNG download, and CSV download.

### 5. How to Use
Built-in documentation tab covering all four screens plus general tips.

---

## Data Pipeline

The dashboard is produced by a four-step pipeline: download PDFs → extract to CSV → clean and convert shapefiles → build the dashboard HTML. Steps 1–3 are R scripts. Step 4 is a hand-authored HTML file.

> **Before running any R script:** open the script and update the file paths in the CONFIG section at the top. All paths are currently set to the original author's local machine (e.g. `C:/Users/X/...` or `G:/.shortcut-targets-by-id/...`) and will not work on another machine without editing.

---

### Step 1 — Downloading the PDFs (`1__scrape_state_district_factsheets_from_dhs_website.R`)

**What it does:** Downloads all NFHS-5 state and district factsheet PDFs from the DHS programme website for all 36 states and UTs, saving them into two subfolders and producing an inventory CSV.

**Config to change before running:**
```r
DOWNLOAD_DIR <- "C:/Users/X/Downloads/NHFS district factsheets"  # ← change to your local path
PAUSE_SECS   <- 1.5   # pause between downloads — increase if the server is slow or rate-limiting you
```

**Output:**
- `DOWNLOAD_DIR/states/` — one PDF per state, named `OF43_XX.pdf` (e.g. `OF43_BR.pdf` for Bihar)
- `DOWNLOAD_DIR/districts/` — one PDF per district, named `XX_DistrictName.pdf` (e.g. `BR_Patna.pdf`)
- `DOWNLOAD_DIR/nfhs_pdf_inventory.csv` — full log of every URL attempted and whether it succeeded

**Dependencies:** `httr`, `tidyverse`

**How it works in detail:**

The DHS website requires a session cookie and an HTML form POST to retrieve the publications page for each state — a plain URL request without the cookie returns an empty or error page. The script establishes the session by making a GET request to the DHS publications page first, then reads the state dropdown directly from the returned HTML rather than using a hardcoded list of state codes. This means that if DHS adds or renames a state, the script picks it up automatically without any manual updates.

For each of the 36 states and UTs, the script POSTs the form field `indiastatecode` with the two-letter state code, extracts all PDF links matching the NFHS-5 pattern from the response HTML, classifies each as either a state factsheet or a district factsheet, and downloads it with a browser-like User-Agent header. A configurable pause between downloads (`PAUSE_SECS`) avoids hammering the server. Files that already exist are silently skipped, so the script is safe to re-run after a partial download or network interruption.

After all downloads complete, the script prints a summary (states downloaded, districts downloaded, failures) and saves the full inventory to a CSV. The inventory CSV is also useful as an input to the district parser in Step 2 — it records the exact district name as it appears in the PDF filename, which must eventually match the district name in the shapefile.

---

### Step 2 — Extracting indicators from PDFs (`2__convert_district_state_factsheets_to_excel.R`)

**What it does:** Reads all state and district PDFs, extracts indicator names and values for both NFHS-5 and NFHS-4, and writes a single combined CSV with columns: `Indicator | Geography | Geo Level | Round | Value | Parent State | Domain | Direction`.

**Config to change before running:**
```r
STATE_FOLDER    <- "C:/Users/X/Downloads/NFHS all factsheets/state"      # ← change this
DISTRICT_FOLDER <- "C:/Users/X/Downloads/NFHS all factsheets/districts"  # ← change this
OUTPUT_FILE     <- "C:/Users/X/Downloads/NFHS all factsheets/nfhs_all_data.csv"  # ← change this
```

Also update the path in the diagnostic block at the bottom of the script:
```r
DIAG_PDF <- "C:/Users/X/Downloads/NFHS all factsheets/state/OF43_BR.pdf"  # ← change this
```

**Dependencies:** `pdftools`, `tidyverse`

**How it works in detail:**

*PDF text extraction.* The script uses `pdftools::pdf_text()` to extract raw text from each PDF page by page. PDF text extraction is unstructured — indicator names and values arrive as a single character stream with column gaps represented as whitespace rather than actual delimiters. The extraction problem is essentially: given a line like `"50. Institutional births (%)  90.3  79.7  88.1  78.9"`, separate the indicator name from the four value columns.

*One function for states and districts.* State factsheets have four value columns (Urban, Rural, NFHS-5 Total, NFHS-4 Total); district factsheets have two (NFHS-5 Total, NFHS-4 Total). Both are handled by a single shared extraction function (`process_lines`) that always takes the last two tokens from each matched row. This works for both formats: for a state sheet the last two tokens are NFHS-5 Total and NFHS-4 Total; for a district sheet the only two tokens are exactly those. No format-specific branching is needed.

*Identifying state vs district PDFs.* The script distinguishes them by the header line on each PDF page. State factsheets have headers like `"Bihar - Key Indicators"`. District factsheets have headers like `"Jaisalmer, Rajasthan - Key Indicators"` — the comma separating district from parent state name is the discriminating signal. The district name and parent state are both extracted from this header; the parent state is stored in the `Parent State` column, which the dashboard uses for hierarchical filtering (select a state, see only its districts).

*The main regex.* Two or more consecutive spaces are used as the column separator, because single spaces appear within indicator names ("age 15–49 years") but column gaps in the PDF are always wider. The pattern is:

```
^((?:\d+\.\s+)?[A-Za-z].+?)\s{2,}(value_tokens)\s*$
```

The regex is applied to `str_trim(line)` — not `str_squish(line)`. This is important: `str_squish` collapses all whitespace to single spaces, which destroys the column gaps the regex depends on. `str_trim` only removes leading and trailing whitespace, leaving internal gaps intact. This distinction fixed a critical bug where indicator numbers 101–110 (which had leading spaces in the PDF) were being silently dropped.

*Handling non-standard value tokens.* NFHS factsheets use four special representations that plain number parsing cannot handle:
- `*` — fewer than 25 unweighted cases; the estimate is not reportable. Converted to `NA`. Appears only in district sheets (small sample sizes in small districts).
- `na` — the indicator was not collected in that round, usually NFHS-4. Converted to `NA`. Appears in both state and district sheets. Adding `na` as an explicit alternative in the value token pattern recovered ~54 previously silently-dropped indicators per state.
- `(3.7)` — a valid estimate but flagged as having a small sample. Parentheses are stripped and the number is used. Appears only in district sheets.
- `1,090` — comma-formatted large numbers (sex ratios, out-of-pocket costs). Commas are stripped before numeric conversion.

*Wrapped indicator names.* Some indicator names are too long to fit on one PDF line and wrap onto the next. The name appears on line N with no values; the values appear on line N+1 with leading whitespace. The script handles this with a `pending_name` buffer: when a line has an indicator number but no values, it is stored as `pending_name`. The next line that contains values prepends `pending_name` automatically. The buffer also carries across page boundaries — an indicator name that wraps at the bottom of one page and whose values appear at the top of the next is handled correctly.

*Skipping non-indicator lines.* PDF pages contain many lines that are not indicator rows: section headings ("Maternity Care", "Child Vaccination"), column header rows ("Urban Rural Total Total"), footnote lines ("1 Piped water into dwelling..."), and round label rows ("NFHS-5 (2019-20)"). A dedicated `is_skip_line()` function identifies these. It uses a critical override: any line starting with an indicator number (`^\\d+\\.\\s+[A-Za-z]`) is never skipped, regardless of what words it contains. Without this override, a section heading keyword like "Blood Sugar" in the skip list would also kill indicators 99–110, which all contain that phrase in their names.

*Domain and direction.* Neither is encoded in the PDF. The script infers both by scanning each indicator name against a keyword lookup table (`indicator_meta`). The table is ordered from most specific to most general — `"blood sugar level"` before `"blood sugar"` before `"blood pressure"` — because matching stops at the first keyword found. Reversing the order would misclassify indicators. Any indicator that matches no keyword is flagged with `Domain = "Unknown"` and printed at the end of the run, making it easy to extend the table.

*Diagnostic block.* At the bottom of the script is a standalone diagnostic section. Run it after changing `DIAG_PDF` to any PDF you want to inspect. It prints every numbered indicator line, whether the regex matched it, and the two raw lines around each failure — showing exactly whether the problem is a leading space, a wrapped name, a special character, or something else.

---

### Step 3 — Cleaning shapefiles and exporting GeoJSON (`3_standardizing_names_and_creating_shapefiles.R`)

**What it does:** Reads state and district shapefiles, reconciles the geography names in those files against the `Geography` column in the CSV from Step 2, applies crosswalk corrections to fix mismatches, and writes GeoJSON files for the dashboard — one for all-India states, one for all-India districts, and one per state for its districts.

**Config to change before running:**
```r
path1 <- "G:/.shortcut-targets-by-id/.../maps and shapefiles"  # ← folder containing India_States.geojson and India_districts.json
path2 <- "G:/.shortcut-targets-by-id/.../NFHS all factsheets"  # ← folder containing nfhs_all_data.csv; GeoJSONs are written to path2/choropleths/
```

**Dependencies:** `sf`, `dplyr`

**Why name matching matters — and why it is hard.**

The choropleth map works by joining the dashboard's data CSV to the GeoJSON features using geography name as the key. A feature whose name in the GeoJSON does not exactly match the `Geography` column in the CSV will render grey — no data. There is no fuzzy matching; it is a strict string equality join.

The problem is that the shapefile was built from a different source (Survey of India / GADM boundaries) than the NFHS factsheets (IIPS). These two sources do not use the same spelling conventions. The same state or district may be called different things in each — and neither source is "wrong". The crosswalk is the manual reconciliation that makes the two sources agree.

**How the script identifies mismatches.**

After loading each shapefile, the script compares names against the CSV using two `setdiff` checks:

```r
setdiff(sheet_states, shp_states)   # in CSV but not shapefile → will render grey on the map
setdiff(shp_states, sheet_states)   # in shapefile but not CSV → will be ignored silently
```

Every name appearing in the first list is a geography the dashboard has data for but cannot colour on the map. Every name in the second list is a shapefile polygon that will never be matched. Both lists must be empty before the GeoJSON files are exported.

**State-level corrections.**

Four fixes were required:

- `"Andaman & Nicobar Island"` (shapefile) → `"Andaman & Nicobar Islands"` (NFHS spelling, with trailing 's')
- `"Dadara & Nagar Havelli"` and `"Daman & Diu"` both → `"Dadra & Nagar Haveli and Daman & Diu"`. The two UTs merged administratively in 2020 and NFHS-5 treats them as one geography; the shapefile still has two separate polygons. The script recodes both to the same name and then uses `st_union()` to merge their geometries into a single feature.
- `"NCT of Delhi"` (shapefile) → `"NCT Delhi"` (NFHS spelling)

**District-level corrections.**

57 district name mismatches were found and corrected via a named crosswalk vector. The types of mismatch encountered:

- *Spelling variants:* `"Ahmadabad"` → `"Ahmedabad"`, `"Darjiling"` → `"Darjeeling"`, `"Cooch Behar"` → `"Koch Bihar"`, `"Gurugram"` → `"Gurgaon"`. These differ because the shapefile and NFHS use different transliteration conventions for the same place name.
- *Truncated names from the PDF:* District names that were too long for the PDF column were cut off with an asterisk, e.g. `"North Twenty Four Pargan*"` → `"North Twenty Four Parganas"`, `"Sahibzada Ajit Singh Nag*"` → `"Sahibzada Ajit Singh Nagar"`, `"Sri Potti Sriramulu Nell*"` → `"Sri Potti Sriramulu Nellore"`.
- *Embedded characters:* `"Almora\n"` had a stray newline from PDF extraction → `"Almora"`.
- *Administrative renames:* `"Amroha"` → `"Jyotiba Phule Nagar"` (the district was renamed but each source used a different name). Similarly `"Kasganj"` → `"Kanshiram Nagar"`.
- *Merged geographies:* `"Karbi Anglong East"` and `"Karbi Anglong West"` (shapefile's split) → both mapped to existing NFHS district names.
- *Formatting differences:* `"Janjgir - Champa"` → `"Janjgir-Champa"` (space around hyphen), `"Kaimur (bhabua)"` → `"Kaimur (Bhabua)"` (capitalisation).

If you are rerunning this pipeline — for example after adding a new NFHS round or switching to a different shapefile source — re-run both `setdiff` checks after loading the new files. New mismatches will appear in the output and need to be added to the crosswalk before exporting.

**Output.**

The script writes three sets of GeoJSON files into `path2/choropleths/`:
- `India_States_fornfhsdashboard.geojson` — all-India state boundaries (36 features after merging)
- `India_Districts_fornfhsdashboard.geojson` — all-India district boundaries
- One file per state — e.g. `Telangana_fornfhsdashboard.geojson` — used for the single-state drill-down view

All GeoJSON files must be placed in the **same folder as `index.html`** before deploying. The dashboard fetches them via relative paths.

**A note on CRS and file size.** The CRS must be WGS84 (EPSG:4326, coordinates in degrees). D3.js cannot render projected coordinate systems where units are metres. File size can be significant at district level; if files exceed GitHub's 25MB per-file limit, simplify before writing:

```r
shapefile_district <- st_simplify(shapefile_district, preserveTopology = TRUE, dTolerance = 0.02)
st_write(shapefile_district, "output.geojson",
         layer_options = "COORDINATE_PRECISION=4", delete_dsn = TRUE)
```
`dTolerance = 0.02` degrees (~2 km) and 4 decimal places of coordinate precision are more than sufficient for a choropleth, with no visible quality loss.

---

### Step 4 — The dashboard (`index.html`)

**What it is:** A single self-contained HTML file — all five screens, all visualisation logic, all styling in one place. No build step, no Node, no bundler. Two CDN-loaded libraries: D3.js (choropleth maps) and Chart.js (line and bar charts). All other logic is vanilla JavaScript.

**Data loading.** On every page load, the dashboard fetches the output CSV from a published Google Sheets URL hardcoded in the `SHEET_URL` constant near the top of the script section. Updating the underlying data only requires editing the sheet — not redeploying the HTML. To publish the sheet: File → Share → Publish to web → select the tab → choose CSV → copy the URL into `SHEET_URL`. The GeoJSON files are fetched from relative paths in the same folder as `index.html`.

**How each screen works:**

- *Snapshot* — filters the in-memory data array for a selected indicator and round, finds the top and bottom performers at state and district level, and computes change from the previous round.
- *Temporal Trend* — groups data by geography and round, passes it to Chart.js as a multi-series line chart. At district level, parent state and national series are overlaid automatically.
- *Spatial Bar Chart* — sorts all geographies by value for the selected indicator, renders a horizontal bar chart via Chart.js, and draws a vertical benchmark line at the national or state average.
- *Choropleth Map* — loads the appropriate GeoJSON, joins dashboard data to features by the `Geography` name, colours each feature on a scale based on standard deviations from the mean, and renders via D3's `geoMercator` projection. Colour direction is set per indicator. Supports zoom and pan via D3's zoom behaviour.

**To update the data source:** change `SHEET_URL` at the top of the `<script>` section and save the file. No other changes are needed; no redeployment is required since the dashboard fetches data at runtime.

**To add a new indicator:** add rows to the Google Sheet, then check whether the indicator's domain and direction are already covered by the keyword table in `2__convert_district_state_factsheets_to_excel.R`. If not, add a new row to `indicator_meta` — more specific keywords above more general ones.

---

## Hosting

### Netlify

Netlify allows drag-and-drop deployment of static sites. Drop the entire folder (containing `index.html` and all GeoJSON files) onto the deploy zone at [app.netlify.com](https://app.netlify.com).

**Important nuances:**
- Always drag the **folder**, not individual files. Dragging just the HTML replaces the entire site and deletes the GeoJSONs.
- The HTML file must be named `index.html` to be served at the root URL. Any other name works but requires the full path in the URL.
- GeoJSON files must be in the **same folder** as `index.html` — the dashboard fetches them using relative paths.
- **Deploy limits.** Netlify's free tier allows 500 deployments per month. Each drag-and-drop counts as one deploy. This is rarely a constraint in practice, but be aware that Netlify also has a separate AI agent product with its own credit limit — exhausting those credits can pause your entire project including static hosting. If the dashboard goes down unexpectedly, check the Netlify dashboard for credit or billing issues before assuming a code problem.
- Redeploying after a data update is **not necessary** — the dashboard fetches data from Google Sheets at runtime. Only redeploy when `index.html` or the GeoJSON files change.

### GitHub Pages

GitHub Pages serves static files directly from a GitHub repository.

**Setup:**
1. Create a public repository
2. Upload all files (`index.html` + all GeoJSON files) to the repository root
3. Go to Settings → Pages → set source to the `main` branch, root folder
4. The site is live at `https://username.github.io/repository-name/`

**Important nuances:**
- The HTML file **must** be named exactly `index.html` — GitHub Pages will show a 404 for any other filename at the root URL
- All files must be at the **repository root**, not inside a subfolder, for the root URL to work
- GitHub has a **25MB file size limit per file** — large GeoJSON files (especially district-level) may need simplification before upload (see Step 3 above)
- GitHub Pages has a build pipeline (Jekyll) that runs on every commit, adding 1–3 minutes before changes go live. This is slower than Netlify's near-instant deploys.
- The browser and GitHub Pages both cache files aggressively. After uploading a new version, wait for the Actions workflow to show a green tick before testing — and do a hard refresh (`Ctrl+Shift+R`) or test in an incognito window to bypass browser cache.

---

## Repository Structure

```
/
├── index.html                               # Dashboard (all screens, all JS/CSS)
├── India_States2_fornfhsdashboard.geojson   # State boundaries
├── India_Districts_fornfhsdashboard.geojson  # District boundaries (all India)
├── Andhra Pradesh_fornfhsdashboard.geojson   # State-level district boundaries
├── Telangana_fornfhsdashboard.geojson        # (one file per state)
├── ...
└── README.md
```

The R scripts (`1__scrape_state_district_factsheets_from_dhs_website.R`, `2__convert_district_state_factsheets_to_excel.R`, `3_standardizing_names_and_creating_shapefiles.R`) are version-controlled separately and are not required to run the dashboard. PDFs are not stored in this repository — they are available on request, and can also be downloaded directly from [IIPS](http://rchiips.org/nfhs/nfhs5.shtml) using Script 1.

---

## Data Sources

- **NFHS-5 (2019–21)** and **NFHS-4 (2015–16)** factsheets: [IIPS](http://rchiips.org/nfhs/)
- Ministry of Health and Family Welfare, Government of India
