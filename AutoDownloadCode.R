# =====================================================================
#  USCRN  (daily01)  →  STREAM ⭢ SUBSET ⭢ CSV
#  Variables: near-surface air temperature (max/min/mean)
# =====================================================================

# ---------------- USER SETTINGS --------------------------------------
year <- 2015             # choose any available year dir (2000 – present)

vars_wanted <- c(        # fields to keep (change as needed)
  "LONGITUDE", "LATITUDE",
  "T_DAILY_MAX", "T_DAILY_MIN", "T_DAILY_MEAN"
)

out_csv  <- sprintf("C:/Users/User/Downloads/RH Research 2/USCRN_%d_airtemp.csv", year)
# ---------------------------------------------------------------------

# ---------------- LIBRARIES ------------------------------------------
# install.packages(c("rvest","readr","dplyr","purrr","stringr","fs"))
library(rvest)    # scrape directory listing
library(readr)    # read_table(), write_csv()
library(dplyr)    # tidy helpers
library(purrr)    # map_dfr
library(stringr)  # string helpers
library(fs)       # path helpers
# ---------------------------------------------------------------------

## 1 ▸ get official field names ---------------------------------------
hdr_url  <- "https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/headers.txt"
header_vec <- read_lines(hdr_url, n_max = 2)[2] |>
  str_split("\\s+") |> unlist() |> keep(nzchar)

stopifnot(length(header_vec) == 28)   # sanity

## 2 ▸ scrape yearly directory for .txt files -------------------------
dir_url <- sprintf(
  "https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/%d/", year
)

file_urls <- read_html(dir_url) |>
  html_elements("a") |>
  html_attr("href") |>
  keep(~ str_detect(.x, "\\.txt$")) |>
  unique() |>
  sort() |>
  paste0(dir_url, _)

if (!length(file_urls))
  stop("No station files found at ", dir_url)

## 3 ▸ helper to read one file directly from web ----------------------
read_one <- function(url) {
  read_table(
    url,
    col_names      = FALSE,
    col_types      = cols(.default = col_character()),
    na             = c("-9999","-9999.0","-99.000","NA",""),
    progress       = FALSE,
    show_col_types = FALSE
  ) |>
    mutate(source_file = path_file(url))
}

## 4 ▸ stream & combine (≈ 1–2 min for 2015) --------------------------
uscrn_raw <- map_dfr(file_urls, read_one)

names(uscrn_raw) <- c(header_vec, "source_file")

uscrn <- type_convert(
  uscrn_raw,
  na = c("-9999","-9999.0","-99.000","NA","")
)

## 5 ▸ parse date → clean `DATE` column -------------------------------
uscrn <- uscrn |>
  mutate(
    DATE = as.Date(sprintf("%08d", as.integer(LST_DATE)), "%Y%m%d")
  )

## 6 ▸ subset to requested variables ----------------------------------
final_df <- uscrn |>
  select(DATE, source_file, all_of(vars_wanted))

## 7 ▸ export ----------------------------------------------------------
write_csv(final_df, out_csv)
cat("✅  CSV written:", out_csv,
    "\n   Rows:", nrow(final_df),
    " Cols:", ncol(final_df), "\n")

