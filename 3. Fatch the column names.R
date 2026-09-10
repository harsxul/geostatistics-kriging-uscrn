library(readr)
library(dplyr)

# ---- 1. Pull the header names automatically ------------------------------
header_url  <- "https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/headers.txt"

header_vec <- read_lines(header_url, n_max = 2)[2] |>          # take SECOND line
  strsplit("\\s+") |>                                          # split on any spaces/tabs
  unlist() |> 
  keep(nzchar)                                                 # drop empty tokens

length(header_vec)   # should print 28
print(header_vec)



## Replace the names with our file

# -- 1. Read the combined file you built earlier -----------------------------
uscrn <- read_csv(
  "C:/Users/User/Downloads/RH Research 2/uscrn_daily01_2015_combined.csv",
  show_col_types = FALSE
)

# -- 2. Apply the official NOAA field names + our provenance column ----------
names(uscrn) <- c(header_vec, "source_file")

# -- 3. Parse the date and glimpse the result --------------------------------
uscrn <- uscrn %>%
  mutate(LST_DATE = as.Date(LST_DATE, format = "%Y%m%d")) %>%
  arrange(source_file, LST_DATE)

glimpse(uscrn, width = 80)


## Select variables - i.e.  "RH_DAILY_MAX", "RH_DAILY_MIN", "RH_DAILY_AVG"  


# 1. Select only the variables you care about
rh_only <- uscrn %>%
  select(
    LST_DATE,          # keep the calendar date for context
    source_file,       # so you still know which station the row came from
    RH_DAILY_MAX,
    RH_DAILY_MIN,
    RH_DAILY_AVG
  )

# quick peek
glimpse(rh_only, width = 80)

# 2. Write to disk (optional)
write_csv(rh_only,
          "C:/Users/User/Downloads/RH Research 2/uscrn_2015_RH.csv")

cat("✅  Saved: uscrn_2015_RH_csv\n")



## Lets import the units too

library(readr)
library(dplyr)
library(stringr)

# ---- read the readme you just uploaded ---------------------------------
readme_path <- "C:/Users/User/Downloads/RH Research 2/readme.txt"   # adjust if needed
txt <- read_lines(readme_path)

# ---- find the summary table header line --------------------------------
start <- which(str_detect(txt, "^Field#\\s+Name"))        # header
stop  <- which(str_detect(txt, "^\\s*1\\s+WBANNO")) + 28  # first data line + 28 rows

table_lines <- txt[start + 2:29]   # 28 data lines (skip the header + separator)

# ---- split each line on ≥2 spaces, discard blanks -----------------------
field_df <- str_split_fixed(table_lines, "\\s{2,}", 3) %>%
  as_tibble(.name_repair = ~c("Field#", "Name", "Units")) %>%
  mutate(`Field#` = as.integer(`Field#`))

# quick peek
print(field_df)


# # # Attach the metadata to subset
rh_geo <- uscrn %>%                      # uscrn already has proper col names
  select(LST_DATE, source_file,
         LONGITUDE, LATITUDE,
         RH_DAILY_MAX, RH_DAILY_MIN, RH_DAILY_AVG)

# -- join variable info for a quick “data dictionary” --------------------
var_info <- field_df %>%
  filter(Name %in% names(rh_geo))

print(var_info)

# store the metadata alongside the data
attr(rh_geo, "variable_info") <- var_info


install.packages("writexl")   # run once if not yet installed
library(writexl)

out_path <- "C:/Users/User/Downloads/RH Research 2/uscrn_2015_RH_geo_with_meta.xlsx"

write_xlsx(
  list(
    USCRN_RH      = rh_geo,   # sheet 1: your five requested variables
    Variable_Info = var_info  # sheet 2: field #, name, units
  ),
  path = out_path
)

cat("✅  Exported:", out_path, "\n")

# fix
rh_geo_fix <- rh_geo %>%                             # your 7-column data frame
  mutate(LST_DATE = format(LST_DATE, "%Y-%m-%d"))     # force to character

write_xlsx(
  list(
    USCRN_RH      = rh_geo_fix,
    Variable_Info = var_info
  ),
  path = "C:/Users/User/Downloads/RH Research 2/uscrn_2015_RH_geo_with_meta2.xlsx"
)


