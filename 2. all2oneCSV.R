# ---------------- PACKAGES ----------------
# install.packages(c("readr", "dplyr", "purrr", "fs"))
library(readr)
library(dplyr)
library(purrr)
library(fs)

# -------------- USER PATHS ----------------
input_folder <- "C:/Users/User/Downloads/RH Research 2/downloaded_txt_files"
output_csv   <- "C:/Users/User/Downloads/RH Research 2/uscrn_daily01_2015_combined.csv"

# 1) List *.txt files ----------------------------------------------------------
file_list <- dir_ls(input_folder, glob = "*.txt")
if (!length(file_list)) stop("No .txt files found in: ", input_folder)

# 2) Helper to read ONE file (everything as character) -------------------------
read_one <- function(file) {
  read_table(
    file,
    col_names      = FALSE,                       # no header row in USCRN txt
    col_types      = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    mutate(source_file = path_file(file))         # provenance column
}

# 3) Stack them safely ---------------------------------------------------------
char_data <- map_dfr(file_list, read_one)

# 4) Now re-infer column types in *one* pass -----------------------------------
#    • We’ll also turn -9999, -99, etc. into proper NA.
#    • Use `na = ...` to list *all* USCRN missing codes you care about.
all_data <- type_convert(
  char_data,
  na = c("", "NA", "-9999", "-9999.0", "-99.000")
)

# 5) Write the combined CSV ----------------------------------------------------
write_csv(all_data, output_csv)
cat("✅  Combined file saved to:\n   ", output_csv, "\n")
