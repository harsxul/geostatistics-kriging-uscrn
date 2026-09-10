# --- Load required package ---
# Install first if needed: install.packages("stringr")
library(stringr) # Using stringr for easier filename extraction

# --- Required: Set these variables ---

# 1. Path to your file (it's not really a standard CSV)
url_list_file_path <- "C:/Users/User/Downloads/RH Research 2/www.ncei.noaa.gov_5th_May_2025.csv"

# 2. Number of lines to skip at the beginning
lines_to_skip <- 5

# 3. Folder where you want to save the downloaded TXT files
output_folder <- "downloaded_txt_files"

# --- End of required variables ---


# --- Script ---

# Create the output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  if(dir.exists(output_folder)) {
    cat("Created output folder:", output_folder, "\n")
  } else {
    stop("Could not create output folder:", output_folder, ". Please check permissions or path.")
  }
} else {
  cat("Output folder already exists:", output_folder, "\n")
}


# Read all lines from the file
tryCatch({
  all_lines <- readLines(url_list_file_path)
}, error = function(e) {
  stop("Error reading file '", url_list_file_path, "': ", e$message)
})

# Check if enough lines exist to skip
if(length(all_lines) <= lines_to_skip) {
  stop("Error: File has ", length(all_lines), " lines, but script is set to skip ", lines_to_skip, " lines.")
}

# Keep lines AFTER the skipped lines
urls_to_download <- all_lines[(lines_to_skip + 1):length(all_lines)]

# Filter out any potentially empty lines
urls_to_download <- urls_to_download[urls_to_download != ""]

# Filter to keep only lines ending with .txt (basic check for valid file URLs)
urls_to_download <- urls_to_download[endsWith(urls_to_download, ".txt")]

cat("Found", length(urls_to_download), ".txt URLs after skipping", lines_to_skip, "lines.\n")

# Loop through URLs and download
successful_downloads <- 0
failed_downloads <- 0

for (file_url in urls_to_download) {
  # Trim whitespace just in case
  file_url <- trimws(file_url)
  
  if (file_url == "" || !endsWith(file_url, ".txt")) {
    cat("Skipping invalid entry:", file_url, "\n")
    next
  }
  
  # Extract filename from the end of the URL
  # Using stringr::str_extract or base R's basename
  # filename <- stringr::str_extract(file_url, "[^/]+$") # Gets text after last '/'
  filename <- basename(file_url) # Base R equivalent
  
  if(is.na(filename) || filename == "") {
    cat("!!! Could not extract filename from URL:", file_url, "\n")
    failed_downloads <- failed_downloads + 1
    next
  }
  
  # Construct the full path to save the file locally
  save_path <- file.path(output_folder, filename)
  
  cat("Attempting download:", file_url, "to", save_path, "\n")
  
  # Use tryCatch to handle download errors
  download_status <- tryCatch({
    download.file(url = file_url, destfile = save_path, mode = "wb", quiet = TRUE, timeout = 60)
    TRUE # Return TRUE on success
  }, warning = function(w) {
    cat("!!! Warning downloading", filename, ":", w$message, "\n")
    FALSE # Indicate failure
  }, error = function(e) {
    cat("!!! ERROR downloading", filename, ":", e$message, "\n")
    FALSE # Indicate failure
  })
  
  # Check if download was successful
  if (download_status && file.exists(save_path) && file.info(save_path)$size > 0) {
    cat("   Successfully downloaded:", filename, "\n")
    successful_downloads <- successful_downloads + 1
  } else if (download_status && file.exists(save_path) && file.info(save_path)$size == 0) {
    cat("!!! Warning: Downloaded file is empty:", filename, "\n")
    failed_downloads <- failed_downloads + 1
  } else {
    failed_downloads <- failed_downloads + 1
    # Clean up potentially empty file created by failed download
    if (file.exists(save_path)) {
      file.remove(save_path)
    }
  }
}

cat("\n--- Download Summary ---\n")
cat("Successful:", successful_downloads, "\n")
cat("Failed:", failed_downloads, "\n")
cat("------------------------\n")


