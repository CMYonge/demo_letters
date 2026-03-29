# STEP1_convert_letters_to_qmd_FINAL.R
# Converts HTML letters with [[person:123]text] tags to clean [[person:123]] format

library(dplyr)
library(readr)
library(stringr)

# Input/Output directories
html_dir <- "C:/db/test_50_letters/html_extracted"
output_dir <- "C:/db/test_49_letters_qmd_v2"

# Load metadata
letterinfo <- read_csv("C:/db/wp_letterinfo.csv", show_col_types = FALSE)

# Create output directory
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get list of HTML files
html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)

cat("Found", length(html_files), "HTML letters to convert\n\n")

# ============================================================
# FUNCTION TO CONVERT ONE LETTER
# ============================================================

convert_letter <- function(html_file) {
  
  # Read HTML as plain text
  body_html <- readLines(html_file, warn = FALSE, encoding = "UTF-8")
  body_html <- paste(body_html, collapse = "\n")
  
  # Extract post_id from filename
  filename <- basename(html_file)
  post_id <- as.numeric(str_extract(filename, "^\\d+"))
  
  if (is.na(post_id)) {
    cat("⚠ Could not extract post_id from", filename, "\n")
    return(NULL)
  }
  
  # Remove <body> and </body> tags
  body_html <- gsub("<body>|</body>", "", body_html)
  
  # ============================================================
  # FIX TAGS: Remove display text from [[type:id]text] format
  # ============================================================
  
  # Fix person tags: [[person:123]any text] -> [[person:123]]
  body_html <- gsub('\\[\\[person:(\\d+)\\][^\\]]*\\]', '[[person:\\1]]', body_html, perl = TRUE)
  
  # Fix other tags: [[other:456]any text] -> [[other:456]]
  body_html <- gsub('\\[\\[other:(\\d+)\\][^\\]]*\\]', '[[other:\\1]]', body_html, perl = TRUE)
  
  # Fix cmybook tags: [[cmybook:789]any text] -> [[cmybook:789]]
  body_html <- gsub('\\[\\[cmybook:(\\d+)\\][^\\]]*\\]', '[[cmybook:\\1]]', body_html, perl = TRUE)
  
  # Fix otherbook tags: [[otherbook:101]any text] -> [[otherbook:101]]
  body_html <- gsub('\\[\\[otherbook:(\\d+)\\][^\\]]*\\]', '[[otherbook:\\1]]', body_html, perl = TRUE)
  
  # ============================================================
  # FIX FOOTNOTES: [[footnote:1]] -> ^[footnote text]
  # ============================================================
  
  # Note: Your footnotes seem to already be in [[footnote:1]] format
  # We'll leave them as-is for now - can be converted later if needed
  
  # ============================================================
  # CONVERT HTML TO MARKDOWN
  # ============================================================
  
  # Convert common HTML elements
  body_html <- gsub("<p>", "\n\n", body_html)
  body_html <- gsub("</p>", "", body_html)
  body_html <- gsub("<br\\s*/?>", "  \n", body_html, perl = TRUE)
  body_html <- gsub("<em>", "*", body_html)
  body_html <- gsub("</em>", "*", body_html)
  body_html <- gsub("<i>", "*", body_html)
  body_html <- gsub("</i>", "*", body_html)
  body_html <- gsub("<strong>", "**", body_html)
  body_html <- gsub("</strong>", "**", body_html)
  body_html <- gsub("<b>", "**", body_html)
  body_html <- gsub("</b>", "**", body_html)
  
  # Remove remaining HTML tags
  body_html <- gsub("<[^>]+>", "", body_html)
  
  # Clean up excessive whitespace
  body_html <- gsub("\n\n\n+", "\n\n", body_html)
  body_html <- trimws(body_html)
  
  # ============================================================
  # GET METADATA
  # ============================================================
  
  meta <- letterinfo %>% filter(post_ID == post_id)
  
  if (nrow(meta) == 0) {
    title <- paste("Letter", post_id)
    date <- ""
    location <- ""
    manuscript <- ""
  } else {
    title <- filename
    date <- ifelse(is.na(meta$letter_date[1]) || meta$letter_date[1] == "NA", 
                   "", meta$letter_date[1])
    location <- ifelse(is.na(meta$letter_fromAddress[1]) || meta$letter_fromAddress[1] == "NA", 
                       "", meta$letter_fromAddress[1])
    manuscript <- ifelse(is.na(meta$manuscript_location[1]) || meta$manuscript_location[1] == "NA", 
                         "", meta$manuscript_location[1])
  }
  
  # ============================================================
  # CREATE YAML HEADER
  # ============================================================
  
  yaml_header <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    ifelse(date != "", paste0("date: \"", date, "\"\n"), ""),
    ifelse(location != "", paste0("location: \"", location, "\"\n"), ""),
    ifelse(manuscript != "", paste0("manuscript: \"", manuscript, "\"\n"), ""),
    "post_id: ", post_id, "\n",
    "---\n\n"
  )
  
  # ============================================================
  # COMBINE AND SAVE
  # ============================================================
  
  full_content <- paste0(yaml_header, body_html)
  
  output_file <- file.path(output_dir, paste0("letter_", post_id, ".qmd"))
  writeLines(full_content, output_file, useBytes = TRUE)
  
  return(post_id)
}

# ============================================================
# PROCESS ALL LETTERS
# ============================================================

converted_count <- 0
for (html_file in html_files) {
  result <- tryCatch({
    convert_letter(html_file)
    converted_count <- converted_count + 1
    TRUE
  }, error = function(e) {
    cat("✗ Error converting", basename(html_file), ":", e$message, "\n")
    FALSE
  })
}

cat("\n✅ Conversion complete!\n")
cat("Converted", converted_count, "letters\n")
cat("Output directory:", output_dir, "\n")
cat("\nSample check: Open a letter and verify tags are now [[person:123]] format\n")
cat("\nNext: Run STEP 2 to create reference pages\n")