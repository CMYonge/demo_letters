# STEP2_copy_files_to_website.R
# Copies converted markdown letters and reference pages into website structure

library(here)

# Source directories (your existing converted files)
source_letters <- "C:/db/test_49_markdown_COMPLETE"
source_base <- "C:/db"  # Base directory where individual folders exist

# Destination (the website we just created)
website_dir <- "C:/db/cmy_letters_website"

cat("Starting file copy...\n\n")

# ============================================================
# COPY LETTERS
# ============================================================

cat("Copying letters...\n")

# Get all letter files
letter_files <- list.files(source_letters, pattern = "\\.md$", full.names = TRUE)

if (length(letter_files) == 0) {
  stop("No letter files found in ", source_letters)
}

# Copy each letter to website/letters/
copied_letters <- 0
for (letter_file in letter_files) {
  file_name <- basename(letter_file)
  dest_file <- file.path(website_dir, "letters", file_name)
  
  file.copy(letter_file, dest_file, overwrite = TRUE)
  copied_letters <- copied_letters + 1
}

cat("✓ Copied", copied_letters, "letters to letters/\n\n")

# ============================================================
# COPY REFERENCE PAGES (from individual folders)
# ============================================================

cat("Copying reference pages...\n")

# Copy people pages
people_source <- file.path(source_base, "people")
if (dir.exists(people_source)) {
  people_files <- list.files(people_source, pattern = "\\.md$", full.names = TRUE)
  for (person_file in people_files) {
    file_name <- basename(person_file)
    dest_file <- file.path(website_dir, "people", file_name)
    file.copy(person_file, dest_file, overwrite = TRUE)
  }
  cat("✓ Copied", length(people_files), "people pages\n")
} else {
  cat("⚠ No people/ directory found at", people_source, "\n")
}

# Copy organization pages
org_source <- file.path(source_base, "organizations")
if (dir.exists(org_source)) {
  org_files <- list.files(org_source, pattern = "\\.md$", full.names = TRUE)
  for (org_file in org_files) {
    file_name <- basename(org_file)
    dest_file <- file.path(website_dir, "organizations", file_name)
    file.copy(org_file, dest_file, overwrite = TRUE)
  }
  cat("✓ Copied", length(org_files), "organization pages\n")
} else {
  cat("⚠ No organizations/ directory found at", org_source, "\n")
}

# Copy CMY books pages
cmy_source <- file.path(source_base, "cmy_books")
if (dir.exists(cmy_source)) {
  cmy_files <- list.files(cmy_source, pattern = "\\.md$", full.names = TRUE)
  for (cmy_file in cmy_files) {
    file_name <- basename(cmy_file)
    dest_file <- file.path(website_dir, "cmy_books", file_name)
    file.copy(cmy_file, dest_file, overwrite = TRUE)
  }
  cat("✓ Copied", length(cmy_files), "CMY book pages\n")
} else {
  cat("⚠ No cmy_books/ directory found at", cmy_source, "\n")
}

# Copy other books pages
books_source <- file.path(source_base, "other_books")
if (dir.exists(books_source)) {
  books_files <- list.files(books_source, pattern = "\\.md$", full.names = TRUE)
  for (book_file in books_files) {
    file_name <- basename(book_file)
    dest_file <- file.path(website_dir, "other_books", file_name)
    file.copy(book_file, dest_file, overwrite = TRUE)
  }
  cat("✓ Copied", length(books_files), "other book pages\n")
} else {
  cat("⚠ No other_books/ directory found at", books_source, "\n")
}

cat("\n✅ File copy complete!\n\n")

# ============================================================
# SUMMARY
# ============================================================

cat("Summary of website contents:\n")
cat("Letters:        ", length(list.files(file.path(website_dir, "letters"), pattern = "\\.md$")), "files\n")
cat("People:         ", length(list.files(file.path(website_dir, "people"), pattern = "\\.md$")), "files\n")
cat("Organizations:  ", length(list.files(file.path(website_dir, "organizations"), pattern = "\\.md$")), "files\n")
cat("CMY Books:      ", length(list.files(file.path(website_dir, "cmy_books"), pattern = "\\.md$")), "files\n")
cat("Other Books:    ", length(list.files(file.path(website_dir, "other_books"), pattern = "\\.md$")), "files\n")

cat("\nWebsite location:", website_dir, "\n")
cat("\nNext step: Convert [[tags]] to proper links (STEP 3)\n")
