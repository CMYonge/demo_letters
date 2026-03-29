# STEP3_convert_tags_to_links.R
# Converts [[person:123]] style tags to proper markdown links

library(stringr)
library(dplyr)
library(readr)

website_dir <- "C:/db/cmy_letters_website"

cat("Converting internal tags to working links...\n\n")

# ============================================================
# LOAD LOOKUP DATA
# ============================================================

# We need to map IDs to names for link text
persons <- read_csv("C:/db/persons.csv")
others <- read_csv("C:/db/wp_others.csv")
cmy_bib <- read_csv("C:/db/wp_cmybibliography.csv")
gen_bib <- read_csv("C:/db/wp_generalbibliography.csv")

cat("✓ Loaded lookup tables\n\n")

# ============================================================
# FUNCTION TO CONVERT TAGS IN A FILE
# ============================================================

convert_tags_in_file <- function(filepath, file_location) {
  
  content <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  content <- paste(content, collapse = "\n")
  
  original_content <- content
  
  # Determine relative path based on where this file lives
  # file_location is one of: "letters", "people", "organizations", "cmy_books", "other_books"
  
  if (file_location == "letters") {
    person_path <- "../people/person_"
    org_path <- "../organizations/other_"
    cmy_path <- "../cmy_books/cmybook_"
    book_path <- "../other_books/otherbook_"
  } else if (file_location == "people") {
    person_path <- "person_"
    org_path <- "../organizations/other_"
    cmy_path <- "../cmy_books/cmybook_"
    book_path <- "../other_books/otherbook_"
  } else if (file_location == "organizations") {
    person_path <- "../people/person_"
    org_path <- "other_"
    cmy_path <- "../cmy_books/cmybook_"
    book_path <- "../other_books/otherbook_"
  } else if (file_location == "cmy_books") {
    person_path <- "../people/person_"
    org_path <- "../organizations/other_"
    cmy_path <- "cmybook_"
    book_path <- "../other_books/otherbook_"
  } else if (file_location == "other_books") {
    person_path <- "../people/person_"
    org_path <- "../organizations/other_"
    cmy_path <- "../cmy_books/cmybook_"
    book_path <- "otherbook_"
  }
  
  # Convert [[person:123]] tags
  person_tags <- str_match_all(content, "\\[\\[person:(\\d+)\\]\\]")[[1]]
  if (nrow(person_tags) > 0) {
    for (i in 1:nrow(person_tags)) {
      tag <- person_tags[i, 1]
      id <- as.numeric(person_tags[i, 2])
      
      # Look up person name
      person_row <- persons %>% filter(ID == id)
      if (nrow(person_row) > 0) {
        name <- person_row$post_title[1]
        # Convert to markdown link
        link <- paste0("[", name, "](", person_path, id, ".qmd)")
        content <- str_replace(content, fixed(tag), link)
      }
    }
  }
  
  # Convert [[other:123]] tags
  other_tags <- str_match_all(content, "\\[\\[other:(\\d+)\\]\\]")[[1]]
  if (nrow(other_tags) > 0) {
    for (i in 1:nrow(other_tags)) {
      tag <- other_tags[i, 1]
      id <- as.numeric(other_tags[i, 2])
      
      # Look up org name
      org_row <- others %>% filter(ID == id)
      if (nrow(org_row) > 0) {
        name <- org_row$post_title[1]
        link <- paste0("[", name, "](", org_path, id, ".qmd)")
        content <- str_replace(content, fixed(tag), link)
      }
    }
  }
  
  # Convert [[cmybook:123]] tags
  cmy_tags <- str_match_all(content, "\\[\\[cmybook:(\\d+)\\]\\]")[[1]]
  if (nrow(cmy_tags) > 0) {
    for (i in 1:nrow(cmy_tags)) {
      tag <- cmy_tags[i, 1]
      id <- as.numeric(cmy_tags[i, 2])
      
      # Look up book title
      book_row <- cmy_bib %>% filter(ID == id)
      if (nrow(book_row) > 0) {
        title <- book_row$post_title[1]
        link <- paste0("[", title, "](", cmy_path, id, ".qmd)")
        content <- str_replace(content, fixed(tag), link)
      }
    }
  }
  
  # Convert [[otherbook:123]] tags
  book_tags <- str_match_all(content, "\\[\\[otherbook:(\\d+)\\]\\]")[[1]]
  if (nrow(book_tags) > 0) {
    for (i in 1:nrow(book_tags)) {
      tag <- book_tags[i, 1]
      id <- as.numeric(book_tags[i, 2])
      
      # Look up book title
      book_row <- gen_bib %>% filter(ID == id)
      if (nrow(book_row) > 0) {
        title <- book_row$post_title[1]
        link <- paste0("[", title, "](", book_path, id, ".qmd)")
        content <- str_replace(content, fixed(tag), link)
      }
    }
  }
  
  # Only write if content changed
  if (content != original_content) {
    writeLines(content, filepath, useBytes = TRUE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# ============================================================
# PROCESS ALL FILES
# ============================================================

# Process letters
cat("Processing letters...\n")
letter_files <- list.files(file.path(website_dir, "letters"), 
                           pattern = "\\.md$", full.names = TRUE)
letters_converted <- sum(sapply(letter_files, function(f) convert_tags_in_file(f, "letters")))
cat("✓ Converted tags in", letters_converted, "letters\n\n")

# Process people pages
cat("Processing people pages...\n")
people_files <- list.files(file.path(website_dir, "people"), 
                           pattern = "\\.md$", full.names = TRUE)
people_converted <- sum(sapply(people_files, function(f) convert_tags_in_file(f, "people")))
cat("✓ Converted tags in", people_converted, "people pages\n\n")

# Process organization pages
cat("Processing organization pages...\n")
org_files <- list.files(file.path(website_dir, "organizations"), 
                        pattern = "\\.md$", full.names = TRUE)
org_converted <- sum(sapply(org_files, function(f) convert_tags_in_file(f, "organizations")))
cat("✓ Converted tags in", org_converted, "organization pages\n\n")

# Process CMY book pages
cat("Processing CMY book pages...\n")
cmy_files <- list.files(file.path(website_dir, "cmy_books"), 
                        pattern = "\\.md$", full.names = TRUE)
cmy_converted <- sum(sapply(cmy_files, function(f) convert_tags_in_file(f, "cmy_books")))
cat("✓ Converted tags in", cmy_converted, "CMY book pages\n\n")

# Process other book pages
cat("Processing other book pages...\n")
book_files <- list.files(file.path(website_dir, "other_books"), 
                         pattern = "\\.md$", full.names = TRUE)
book_converted <- sum(sapply(book_files, function(f) convert_tags_in_file(f, "other_books")))
cat("✓ Converted tags in", book_converted, "other book pages\n\n")

cat("✅ All tags converted to working links!\n\n")
cat("Total files modified:\n")
cat("  Letters:", letters_converted, "\n")
cat("  People:", people_converted, "\n")
cat("  Organizations:", org_converted, "\n")
cat("  CMY Books:", cmy_converted, "\n")
cat("  Other Books:", book_converted, "\n\n")

cat("Next step: Render the website!\n")
cat("In RStudio terminal, navigate to", website_dir, "\n")
cat("Then run: quarto render\n")