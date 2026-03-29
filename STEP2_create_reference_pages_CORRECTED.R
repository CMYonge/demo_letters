# STEP2_create_reference_pages_v2.R
# Creates reference pages for people, organizations, and bibliography

library(dplyr)
library(readr)
library(stringr)

# Output directory
output_dir <- "C:/db/test_49_reference_pages_qmd_v2"

# Create subdirectories
dir.create(file.path(output_dir, "people"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "organizations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "cmy_books"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "other_books"), showWarnings = FALSE, recursive = TRUE)

# Load data
persons <- read_csv("C:/db/persons.csv", show_col_types = FALSE)
others <- read_csv("C:/db/wp_others.csv", show_col_types = FALSE)
cmy_bib <- read_csv("C:/db/wp_cmybibliography.csv", show_col_types = FALSE)
gen_bib <- read_csv("C:/db/wp_generalbibliography.csv", show_col_types = FALSE)

# Load linking tables
person_links <- read_csv("C:/db/wp_persons_posts.csv", show_col_types = FALSE)
other_links <- read_csv("C:/db/wp_others_posts.csv", show_col_types = FALSE)
cmy_links <- read_csv("C:/db/wp_cmybibliography_posts.csv", show_col_types = FALSE)
gen_links <- read_csv("C:/db/wp_generalbibliography_posts.csv", show_col_types = FALSE)

# Load letter info - UPDATED to v2 folder
letter_dir <- "C:/db/test_49_letters_qmd_v2"
letter_files <- list.files(letter_dir, pattern = "\\.qmd$", full.names = TRUE)

# Get list of letter post_ids we're working with (our 49 letters)
letter_ids <- as.numeric(str_extract(basename(letter_files), "\\d+"))

cat("Creating reference pages for", length(letter_ids), "letters\n\n")

# ============================================================
# CREATE PEOPLE PAGES
# ============================================================

cat("Creating people pages...\n")

people_count <- 0
for (i in 1:nrow(persons)) {
  person_id <- persons$persons_id[i]
  
  # Build name from components
  name_parts <- c(
    persons$prefix[i],
    persons$first_name[i],
    persons$maiden_name[i],
    persons$surname[i],
    persons$suffix[i]
  )
  name_parts <- name_parts[!is.na(name_parts)]
  name <- paste(name_parts, collapse = " ")
  
  if (name == "") {
    name <- paste("Person", person_id)
  }
  
  # Get biographical info
  bio_parts <- c()
  if (!is.na(persons$title[i]) && persons$title[i] != "") {
    bio_parts <- c(bio_parts, persons$title[i])
  }
  if (!is.na(persons$dates[i]) && persons$dates[i] != "") {
    bio_parts <- c(bio_parts, persons$dates[i])
  }
  if (!is.na(persons$description[i]) && persons$description[i] != "") {
    bio_parts <- c(bio_parts, persons$description[i])
  }
  if (!is.na(persons$biography[i]) && persons$biography[i] != "") {
    bio_parts <- c(bio_parts, persons$biography[i])
  }
  
  bio <- paste(bio_parts, collapse = "\n\n")
  
  # Find which of our 49 letters mention this person
  mentions <- person_links %>% 
    filter(persons_id == person_id, post_id %in% letter_ids)
  
  # Create YAML header
  content <- paste0(
    "---\n",
    "title: \"", name, "\"\n",
    "person_id: ", person_id, "\n",
    "---\n\n"
  )
  
  # Add biographical info if available
  if (bio != "") {
    content <- paste0(content, "## Biographical Information\n\n", bio, "\n\n")
  }
  
  # Add list of letters mentioning this person
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This Person\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_id[j]
      content <- paste0(content, "- [Letter ", letter_id, "](../letters/letter_", letter_id, ".qmd)\n")
    }
  }
  
  # Write file
  output_file <- file.path(output_dir, "people", paste0("person_", person_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  
  people_count <- people_count + 1
}

cat("✓ Created", people_count, "people pages\n\n")

# ============================================================
# CREATE ORGANIZATION PAGES
# ============================================================

cat("Creating organization pages...\n")

org_count <- 0
for (i in 1:nrow(others)) {
  other_id <- others$others_id[i]
  name <- others$name[i]
  
  # Get description
  description <- ifelse(is.na(others$description[i]) || others$description[i] == "", 
                        "", others$description[i])
  
  # Get type
  type <- ifelse(is.na(others$type[i]) || others$type[i] == "", 
                 "", others$type[i])
  
  # Find which of our 49 letters mention this organization
  mentions <- other_links %>% 
    filter(others_id == other_id, post_id %in% letter_ids)
  
  # Create YAML header
  content <- paste0(
    "---\n",
    "title: \"", name, "\"\n",
    "other_id: ", other_id, "\n",
    "---\n\n"
  )
  
  # Add type if available
  if (type != "") {
    content <- paste0(content, "**Type:** ", type, "\n\n")
  }
  
  # Add description if available
  if (description != "") {
    content <- paste0(content, "## Description\n\n", description, "\n\n")
  }
  
  # Add list of letters mentioning this organization
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_id[j]
      content <- paste0(content, "- [Letter ", letter_id, "](../letters/letter_", letter_id, ".qmd)\n")
    }
  }
  
  # Write file
  output_file <- file.path(output_dir, "organizations", paste0("other_", other_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  
  org_count <- org_count + 1
}

cat("✓ Created", org_count, "organization pages\n\n")

# ============================================================
# CREATE CMY BOOK PAGES
# ============================================================

cat("Creating CMY bibliography pages...\n")

cmy_count <- 0
for (i in 1:nrow(cmy_bib)) {
  book_id <- cmy_bib$cmy_bookID[i]
  title <- cmy_bib$title[i]
  
  # Get book info
  info_parts <- c()
  if (!is.na(cmy_bib$date[i]) && cmy_bib$date[i] != "") {
    info_parts <- c(info_parts, paste("**Date:**", cmy_bib$date[i]))
  }
  if (!is.na(cmy_bib$genre[i]) && cmy_bib$genre[i] != "") {
    info_parts <- c(info_parts, paste("**Genre:**", cmy_bib$genre[i]))
  }
  if (!is.na(cmy_bib$publisher[i]) && cmy_bib$publisher[i] != "") {
    info_parts <- c(info_parts, paste("**Publisher:**", cmy_bib$publisher[i]))
  }
  if (!is.na(cmy_bib$notes[i]) && cmy_bib$notes[i] != "") {
    info_parts <- c(info_parts, paste("**Notes:**", cmy_bib$notes[i]))
  }
  
  info <- paste(info_parts, collapse = "\n\n")
  
  # Find which of our 49 letters mention this book
  mentions <- cmy_links %>% 
    filter(cmy_bookID == book_id, post_ID %in% letter_ids)
  
  # Create YAML header
  content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "book_id: ", book_id, "\n",
    "---\n\n"
  )
  
  # Add book info if available
  if (info != "") {
    content <- paste0(content, "## Publication Information\n\n", info, "\n\n")
  }
  
  # Add list of letters mentioning this book
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This Work\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_ID[j]
      content <- paste0(content, "- [Letter ", letter_id, "](../letters/letter_", letter_id, ".qmd)\n")
    }
  }
  
  # Write file
  output_file <- file.path(output_dir, "cmy_books", paste0("cmybook_", book_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  
  cmy_count <- cmy_count + 1
}

cat("✓ Created", cmy_count, "CMY book pages\n\n")

# ============================================================
# CREATE GENERAL BIBLIOGRAPHY PAGES
# ============================================================

cat("Creating general bibliography pages...\n")

gen_count <- 0
for (i in 1:nrow(gen_bib)) {
  book_id <- gen_bib$general_bookID[i]
  
  # Build title
  title_parts <- c()
  
  # Author name
  author_parts <- c(
    gen_bib$author_prefix[i],
    gen_bib$author_first_name[i],
    gen_bib$author_surname[i],
    gen_bib$author_suffix[i]
  )
  author_parts <- author_parts[!is.na(author_parts)]
  author <- paste(author_parts, collapse = " ")
  
  if (author != "") {
    title_parts <- c(title_parts, author)
  }
  
  if (!is.na(gen_bib$title[i]) && gen_bib$title[i] != "") {
    title_parts <- c(title_parts, gen_bib$title[i])
  }
  
  title <- paste(title_parts, collapse = " - ")
  if (title == "") {
    title <- paste("Book", book_id)
  }
  
  # Get book info
  info_parts <- c()
  if (!is.na(gen_bib$date[i]) && gen_bib$date[i] != "") {
    info_parts <- c(info_parts, paste("**Date:**", gen_bib$date[i]))
  }
  if (!is.na(gen_bib$imprint[i]) && gen_bib$imprint[i] != "") {
    info_parts <- c(info_parts, paste("**Imprint:**", gen_bib$imprint[i]))
  }
  if (!is.na(gen_bib$notes[i]) && gen_bib$notes[i] != "") {
    info_parts <- c(info_parts, paste("**Notes:**", gen_bib$notes[i]))
  }
  
  info <- paste(info_parts, collapse = "\n\n")
  
  # Find which of our 49 letters mention this book
  mentions <- gen_links %>% 
    filter(gen_bookID == book_id, post_id %in% letter_ids)
  
  # Create YAML header
  content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "book_id: ", book_id, "\n",
    "---\n\n"
  )
  
  # Add book info if available
  if (info != "") {
    content <- paste0(content, "## Publication Information\n\n", info, "\n\n")
  }
  
  # Add list of letters mentioning this book
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This Work\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_id[j]
      content <- paste0(content, "- [Letter ", letter_id, "](../letters/letter_", letter_id, ".qmd)\n")
    }
  }
  
  # Write file
  output_file <- file.path(output_dir, "other_books", paste0("otherbook_", book_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  
  gen_count <- gen_count + 1
}

cat("✓ Created", gen_count, "general bibliography pages\n\n")

cat("✅ All reference pages created!\n\n")
cat("Summary:\n")
cat("  People:", people_count, "\n")
cat("  Organizations:", org_count, "\n")
cat("  CMY Books:", cmy_count, "\n")
cat("  Other Books:", gen_count, "\n")
cat("  Total:", people_count + org_count + cmy_count + gen_count, "\n\n")
cat("Output directory:", output_dir, "\n")
cat("\nNext: Run STEP 3 to create website structure\n")