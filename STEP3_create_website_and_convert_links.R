# STEP3_create_website_and_convert_links_CORRECTED.R
# Creates website structure, copies files, converts [[tags]] to links, and fixes YAML

library(dplyr)
library(readr)
library(stringr)

# Directories
letter_source <- "C:/db/test_49_letters_qmd_v2"
ref_source <- "C:/db/test_49_reference_pages_qmd_v2"
website_dir <- "C:/db/cmy_letters_website_v2"

cat("STEP 3: Building website structure\n\n")

# ============================================================
# PART 1: CREATE WEBSITE STRUCTURE
# ============================================================

cat("Part 1: Creating website folders...\n")

# Create main structure
dir.create(website_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "letters"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "people"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "organizations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "cmy_books"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "other_books"), showWarnings = FALSE, recursive = TRUE)

cat("✓ Created folder structure\n\n")

# ============================================================
# PART 2: CREATE QUARTO CONFIGURATION
# ============================================================

cat("Part 2: Creating Quarto config files...\n")

# Create _quarto.yml
quarto_config <- '
project:
  type: website
  output-dir: _site

website:
  title: "Charlotte Mary Yonge Letters"
  navbar:
    left:
      - text: "Home"
        href: index.qmd
      - text: "Letters"
        href: letters/index.qmd
      - text: "People"
        href: people/index.qmd
      - text: "Organizations"
        href: organizations/index.qmd
      - text: "CMY Bibliography"
        href: cmy_books/index.qmd
      - text: "General Bibliography"
        href: other_books/index.qmd
  search: true

format:
  html:
    theme: cosmo
    css: styles.css
    toc: true
    toc-depth: 2
'

writeLines(quarto_config, file.path(website_dir, "_quarto.yml"))

# Create main index page
index_content <- '---
title: "Charlotte Mary Yonge Letters Collection"
---

## About This Collection

This digital collection contains correspondence from Charlotte Mary Yonge, with detailed cross-references to people, organizations, and publications mentioned in the letters.

## Browse

- **[Letters](letters/index.qmd)** - Browse all letters chronologically
- **[People](people/index.qmd)** - Index of correspondents and people mentioned
- **[Organizations](organizations/index.qmd)** - Churches, institutions, and groups
- **[CMY Bibliography](cmy_books/index.qmd)** - Works by Charlotte Mary Yonge
- **[General Bibliography](other_books/index.qmd)** - Other works referenced

## Search

Use the search box in the navigation bar to find letters, people, or topics.

---

*This collection was originally maintained as a WordPress database. It has been converted to a static website for long-term preservation and accessibility.*
'

writeLines(index_content, file.path(website_dir, "index.qmd"))

# Create section index pages
writeLines('---\ntitle: "Letters"\n---\n\n## Browse Letters\n\nLetters are listed chronologically below.\n', 
           file.path(website_dir, "letters", "index.qmd"))

writeLines('---\ntitle: "People"\n---\n\n## Index of People\n\nThis section contains biographical information about correspondents and people mentioned in the letters.\n', 
           file.path(website_dir, "people", "index.qmd"))

writeLines('---\ntitle: "Organizations"\n---\n\n## Organizations, Places, and Topics\n\nThis section includes churches, institutions, publications, and other entities referenced in the letters.\n', 
           file.path(website_dir, "organizations", "index.qmd"))

writeLines('---\ntitle: "Works by Charlotte Mary Yonge"\n---\n\n## Bibliography of CMY\'s Works\n\nBooks and publications by Charlotte Mary Yonge referenced in the correspondence.\n', 
           file.path(website_dir, "cmy_books", "index.qmd"))

writeLines('---\ntitle: "General Bibliography"\n---\n\n## Other Works Referenced\n\nBooks, articles, and publications by other authors mentioned in the letters.\n', 
           file.path(website_dir, "other_books", "index.qmd"))

# Create CSS
css_content <- '
/* Custom styles for CMY Letters website */

body {
  font-family: Georgia, serif;
  line-height: 1.6;
}

.letter-metadata {
  background-color: #f5f5f5;
  padding: 1em;
  margin-bottom: 1.5em;
  border-left: 4px solid #333;
}

.person-bio {
  font-style: italic;
  color: #555;
  margin-bottom: 1em;
}
'

writeLines(css_content, file.path(website_dir, "styles.css"))

cat("✓ Created config files\n\n")

# ============================================================
# PART 3: LOAD LOOKUP TABLES FOR LINK CONVERSION
# ============================================================

cat("Part 3: Loading lookup tables...\n")

persons <- read_csv("C:/db/persons.csv", show_col_types = FALSE)
others <- read_csv("C:/db/wp_others.csv", show_col_types = FALSE)
cmy_bib <- read_csv("C:/db/wp_cmybibliography.csv", show_col_types = FALSE)
gen_bib <- read_csv("C:/db/wp_generalbibliography.csv", show_col_types = FALSE)

# Create lookup functions
get_person_name <- function(id) {
  person <- persons %>% filter(persons_id == id)
  if (nrow(person) == 0) return(paste("Person", id))
  
  name_parts <- c(
    person$prefix[1],
    person$first_name[1],
    person$maiden_name[1],
    person$surname[1],
    person$suffix[1]
  )
  name_parts <- name_parts[!is.na(name_parts)]
  name <- paste(name_parts, collapse = " ")
  
  if (name == "") return(paste("Person", id))
  return(name)
}

get_other_name <- function(id) {
  other <- others %>% filter(others_id == id)
  if (nrow(other) == 0) return(paste("Other", id))
  return(other$name[1])
}

get_cmy_title <- function(id) {
  book <- cmy_bib %>% filter(cmy_bookID == id)
  if (nrow(book) == 0) return(paste("CMY Book", id))
  return(book$title[1])
}

get_gen_title <- function(id) {
  book <- gen_bib %>% filter(general_bookID == id)
  if (nrow(book) == 0) return(paste("Book", id))
  return(book$title[1])
}

cat("✓ Lookup tables loaded\n\n")

# ============================================================
# PART 4: FUNCTION TO CONVERT TAGS TO LINKS AND FIX YAML
# ============================================================

convert_tags_and_fix_yaml <- function(filepath, file_location) {
  
  content <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  content_text <- paste(content, collapse = "\n")
  
  # Determine relative paths based on file location
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
  person_matches <- str_match_all(content_text, "\\[\\[person:(\\d+)\\]\\]")[[1]]
  if (nrow(person_matches) > 0) {
    for (i in 1:nrow(person_matches)) {
      tag <- person_matches[i, 1]
      id <- as.numeric(person_matches[i, 2])
      name <- get_person_name(id)
      link <- paste0("[", name, "](", person_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # Convert [[other:456]] tags
  other_matches <- str_match_all(content_text, "\\[\\[other:(\\d+)\\]\\]")[[1]]
  if (nrow(other_matches) > 0) {
    for (i in 1:nrow(other_matches)) {
      tag <- other_matches[i, 1]
      id <- as.numeric(other_matches[i, 2])
      name <- get_other_name(id)
      link <- paste0("[", name, "](", org_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # Convert [[cmybook:789]] tags
  cmy_matches <- str_match_all(content_text, "\\[\\[cmybook:(\\d+)\\]\\]")[[1]]
  if (nrow(cmy_matches) > 0) {
    for (i in 1:nrow(cmy_matches)) {
      tag <- cmy_matches[i, 1]
      id <- as.numeric(cmy_matches[i, 2])
      title <- get_cmy_title(id)
      link <- paste0("[", title, "](", cmy_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # Convert [[otherbook:101]] tags
  book_matches <- str_match_all(content_text, "\\[\\[otherbook:(\\d+)\\]\\]")[[1]]
  if (nrow(book_matches) > 0) {
    for (i in 1:nrow(book_matches)) {
      tag <- book_matches[i, 1]
      id <- as.numeric(book_matches[i, 2])
      title <- get_gen_title(id)
      link <- paste0("[", title, "](", book_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # FIX YAML: Convert double quotes to single quotes in title
  # This avoids escaping issues with embedded quotes
  yaml_pattern <- '(---\\s*\\ntitle: )"([^"]*(?:""[^"]*)*)"(\\s*\\n)'
  
  if (grepl(yaml_pattern, content_text, perl = TRUE)) {
    content_text <- gsub(
      yaml_pattern,
      function(match) {
        # Extract the title content between double quotes
        title_content <- sub('.*title: "', '', match)
        title_content <- sub('".*', '', title_content)
        
        # Unescape any doubled quotes from previous fixes
        title_content <- gsub('""', '"', title_content)
        
        # Escape single quotes for YAML
        title_content <- gsub("'", "''", title_content)
        
        # Reconstruct with single quotes
        parts <- strsplit(match, 'title: "')[[1]]
        before <- parts[1]
        after <- sub('"', '', sub('^[^"]*"', '', match))
        
        paste0(before, "title: '", title_content, "'", after)
      },
      content_text,
      perl = TRUE
    )
  }
  
  return(content_text)
}

# ============================================================
# PART 5: COPY AND CONVERT LETTERS
# ============================================================

cat("Part 5: Processing letters...\n")

letter_files <- list.files(letter_source, pattern = "\\.qmd$", full.names = TRUE)
letters_processed <- 0

for (letter_file in letter_files) {
  converted_content <- convert_tags_and_fix_yaml(letter_file, "letters")
  output_file <- file.path(website_dir, "letters", basename(letter_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  letters_processed <- letters_processed + 1
}

cat("✓ Processed", letters_processed, "letters\n\n")

# ============================================================
# PART 6: COPY AND CONVERT REFERENCE PAGES
# ============================================================

cat("Part 6: Processing reference pages...\n")

# People
people_files <- list.files(file.path(ref_source, "people"), pattern = "\\.qmd$", full.names = TRUE)
people_processed <- 0
for (people_file in people_files) {
  converted_content <- convert_tags_and_fix_yaml(people_file, "people")
  output_file <- file.path(website_dir, "people", basename(people_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  people_processed <- people_processed + 1
}
cat("✓ Processed", people_processed, "people pages\n")

# Organizations
org_files <- list.files(file.path(ref_source, "organizations"), pattern = "\\.qmd$", full.names = TRUE)
org_processed <- 0
for (org_file in org_files) {
  converted_content <- convert_tags_and_fix_yaml(org_file, "organizations")
  output_file <- file.path(website_dir, "organizations", basename(org_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  org_processed <- org_processed + 1
}
cat("✓ Processed", org_processed, "organization pages\n")

# CMY Books
cmy_files <- list.files(file.path(ref_source, "cmy_books"), pattern = "\\.qmd$", full.names = TRUE)
cmy_processed <- 0
for (cmy_file in cmy_files) {
  converted_content <- convert_tags_and_fix_yaml(cmy_file, "cmy_books")
  output_file <- file.path(website_dir, "cmy_books", basename(cmy_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  cmy_processed <- cmy_processed + 1
}
cat("✓ Processed", cmy_processed, "CMY book pages\n")

# Other Books
book_files <- list.files(file.path(ref_source, "other_books"), pattern = "\\.qmd$", full.names = TRUE)
book_processed <- 0
for (book_file in book_files) {
  converted_content <- convert_tags_and_fix_yaml(book_file, "other_books")
  output_file <- file.path(website_dir, "other_books", basename(book_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  book_processed <- book_processed + 1
}
cat("✓ Processed", book_processed, "other book pages\n\n")

# ============================================================
# SUMMARY
# ============================================================

cat("✅ Website structure complete!\n\n")
cat("Summary:\n")
cat("  Letters:", letters_processed, "\n")
cat("  People:", people_processed, "\n")
cat("  Organizations:", org_processed, "\n")
cat("  CMY Books:", cmy_processed, "\n")
cat("  Other Books:", book_processed, "\n")
cat("  Total files:", letters_processed + people_processed + org_processed + cmy_processed + book_processed, "\n\n")
cat("Website location:", website_dir, "\n\n")
cat("Next step: Render the website\n")
cat("1. Open terminal/command prompt\n")
cat("2. Navigate to:", website_dir, "\n")
cat("3. Run: quarto render\n")
cat("4. Website will be built in _site/ folder\n")