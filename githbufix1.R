# PREPARE_for_github_FIXED.R
# Prepares website for GitHub deployment (skips problem files)

# Source and destination
source_dir <- "C:/db/cmy_letters_website_v2/_site"
deploy_dir <- "C:/db/cmy_letters_deploy"

# Create fresh deploy directory
if (dir.exists(deploy_dir)) {
  unlink(deploy_dir, recursive = TRUE)
}
dir.create(deploy_dir, recursive = TRUE)

cat("Copying website files...\n")

# Get all files and directories, excluding . and ..
all_items <- list.files(source_dir, full.names = TRUE, all.files = FALSE)

cat("Found", length(all_items), "items to copy\n\n")

# Copy each item
copied <- 0
errors <- 0

for (item in all_items) {
  item_name <- basename(item)
  dest_path <- file.path(deploy_dir, item_name)
  
  result <- tryCatch({
    if (dir.exists(item)) {
      # It's a directory - copy recursively
      dir.create(dest_path, showWarnings = FALSE, recursive = TRUE)
      file.copy(item, deploy_dir, recursive = TRUE, overwrite = TRUE)
    } else {
      # It's a file - copy it
      file.copy(item, dest_path, overwrite = TRUE)
    }
    copied <- copied + 1
    TRUE
  }, error = function(e) {
    cat("⚠ Error copying", item_name, ":", e$message, "\n")
    errors <- errors + 1
    FALSE
  })
}

cat("\n✓ Copied", copied, "items\n")
if (errors > 0) {
  cat("⚠", errors, "errors\n")
}

# Create .nojekyll file
writeLines("", file.path(deploy_dir, ".nojekyll"))
cat("✓ Created .nojekyll file\n\n")

# Verify what we have
cat("Contents of deploy directory:\n")
deploy_items <- list.files(deploy_dir, all.files = TRUE)
cat(paste(deploy_items, collapse = "\n"), "\n\n")

cat("✅ Ready for GitHub!\n\n")
cat("Next: Open command prompt and run:\n")
cat("cd C:/db/cmy_letters_deploy\n")
cat("git init\n")
cat("git add .\n")
cat('git commit -m "Initial CMY Letters website"\n')
cat("git branch -M main\n")
cat("git remote add origin https://github.com/CMYonge/letters.git\n")
cat("git push -u origin main\n")