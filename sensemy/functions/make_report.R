# Clean up the environment
#rm(list = ls())

# Load the libraries
library(knitr)
library(rmarkdown)

# Set the root dir because my rmds live in rmds/ subfolder
opts_knit$set(root.dir = '../.')

# By default, don't open the report at the end of processing
default_open_file <- FALSE

# Main function
report <- function(file, n_file = "", open_file = default_open_file,  
                   report_dir = "reports") {
  
  ### Set the name of the report file ###
  base_name <- sub(pattern = ".Rmd", replacement = "", x = basename(file))
  
  # Make nfiles with always 2 digits
  n_file <- ifelse(as.integer(n_file) < 10, paste0("0", n_file), n_file)
  
  file_name <- paste0(n_file, "-", base_name, ".html")
  
  ### Render ###
  render(
    input = file,
    output_format = html_document(
      toc = TRUE,
      toc_depth = 1,
      code_folding = "hide"
    ),
    output_file = file_name,
    output_dir = report_dir,
    envir = new.env()
  )
  
  ### Under macOS, open the report file  ###
  ### in firefox at the end of rendering ###
  if(open_file & Sys.info()[1] == "Darwin") {
    result_path <- file.path(report_dir, file_name)
    system(command = paste("firefox", result_path))
  }
  
}