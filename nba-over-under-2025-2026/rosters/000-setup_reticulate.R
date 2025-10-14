# R script to set up Python virtual environment

python_packages <- c("pandas", "nba_api")

library(reticulate)

# Define a relative path for the virtual environment
virtualenv_dir <- file.path("venv")

# Create the virtual environment in the project directory
virtualenv_create(envname = virtualenv_dir)

# Install required Python packages
virtualenv_install(envname = virtualenv_dir, packages = python_packages)

# Set RETICULATE_PYTHON to point to the virtual environment's Python binary
path_to_python <- virtualenv_python(envname = virtualenv_dir)
writeLines(sprintf("RETICULATE_PYTHON=%s", path_to_python), Sys.getenv("GITHUB_ENV"))
