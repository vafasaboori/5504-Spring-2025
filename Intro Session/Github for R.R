# Tutorial: https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

# Git is open source software for version control. 
# Using Git, you can see all previous versions of your code.

# Install Git: https://vimeo.com/511798611

# Configure Git: https://vimeo.com/511798861

install.packages("usethis")
library(usethis)

edit_git_config() # Enter your name and email

# Initialize a Git Repository: https://vimeo.com/511799486
# 1st create a new project (check create a git repository)
# Alternatively

library(usethis)
use_git()

# View Commit History: https://vimeo.com/511799855

# Make a Commit and View More History: https://vimeo.com/511800674

# Connect RStudio and GitHub
  # Sign up for GitHub
  # Create a Personal Access Token (PAT) on GitHub: https://vimeo.com/511801645
  
library(usethis)
create_github_token()

# Store PPAT to Connect RStudio and GitHub: https://vimeo.com/511803103

library(gitcreds)
gitcreds_set()

# How to Connect RStudio Projects with GitHub Repositories: https://vimeo.com/511804087
library(usethis)
use_github()

# General Work-flow (Push): https://vimeo.com/511805399 
# General Work-flow (Pull): https://vimeo.com/511805818




