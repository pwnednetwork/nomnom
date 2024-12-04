#!/bin/bash

# Check if a target directory was provided
if [ $# -ne 1 ]; then
  echo "Usage: $0 <target_directory>"
  echo "Example: $0 ~/.doom.d/"
  exit 1
fi

target_dir="$1"

# Remove trailing slash if present
target_dir="${target_dir%/}"

# Check if target directory exists
if [ ! -d "$target_dir" ]; then
  echo "Error: Target directory '$target_dir' does not exist"
  exit 1
fi

# Initialize counters
success_count=0
skip_count=0
error_count=0

# Print header
printf "\nCreating symbolic links for .el files:\n"
printf "======================================\n"

# Process each .el file in current directory
for file in *.el; do
  # Check if any .el files exist
  if [ ! -e "$file" ]; then
    echo "No .el files found in current directory"
    exit 1
  fi

  # Get absolute path of source file
  src_path="$(pwd)/$file"
  target_path="$target_dir/$file"

  printf "Processing %-30s -> " "$file"

  # Check if target already exists
  if [ -e "$target_path" ]; then
    if [ -L "$target_path" ]; then
      printf "[\e[33mSKIPPED\e[0m] Symbolic link already exists\n"
      ((skip_count++))
    else
      printf "[\e[31mERROR\e[0m] Regular file exists at target location\n"
      ((error_count++))
    fi
    continue
  fi

  # Create symbolic link
  if ln -s "$src_path" "$target_path" 2>/dev/null; then
    printf "[\e[32mSUCCESS\e[0m] Link created\n"
    ((success_count++))
  else
    printf "[\e[31mERROR\e[0m] Failed to create link\n"
    ((error_count++))
  fi
done

# Print summary
printf "\nSummary:\n"
printf "========\n"
printf "Successfully created: %d\n" $success_count
printf "Skipped existing:    %d\n" $skip_count
printf "Errors encountered:  %d\n" $error_count
printf "\n"

# Exit with error if any links failed
[ $error_count -eq 0 ] || exit 1
