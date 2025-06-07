read_csv_columns <- function(folder_path) {  
  # List all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if any CSV files exist
  if (length(csv_files) == 0) {
    cat("No CSV files found in the specified folder.\n")
    return()
  }
  
  # Loop through each file and print column names
  for (file in csv_files) {
    cat("File:", basename(file), "\n")
    df <- read.csv(file, nrows = 1)  # Read only the first row to extract column names efficiently
    print(names(df))
    cat("---------------------------\n")
  }
}

# Example usage
# Set your folder path here
folder_path <- "C:/Users/HP/Documents/FHM_project/data/20230405/" 

read_csv_columns(folder_path)


csv_file_names <- function(folder_path) {  
  # List all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Remove files with "PCR" in the filename
  csv_files <- csv_files[!grepl("PCR", csv_files)]
  
  # Remove files with "test" in the filename
  csv_files <- csv_files[!grepl("test", csv_files)]
  
  # Check if any CSV files exist
  if (length(csv_files) == 0) {
    cat("No CSV files found in the specified folder.\n")
    return(character(0))
  }
  
  # Convert to vector and remove ".csv" extension
  csv_files <- tools::file_path_sans_ext(basename(csv_files))
  
  # Print list of all CSV files
  cat("List of CSV files:\n")
  print(csv_files)
}

csv_file_names(folder_path)


read_csv_columns_df <- function(folder_path) {  
  # List all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Remove files with "PCR" in the filename
  csv_files <- csv_files[!grepl("PCR", csv_files)]
  
  # Remove files with "test" in the filename
  csv_files <- csv_files[!grepl("test", csv_files)]
  
  # Check if any CSV files exist
  if (length(csv_files) == 0) {
    cat("No CSV files found in the specified folder.\n")
    return(data.frame())  # Return an empty dataframe
  }
  
  # Create an empty list to store column names for each file
  column_data <- list()
  
  # Loop through each file and store column names
  for (file in csv_files) {
    file_name <- tools::file_path_sans_ext(basename(file))  # Remove .csv extension
    df <- read.csv(file, nrows = 1)  # Read only the first row to get column names
    column_data[[file_name]] <- names(df)
  }
  
  # Convert the list to a dataframe (fill missing values with NA)
  column_df <- as.data.frame(do.call(cbind, lapply(column_data, function(x) {
    length_x <- length(x)
    max_length <- max(sapply(column_data, length))
    c(x, rep(NA, max_length - length_x))  # Fill shorter columns with NA
  })), stringsAsFactors = FALSE)
  
  # Print the resulting dataframe
  print(column_df)
  
  return(column_df)  # Return the dataframe
}

column_dataframe <- read_csv_columns_df(folder_path)
column_dataframe <- as.data.frame(apply(column_dataframe, c(1, 2), function(x) gsub("\\.", "_", x)))

# Extract column names (remove NAs)
col_names <- na.omit(column_dataframe[1])

# Replace "." with "_" in column names
col_names[3, ] <- gsub("\\.", "_", col_names[3, ])


View(column_dataframe)

folder_path <- "C:/Users/HP/Documents/FHM_project/data/20240405/" 

# Load sample data in R and replace . with _ in column names
my_csv_files <- function(folder_path) {  
  # List all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Remove files with "PCR" or "test" in the filename
  csv_files <- csv_files[!grepl("PCR", csv_files)]
  csv_files <- csv_files[!grepl("test", csv_files)]
  csv_files <- csv_files[!grepl("100k", csv_files)]
  
  # Check if any CSV files exist
  if (length(csv_files) == 0) {
    cat("No CSV files found in the specified folder.\n")
    return(invisible(NULL))
  }
  
  # Loop through each file, read it, and assign it dynamically
  for (file in csv_files) {
    # Extract the filename without extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Read the CSV file into a dataframe
    df <- read.csv(file, stringsAsFactors = FALSE)
    
    # Replace . with _ in column names
    colnames(df) <- gsub("\\.", "_", colnames(df))
    
    # Assign the modified dataframe to a variable named after the file
    assign(file_name, df, envir = .GlobalEnv)
    
    # Print message
    cat("Loaded:", file_name, "\n")
  }
}

# Call the function
my_csv_files(folder_path)

