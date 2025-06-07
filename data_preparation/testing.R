#Step 1: Load csv files for a given folder into R:
# Set your folder path here
folder_path <- "C:/Users/HP/Documents/FHM_project/data/20241108/" 

# Function to load sample data in R, replace . with _ in column names, and add published_date
my_csv_files <- function(folder_path) {  
  # Extract published_date from folder_path
  parts <- strsplit(folder_path, "/|\\\\")[[1]]  # Split by both '/' and '\'
  published_date <- tail(parts, 1)  # Get the last element
  
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
    
    # Add published_date column
    df$published_date <- published_date
    
    # Assign the modified dataframe to a variable named after the file
    assign(file_name, df, envir = .GlobalEnv)
    
    # Print message
    cat("Loaded:", file_name, "\n")
  }
}

# Call the function
my_csv_files(folder_path)

#Step 2: Insert into SQL database:
# Function to insert a dataframe into SQLite
insert_into_sqlite <- function(conn, df_name) {
  df <- get(df_name)  # Retrieve the dataframe from its name as a string
  
  # Ensure table exists before inserting
  if (dbExistsTable(conn, df_name)) {
    dbWriteTable(conn, df_name, df, append = TRUE, row.names = FALSE)
    cat("Inserted data into:", df_name, "\n")
  } else {
    cat("Table", df_name, "does not exist. Skipping...\n")
  }
}

# Connect to SQLite database
sqlcon <- dbConnect(SQLite(), "fhmdata.sqlite")

# List of dataframe names (same as table names)
df_names <- c("acov19DAG", "bcov19Kom", "ccov19kon", "ccov19Reg", "ccov19Regsasong",
              "dcov19ald", "ecov19sabo", "ecov19sabosasong", "xcov19ivavDAG",
              "ycov19ivavald", "ycov19ivavkon")

# Loop through each dataframe and insert data
for (df_name in df_names) {
  insert_into_sqlite(sqlcon, df_name)
}
