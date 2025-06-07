sqlcon <- dbConnect(RSQLite::SQLite(),"fhmdata.sqlite")
sqlcon

library(DBI)

drop_tables_from_df <- function(db_connection, column_dataframe) {
  for (table_name in colnames(column_dataframe)) {
    # Extract column names (remove NAs)
    col_names <- na.omit(column_dataframe[[table_name]])
    
    # Define column definitions (all columns as TEXT, adjust as needed)
    col_definitions <- paste0(col_names, " TEXT", collapse = ", ")
    
    # Construct the SQL statement
    sql_drop <- paste0("DROP TABLE IF EXISTS ", table_name)
    
    #print(sql_query)
    # Execute the SQL command
    dbExecute(db_connection, sql_drop)
    
    cat("dropped table:", table_name, "\n")
  }
  
  
}

create_tables_from_df <- function(db_connection, column_dataframe) {
  for (table_name in colnames(column_dataframe)) {
    # Extract column names (remove NAs)
    col_names <- na.omit(column_dataframe[[table_name]])
    
    # Define column definitions (all columns as TEXT, adjust as needed)
    col_definitions <- paste0(col_names, " TEXT", collapse = ", ")
    
    # Construct the SQL statement
    sql_query <- paste0("CREATE TABLE IF NOT EXISTS ", table_name, " (", col_definitions, " )")
    
    #print(sql_query)
    # Execute the SQL command
    dbExecute(db_connection, sql_query)
    
    cat("Created table:", table_name, "\n")
  }
  

}


# Usage Example
column_dataframe <- read_csv_columns_df("C:/Users/HP/Documents/FHM_project/data/20230405/")  # Your function to extract column names
column_dataframe <- as.data.frame(apply(column_dataframe, c(1, 2), function(x) gsub("\\.", "_", x)))
View(column_dataframe)
drop_tables_from_df(sqlcon, column_dataframe)
create_tables_from_df(sqlcon, column_dataframe)

dbListTables(sqlcon)
dbGetQuery(sqlcon, "SELECT * FROM acov19DAG order by published_date desc")


library(DBI)
library(RSQLite)

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

# Disconnect after inserting data
dbDisconnect(sqlcon)

# Load required package
library(DBI)

# Function to add a column to all tables in SQLite
add_published_date_column <- function(conn) {
  # Get a list of all tables
  tables <- dbListTables(conn)
  
  # Loop through each table and alter it
  for (table in tables) {
    # Check if 'published_date' already exists
    existing_cols <- dbListFields(conn, table)
    
    if (!"published_date" %in% existing_cols) {
      # Alter the table to add 'published_date'
      query <- paste0("ALTER TABLE ", table, " ADD COLUMN published_date TEXT;")
      dbExecute(conn, query)
      cat("Added 'published_date' to table:", table, "\n")
    } else {
      cat("'published_date' already exists in table:", table, "\n")
    }
  }
}

# Call the function (Assuming sqlcon is your SQLite connection)
add_published_date_column(sqlcon)


# Extract the last part of the folder path as the published_date
extract_published_date <- function(folder_path) {
  parts <- strsplit(folder_path, "/|\\\\")[[1]]  # Split by both '/' and '\'
  return(tail(parts, 1))  # Get the last element
}

# Assign extracted date to a variable
published_date <- extract_published_date(folder_path)

# Function to update 'published_date' column in all tables
update_published_date <- function(conn, published_date) {
  tables <- dbListTables(conn)  # Get all table names
  
  for (table in tables) {
    # Update all rows in the table with the extracted published_date
    query <- paste0("UPDATE ", table, " SET published_date = '", published_date, "';")
    dbExecute(conn, query)
    cat("Updated 'published_date' for table:", table, "\n")
  }
}

# Call the function (Assuming sqlcon is your SQLite connection)
update_published_date(sqlcon, published_date)

