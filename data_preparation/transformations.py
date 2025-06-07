import os
import pandas as pd
from defined import *
import shutil

def folder_creation(file_name):
    """Create folder for transformed data
    Parameters:
        file_name: name of file
    """
    if not os.path.exists("transformed_data"):
        os.mkdir("transformed_data")

def find_differences():
    """Compare files and creates differences files"""
    #Sort the folders in descending order
    list_of_folder = sorted(folders, key=lambda x: int(x), reverse=True) 
    #Iterate to go through all files
    file_names = os.listdir(f"data/{list_of_folder[0]}")
    for name_of_file in file_names:   
        #Create folder for transformed data 
        folder_creation(f"{name_of_file}")
        #Read the first file
        df1 = pd.read_csv(f"data/{list_of_folder[0]}/{name_of_file}", sep=',', encoding='utf-8')
        try:
            #If the file exists, read it
            results = pd.read_csv(f"transformed_data/changes_{name_of_file}", sep=',', encoding='utf-8')
        except:
            #File does not exist - runs for the first time
            #Get the columns of that file
            columns_of_df1 = list(df1.columns)
            #Copy the name of the last column and then drop it. 
            tmp = columns_of_df1[-1]
            columns_of_df1 = columns_of_df1[:-1]
            #Add _From and _To to the copy where the old and new values are stored
            columns_of_df1.append(tmp+ "_From")
            columns_of_df1.append(tmp+ "_To")
            #Then add Datum_för_ändring where the date of the changes will be stored
            columns_of_df1.append("Datum_för_ändring")
            #Create a dataframe with the new columns
            results = pd.DataFrame(columns=columns_of_df1)
        columns_of_df1 = list(results.columns)
        date_of_change = list_of_folder[0]
    
        #Go through all other files and compare them
        for comparing_folder in list_of_folder[1:]:
            #Try to read the file from the folder else continue, in case of missing files
            try:
                df2 = pd.read_csv(f"data/{comparing_folder}/{name_of_file}", sep=',', encoding='utf-8')
            except:
                print(f"File {name_of_file} in folder {comparing_folder} could not be read")
                continue
            #Compare the two files
            different_values = compare_textfiles(data1 = df1, data2 = df2, date=date_of_change, columns_list= columns_of_df1)
            #Add new differences to the dataframe
            results = pd.concat([results, different_values], ignore_index=True)
            #Save the new file as dataframe1
            df1 = df2
            #Save the date of the changes
            date_of_change = comparing_folder
            print(f"{comparing_folder} is done")
        #Remove duplicate rows
        results = results.drop_duplicates()
        #Save the dataframe as csv file
        results.to_csv(f"transformed_data/changes_{name_of_file}", index=False)
    #Delete all folders except the newest one
    for folder in list_of_folder[1:]:
        shutil.rmtree(f"data/{folder}")

def compare_textfiles(data1: pd.DataFrame, data2: pd.DataFrame, date: int, columns_list: list):
    """Compare two dataframes and return the differences
    Parameters:
        data1: dataframe1
        data2: dataframe2
        date: date of the data2
        columns_list: list of columns of the dataframe

    Returns:
        different_dataframe: dataframe with the differences"""
    #Merge the two dataframes, values will get _From (Data1) and _To (Data2) suffixes.
    merged_data = data2.merge(data1, on=columns_list[:-3], how="inner", suffixes=["_From", "_To"])
    #Convert the relevant columns to numeric and integers if necessary
    #If the values are not numeric, they will be converted to NaN
    merged_data[columns_list[-3]] = pd.to_numeric(merged_data[columns_list[-3]], downcast="integer", errors="coerce")
    merged_data[columns_list[-2]] = pd.to_numeric(merged_data[columns_list[-2]], downcast="integer", errors="coerce")
    # Compare rows that are different in both dataframes
    different_dataframe = merged_data[merged_data[columns_list[-3]] != merged_data[columns_list[-2]]]
    #Store all the values except the last one.
    different_dataframe = different_dataframe[columns_list[:-1]]
    #Store the date of the changes as the name of the folder where the file is stored
    different_dataframe[columns_list[-1]] = date
    #Drop NA, NaN values or empty
    different_dataframe = different_dataframe.dropna()


    return different_dataframe

    