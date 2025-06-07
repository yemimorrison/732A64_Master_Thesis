import pandas as pd
from defined import *
from datetime import date
import numpy as np
import shutil

def make_dir_excel(date_file: str):
    """ Create a folder to store the data if it does not exist
    Parameters:
        date_file: date of the file
    """
    #Create date folder
    if not os.path.exists("data/" + date_file):
        os.mkdir("data/" + date_file)
    
def clean_rows(df: pd.DataFrame):
    """ Clean rows with specific values in a dataframe
    Drops rows with ".." ,"." in specific dataframes
    Additionally drops categories that can be found in defined.py
    These categories are usually per 10 000, per 100 000 
    For PCR values, the first 3 characters are deleted in the region column in order
    to make it easier to join the tables together

    Parameters:
        df: dataframe to discard rows from
    """
    
    
    
    if "Kategori" in df:
        df = df[df["Kategori"].isin(["..", "."]) == False]
    
    if "Ej bedömbara" in df:
        df = df[~df.isin(['.']).any(axis=1)]
    
    if "Okänd tidpunkt" in df:
        df = df[~df.isin(['.']).any(axis=1)]
    
    if "Testade individer med PCR" in df:
        #Delete first 3 characters in region - str removes first 3 characters
        df["Region"] = df["Region"].str[3:]
    
    if df.isin(['Södermland']).any().any():
        #Convert to södermland to södermanland
        df["Region"] = df["Region"].replace("Södermland", "Södermanland", regex=True)
    
    if df.isin(['Sörmland']).any().any():
        #Convert Sörmland to "Södermanland"
        df["Region"] = df["Region"].replace("Sörmland", "Södermanland", regex=True)

    #Drop categories that are in categories_to_skip
    if "Indikator" in df.columns:
        df = df[df["Indikator"].isin(categories_to_skip) == False]
    
    return df


def get_excel_files():
    """Get all excel files from a folder convert them to corresponding csv file."""
    #Get all files from excel folder
    files = os.listdir("excel")
    for file in files:
        if file.endswith(".xlsx"):
            url =  f"excel/{file}"
            #Read all sheets from the excel file
            df_excel = pd.read_excel(url, sheet_name=None)
            #Get all dataframes from the excel file
            get_dfs_from_excel(df_excel, file)
            #Remove Excel
            os.remove(url)

def get_csv_files():
    """Get all txt files from a folder, transform them and save them as csv files."""
    #Get all files from a folder
    encodings = ['utf-8', 'latin-1']
    data = os.listdir("txt/")
    for folder in data:
        files = os.listdir(f"txt/{folder}")
        for file in files:
            if file.endswith(".txt"):
                url =  f"txt/{folder}/{file}"
                #Read a text file and save it as csv
                text_file = pd.DataFrame()  # Initialize text_file before the loop
                for encoding in encodings:
                    try:
                        text_file = pd.read_csv(url, sep='\t', encoding=encoding)
                        break
                    except:
                        continue
                        #print(f"Encoding {encoding} did not work for {file}")
                #Clean rows
                cleaned_file = clean_rows(text_file)
                
                date_of_file = f"{folder}"
                #Create date folder
                make_dir_excel(date_of_file)
                #Save the csv file
                cleaned_file.to_csv(f"data/{folder}/{file[:-4]}.csv", index=False, encoding='utf-8')
                #Delete the text file
                os.remove(url)
        print(f"Folder - {folder} - Completed")

def get_dfs_from_excel(df: pd.DataFrame, file: str):
    """Get dataframes from an excel file and save them as corresponding csv files
    Parameters:
      df: dataframe of an excel file
      file: name of the excel file"""
    #Get the last 5 characters from the file name
    name_of_file = file[:-5]
    #Get month, day and year from the file name
    date_of_file = name_of_file[-11:]
    #Convert to date
    date_of_file = pd.to_datetime(date_of_file).date().strftime("%Y%m%d")
    #get year from file name
    year_of_file = name_of_file[-4:]
    #Create date folder
    make_dir_excel(date_of_file)
    #Veckodata Kommun_stadsdel and Region may not exist in older files
    if ("Veckodata Region" or "Veckodata Kommun_stadsdel") in df:
        #As some files have no year in the file name, it needs to be added manually
        if year_of_file == "2020":
            df["Veckodata Region"]["år"] = 2020
            df["Veckodata Kommun_stadsdel"]["år"] = 2020
        #Creates a copy in order to not overrite the original dataframe
        df2 = df["Veckodata Region"].copy()
        convert_to_acov19dag(df["Antal per dag region"]).to_csv("data/"+ date_of_file +"/acov19DAG.csv", sep=',', index=False, encoding="utf-8")
        convert_to_xcov19ivavDAG(df["Antal avlidna per dag"], df["Antal intensivvårdade per dag"]).to_csv("data/"+ date_of_file +"/xcov19ivavDAG.csv", sep=',', index=False, encoding="utf-8")
        convert_to_ccov19regsasong(df2).to_csv("data/"+ date_of_file +"/ccov19regsasong.csv", sep=',', index=False, encoding="utf-8")
        convert_to_bcov19kom(df["Veckodata Kommun_stadsdel"]).to_csv("data/"+ date_of_file +"/bcov19Kom.csv", sep=',', index=False, encoding="utf-8")
        convert_to_ccov19reg(df["Veckodata Region"]).to_csv("data/"+ date_of_file +"/ccov19reg.csv", sep=',', index=False, encoding="utf-8")
        print("Done with " + date_of_file)
    #Veckodata Kommun_stadsdel and Region may not exist in older files
    else:
        convert_to_acov19dag(df["Antal per dag region"]).to_csv("data/"+ date_of_file +"/acov19DAG.csv", sep=',', index=False, encoding="utf-8")
        convert_to_xcov19ivavDAG(df["Antal avlidna per dag"], df["Antal intensivvårdade per dag"]).to_csv("data/"+ date_of_file +"/xcov19ivavDAG.csv", sep=',', index=False, encoding="utf-8")
        print("Done with " + date_of_file)
    
def convert_to_acov19dag(df_excel: pd.DataFrame):
    """Convert to type of acov19DAG.csv
    Parameters:
        df_excel: dataframe of sheet "Antal per dag region"

    Returns:
        result: dataframe of type acov19dag.csv
    """

    #Melt the DataFrame - Converts from wide format to long format
    df_melted = pd.melt(df_excel, id_vars=['Statistikdatum'], var_name='Region', value_name='Value')
    #The Excelfile has Västra Götaland as "Västra_Götaland"
    df_melted["Region"] = df_melted["Region"].replace("Västra_Götaland", "Västra Götaland")
    #The excelfile has Jämtland Härjedalen as "Jämtland_Härjedalen"
    df_melted["Region"] = df_melted["Region"].replace("Jämtland_Härjedalen", "Jämtland")
    #The excelfile has Södermanland as "Sörmland"
    df_melted["Region"] = df_melted["Region"].replace("Sörmland", "Södermanland")
    #Create a new DataFrame with Date and Types columns
    result = df_melted.groupby(['Region','Statistikdatum'])['Value'].sum().reset_index()
    #Change name of columns corresponding to csv file
    result.columns = ["Region", "Dag", "Fall per dag"]
    #Replace in order to get the same name as in csv file
    result["Region"] = result["Region"].replace("Totalt_antal_fall", "Riket")
    return(result)

def convert_to_xcov19ivavDAG(df_avlidna : pd.DataFrame, df_intensiv_vard : pd.DataFrame):
    """ Combine two dataframes and melt them to one dataframe in form of xcov19ivavDAG.csv
    Parameters:
        df_avlidna: dataframe of sheet "Antal avlidna per dag"
        df_intensiv_vard: dataframe of sheet "Antal intensivvårdade per dag" 
    
    Returns:
        result: dataframe of type xcov19ivavDAG.csv"""
    #Clean rows
    df_avlidna = clean_rows(df_avlidna)
    df_intensiv_vard = clean_rows(df_intensiv_vard)
    #Change name of columns corresponding to the one in csv file
    df_avlidna.columns = ["Dag", "Antal avlidna fall"]
    df_intensiv_vard.columns = ["Dag", "Antal intensivvårdade fall"]
    #Use all observations except last row and convert to date
    df_avlidna["Dag"] = pd.to_datetime(df_avlidna["Dag"].iloc[:-1]).dt.date
    #Convert to date
    df_intensiv_vard["Dag"] = pd.to_datetime(df_intensiv_vard["Dag"]).dt.date
    #Combine two dataframes
    df_merged = df_intensiv_vard.merge(df_avlidna, on=["Dag"], how="outer")
    #For column "Dag" and value Uppgift saknas replace value with temporary date
    df_merged["Dag"] = df_merged["Dag"].replace("Uppgift saknas", pd.to_datetime("2040-10-26").date())
    #When we are merging, the number of rows differ which creates NaN values.
    df_merged["Dag"] = df_merged["Dag"].fillna(pd.to_datetime("2040-10-26").date())
    df_merged = df_merged.fillna(0)
    #Melt the DataFrame - Converts from wide format to long format
    df_melted = pd.melt(df_merged, id_vars=['Dag'], var_name='Indikator', value_name='Intensivvårdade respektive avlidna per dag')
    #Create a new DataFrame with Date and Types columns
    result = df_melted.groupby(['Indikator','Dag'])['Intensivvårdade respektive avlidna per dag'].sum().reset_index()
    result["Indikator"] = pd.Categorical(result["Indikator"],categories=["Antal intensivvårdade fall","Antal avlidna fall"])
    #Sort by values
    result=result.sort_values(["Indikator","Dag"]).reset_index(drop=True)
    #Replace temporary date with "Okänd tidpunkt"
    df_merged["Dag"] = df_merged["Dag"].replace("2040-10-26", "Okänd tidpunkt")
    return(result)

def convert_to_ccov19regsasong(df_reg: pd.DataFrame):
    """Convert to type of ccov19regsasong.csv
    Parameters:
        df_reg: dataframe of sheet "Veckodata Region" 
    
    Returns:
        result: dataframe of type ccov19regsasong.csv"""
    #Clean rows
    df_reg = clean_rows(df_reg)
    #The Years counts to week 25. --> 2019-2020 is til 2020 week 25.
    #all values from week 26 to 2021 week 25 is 2020-2021
    df_reg["år"] = df_reg.apply(lambda x: x["år"] if x["veckonummer"] <= 25 else x["år"]+1, axis=1)
    #To get 3 --> 03, 4 --> 04 etc.
    df_reg["veckonummer"] = df_reg["veckonummer"].apply(lambda x: ("0" + str(x)) if len(str(x)) == 1 else x)
    #To get V 03, V 04 etc.
    df_reg["veckonummer"] = "v " + df_reg["veckonummer"].astype(str)
    #To get years in format 2019-2020, 2020-2021 etc.
    df_reg["år"] = (df_reg["år"].astype(int)-1).astype(str) + "-" + (df_reg["år"].astype(int)).astype(str)
    #Create new dataframe with specific chosen columns
    df_reg = df_reg[["år", "veckonummer", "Region", "Antal_fall_vecka", "Antal_intensivvårdade_vecka", "Antal_avlidna_vecka"]]
    #Change column names
    df_reg.columns = ["År", "Vecka", "Region", "Antal fall", "Antal intensivvårdade fall", "Antal avlidna fall"]
    #Melt the DataFrame - Converts from wide format to long format
    df_melted = pd.melt(df_reg, id_vars=['År', 'Vecka', 'Region'], var_name='Indikator', value_name='Fall, intensivvårdade och avlidna efter region och vecka (säsongsvis).')
    #Groupby
    result = df_melted.groupby(["Region", "Indikator", "Vecka","År"])['Fall, intensivvårdade och avlidna efter region och vecka (säsongsvis).'].sum().reset_index()
    #Change name of files to get the in them same type as in txt file
    result["Region"] = result["Region"].replace("Jämtland Härjedalen", "Jämtland")
    result["Region"] = result["Region"].replace("Sörmland", "Södermanland")
    #Create a new dataframe for riket
    riket = result.groupby(["Indikator", "Vecka","År"])['Fall, intensivvårdade och avlidna efter region och vecka (säsongsvis).'].sum().reset_index()
    #Add riket to results as a new region in dataframe
    riket["Region"] = "Riket"
    #Combine two dataframes
    result = pd.concat([result, riket], ignore_index=True)
    #Sort in correct order 
    result["Region"] = pd.Categorical(result["Region"], categories = regions_dict_according_ccov19kon.values(), ordered=True)
    result["Indikator"] = pd.Categorical(result["Indikator"], categories = indicator_ccov19Regsasong_dict.values(), ordered=True)
    #Sort results
    result = result.sort_values(["Region", "Indikator", "Vecka", "År" ]).reset_index(drop=True)
    return(result)

def convert_to_bcov19kom(df: pd.DataFrame):
    """Convert to type of bcov19Kom.csv
    Parameters:
        df: dataframe of sheet "Veckodata Kommun_stadsdel"
    
    Returns:
        result: dataframe of type bcov19kom.csv
    """
    #Clean rows
    df = clean_rows(df)
    #Convert some columns to specific type as in the txt file
    df["KnKod"] = df["KnKod"].replace("Okänd", "9999")
    df["KnNamn"] = df["KnNamn"].replace("Malung", "Malung-Sälen")
    df["Kommun"] = df["KnKod"] + " " + df["KnNamn"]
    #Replace <15 to get numeric (These values are interpolated later on)
    df["nya_fall_vecka"] = df["nya_fall_vecka"].replace("<15", np.nan)
    df["nya_fall_vecka"] = df["nya_fall_vecka"].astype(float)
    #Add 0 if veckonummer is a single number
    df["veckonummer"] = df["veckonummer"].apply(lambda x: ("0" + str(x)) if len(str(x)) == 1 else x)
    #Combine in a new column "År och vecka" the year and week number
    df["År och vecka"] = df["år"].astype(str) + " v " + df["veckonummer"].astype(str)
    #Create new dataframe with specific chosen columns
    df_kommun_stadsdel = df[["Kommun", "År och vecka", "nya_fall_vecka"]]
    #Change column names
    df_kommun_stadsdel.columns = ["Kommun", "År och vecka", "Antal fall"]
    #Group by Kommun and År och vecka and sum the values
    df_kommun_stadsdel = df_kommun_stadsdel.groupby(["Kommun", "År och vecka"])[["Antal fall"]].sum().reset_index()
    #Melt the DataFrame - Converts from wide format to long format
    df_melted = pd.melt(df_kommun_stadsdel, id_vars=["Kommun", "År och vecka"], var_name='Indikator', value_name='Fall efter kommun och vecka (tidsserie).')
    #Groupby
    result = df_melted.groupby(["Kommun", "Indikator", "År och vecka"])['Fall efter kommun och vecka (tidsserie).'].sum().reset_index()
    return(result)

def convert_to_ccov19reg(df: pd.DataFrame):
    """Convert to type of ccov19reg.csv
    Parameters:
        df: dataframe of sheet "Veckodata Region"

    Returns:
        result: dataframe of type ccov19reg.csv
    """
    #Clean rows
    df = clean_rows(df)
    #Add 0 if veckonummer is a single number
    df["veckonummer"] = df["veckonummer"].apply(lambda x: ("0" + str(x)) if len(str(x)) == 1 else x)
    #Combine in a new column "År och vecka" the year and week number
    df["År och vecka"] = df["år"].astype(str) + " v " + df["veckonummer"].astype(str) 
    #Create new dataframe with specific chosen columns
    df = df[["År och vecka", "Region", "Antal_fall_vecka", "Antal_intensivvårdade_vecka", "Antal_avlidna_vecka"]]
    #Change column names
    df.columns = ["År och vecka","Region", "Antal fall", "Antal intensivvårdade fall", "Antal avlidna"]
    #Melt the DataFrame - Converts from wide format to long format
    df_melted = pd.melt(df, id_vars=["År och vecka", "Region"], var_name='Indikator', value_name='Bekräftade fall')
    #Groupby
    result = df_melted.groupby(["Region", "Indikator", "År och vecka"])['Bekräftade fall'].sum().reset_index()
    #Change name of files to get the in them same type as in txt file
    result["Region"] = result["Region"].replace("Jämtland Härjedalen", "Jämtland")
    result["Region"] = result["Region"].replace("Sörmland", "Södermanland")
    #Create a new dataframe for riket
    riket = result.groupby(["Indikator", "År och vecka"])['Bekräftade fall'].sum().reset_index()
    #Add riket to results as a new region in dataframe
    riket["Region"] = "Riket"
    #Combine two dataframes
    result = pd.concat([result, riket], ignore_index=True)
    #Sort in correct order  
    result["Region"] = pd.Categorical(result["Region"], categories = regions_dict_according_ccov19kon.values(), ordered=True)
    result["Indikator"] = pd.Categorical(result["Indikator"], categories = indicator_ccov19Reg_dict.values(), ordered=True)
    #Sort results
    result = result.sort_values(["Region", "Indikator", "År och vecka"]).reset_index(drop=True)
    return(result)

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        if sys.argv[1] == "excel":
            get_excel_files()
        elif sys.argv[1] == "csv":
            get_csv_files()
        else:
            print("Invalid argument. Use 'excel' or 'csv'")