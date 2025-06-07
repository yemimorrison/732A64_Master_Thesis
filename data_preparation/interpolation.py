from defined import *
import pandas as pd
import numpy as np
from convert_to_csv import convert_to_bcov19kom

def read_excel():
    """Read specific excel file for interpolation
    
    Returns:
        df_excel: dataframe of the excelfile
    """
    df_excel = pd.read_excel("excel_for_interpolation/Folkhalsomyndigheten_Covid19_Apr 01 2022.xlsx", sheet_name=None)
    return df_excel

def read_data_and_interpolate():
    """Interpolates values for the csv file for each Kommun_stadsdel"""
    df = read_excel()
    df_kommun_stadsdel = df["Veckodata Kommun_stadsdel"].copy()
    #For each group in df interpolate <15 values
    #  df group by Kommun_stadsdel
    df_kommun_stadsdel = df_kommun_stadsdel.groupby(["Kommun_stadsdel"], group_keys=False)
    new_df = df_kommun_stadsdel.apply(average_interpolation_total_bcov19Kom).reset_index(drop=True)
    #For all bcov19Kom files, if there are .., change them to the new_df values
    list_of_folder = sorted(folders, key=lambda x: int(x), reverse=True)
    tmp_new_df = convert_to_bcov19kom(new_df)
    for folder in list_of_folder:
        #Try to read the file from the folder else continue
        try:
            df_2 = pd.read_csv(f"data/{folder}/bcov19Kom.csv", sep=',', encoding='utf-8')
        except FileNotFoundError:
            print(f"File {folder} does not exist")
            continue
        # Replace '..' with NaN in df_2
        df_2.replace('..', np.nan, inplace=True)
        # Merge df_2 with tmp_new_df using a common key
        # Replace 'key_column' with the actual column name(s) that you want to merge on
        merged_df = df_2.merge(tmp_new_df, on=["Kommun","Indikator","År och vecka"], how='left', suffixes=('', '_tmp'))
        # Fill NaN values in df_2 columns using values from tmp_new_df columns
        for col in df_2.columns:
            if col + '_tmp' in merged_df:
                merged_df[col].fillna(merged_df[col + '_tmp'], inplace=True)
        # Select only the original columns from df_2
        df_2 = merged_df[df_2.columns]
        df_2.to_csv(f"data/{folder}/bcov19Kom.csv", index=False)
        print(f"Interpolated values for {folder}")

def average_interpolation_total_bcov19Kom(df):
    """
    Average interpolation of new cases for total cases per day in order to interpolate <15 values.
    df: grouped dataframe by Kommun_stadsdel with group_keys=False.
    returns: grouped dataframe by Kommun_stadsdel with interpolated values.
    """
    #Get na values
    df["tot_antal_fall"] = df["tot_antal_fall"].replace("<15", np.nan)
    df["nya_fall_vecka"] = df["nya_fall_vecka"].replace("<15", np.nan)
    #reset index
    df = df.reset_index(drop=True)
    #Get indexes of na values
    na_index_list = list(df.loc[pd.isna(df["tot_antal_fall"]), :].index)
    if len(na_index_list) == 0:
        return df
    #Get max value
    max_val = int(df.iat[int(max(na_index_list))+1, 8]) - int(df.iat[int(max(na_index_list))+1, 9])
    #Get number of na values
    length_na_values = len(na_index_list)
    #fillna
    df["nya_fall_vecka"] = df["nya_fall_vecka"].fillna(max_val/length_na_values)
    tmp = 0
    #For each value in y0, check if the difference between the current and the next value is smaller than 1.
    #This is created in order to not get values that are larger than the total number of cases.
    tmp_sum = 0
    #For the values that are interpolated
    for value in range(min(na_index_list), max(na_index_list)+1):
        #If the value + tmp value holder is smaller than 1
        #Add the tmp value holder to the value and set the number of cases to 0
        if round(float(df.iat[value,9]) + tmp) < 1:
            tmp += float(df.iat[value,9])
            df.iat[value,9] = 0
        #If there are more or equal to 1 case
        elif round(float(df.iat[value,9]) + tmp) >= 1:
            #If the difference is close to 1, add the tmp value holder to the value and set the tmp value holder to 0
            if abs((round(float(df.iat[value,9]) + tmp)) - float(df.iat[value,9])) <= 0.01:
                df.iat[value,9] = round(float(df.iat[value,9]) + tmp)
                tmp = 0
            #Else set the value to the difference between the value and the tmp value holder
            #Set the tmp value holder to the difference between the value and the tmp value holder
            else:
                diff = round(float(df.iat[value,9]) + tmp)
                tmp += float(df.iat[value,9]) - diff
                df.iat[value,9] = diff
        tmp_sum += df.iat[value,9]
    return df
    