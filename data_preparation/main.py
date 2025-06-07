from format_data import *
from transformations import *
from convert_to_csv import *
#from collect_data import *
from interpolation import *

def main():
    #Converts excel files to csv files
    #get_excel_files()
    
    #Converts text files to csv files
    #get_csv_files()

    #Collects data from the API and saves it as a csv file
    #collect_data_from_API()
    
    #Interpolates values for the bcov19Kom.csv file
    #read_data_and_interpolate()

    #Created the transformed_data and saves it as a csv file
    find_differences()
    
if __name__ == "__main__":
    main()