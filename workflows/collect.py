import pandas as pd
import os
import glob

def collect(folder: str, data_set: str, parameters: str) -> pd.DataFrame:
    """
    For each subfolder , find all data files matching the data_set pattern and annotate with the data in the parameters data.
    Concatenate all annotated dataframes and return the final dataframe.

    :param folder: Where to look for the data files
    :param data_set: Filename pattern containing data. The datasets are concatenated.
    :param parameters: Parameters are added as columns to the dataframes.
    :type parameters: str
    :return: The final concatenated dataframe
    :rtype: pd.DataFrame
    """
    
    dataframes = []

    # Loop over all subfolders in the folder and find data files matching the data_set pattern
    for subfolder in os.listdir(folder):
        subfolder_path = os.path.join(folder, subfolder)
        if os.path.isdir(subfolder_path):
            data_files = glob.glob(os.path.join(subfolder_path, data_set))
            if data_files:
                parameters_data = pd.read_csv(os.path.join(subfolder_path, parameters), sep=r"\s+")

                data=pd.concat([pd.read_csv(data_file, sep=r"\s+") for data_file in data_files])

                for column in parameters_data.columns:
                    if column not in data.columns:
                        data[column] = parameters_data[column].iloc[0]

                dataframes.append(data)
    

    # Concatenate all dataframes and return the final dataframe. If no dataframes are found, return None
    dataframes= pd.concat(dataframes, ignore_index=True) if len(dataframes) > 0 else None
    return dataframes

if __name__ == "__main__":
    import argparse
    import sys

    parser = argparse.ArgumentParser(description="Collect data from multiple files in a folder, annotate with parameters and concatenate into a single dataframe.")
    parser.add_argument("folder", help="Where to look for experiment subfolders")
    parser.add_argument("data_file", help="Filename pattern containing data. Can be a glob pattern. The found datasets are concatenated.")
    parser.add_argument("parameters_file", help="Dataset with a single row. The rows are joined with the data for each subfolder.")
    parser.add_argument("--output_file", default=sys.stdout, help="Path to save the final dataframe as a CSV file. If not provided, the dataframe will be printed to standard output.")

    args = parser.parse_args()

    df = collect(args.folder, args.data_file, args.parameters_file)
    
    if df is not None:
        df.to_csv(args.output_file, index=False, sep=" ")