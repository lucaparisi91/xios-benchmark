import pandas as pd
import re
import argparse
import sys

def annotate_dataframe(data, keys, values):
    """
    Annotate a dataframe by adding new columns. The column names are taken from the keys argument.
    Each column is set to the same value for all rows in the dataframe. The values for the new columns are taken from the values argument, where each value is associated with the corresponding column in the keys argument.
    
    :param data: Pandsas DataFrame to annotate
    :param keys: vector of names to add as columns
    :param values: vector of values, each associated with the column in the keys vector.
    """

    assert len(keys) == len(values), "The number of keys and values must be the same"
    
    for key, value in zip(keys, values):
        data[key] = value
    
    return data

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Annotate a dataframe by adding new columns with the same value for all rows")
    parser.add_argument("--data_files", type=str, nargs="+", help="Path to the input data files (dsv format)")
    parser.add_argument("--keys", type=str, nargs="+", help="Names of the columns to add")
    parser.add_argument("--values", type=str, nargs="+", help="Values for the new columns")
    parser.add_argument("--annotations_dataframe", help="Path to a dataframe with one row")
    parser.add_argument("--output_file", type=str, default=sys.stdout, help="Path to the output data file (dsv format)")

    args = parser.parse_args()
        
    annotated_dataframes = []

    data_files = args.data_files if args.data_files else [None]

    for file in data_files:
        if file is not None:
            data = pd.read_csv(file, sep=r"\s+")
        else: # Create an empty dataframe with a single row to annotate if no data file is provided
            data = pd.DataFrame(index=[0]) 
        
        if args.keys and args.values:
            
            annotated_data = annotate_dataframe(data, args.keys, args.values)
        else:
            annotated_data = data
        
        # Annotate based on input dataframe
        if args.annotations_dataframe:
            annotations_df = pd.read_csv(args.annotations_dataframe, sep=r"\s+")
            for column in annotations_df.columns:
                if column not in annotated_data.columns:
                    annotated_data[column] = annotations_df[column].iloc[0]

        annotated_dataframes.append(annotated_data)
    
    if len(annotated_dataframes) > 0:
        final_dataframe = pd.concat(annotated_dataframes, ignore_index=True)
        final_dataframe.to_csv(args.output_file, index=False, sep=" ")