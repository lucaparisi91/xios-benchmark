import pandas as pd
import re

def extract_performance_xios_output(xios_output_file) -> pd.DataFrame :
    """ Extract performance metrics from the xios output file.
    Examples of lines to parse metrics from:

    Time send/recv:    1.9638779999695544E-003
    Initialise:    5.6166085000313615E-002
    Finalise:    1.9923395409996374     

    :param xios_output_file: open file object for the xios output file
    :return: pandas DataFrame containing the performance metrics
    
    """
    metrics = ["Time send/recv", "Initialise", "Finalise"]
    data = {metric: [] for metric in metrics}

    for line in xios_output_file.readlines():
        for metric in metrics:
            pattern = rf"\s*{metric}:\s+(\d+\.\d+(?:E[+-]\d+)?)"
            m = re.match(pattern, line)
            if m:
                value = float(m.group(1) )
                data[metric].append(value)

    return pd.DataFrame(data)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Extract performance metrics from xios output file")
    parser.add_argument("xios_output_file", type=str, help="Path to the xios output file")
    parser.add_argument("--keys", type=str, nargs="*", default=[], help="List of keys to add as columns to the output dataframe")
    parser.add_argument("--values", type=str, nargs="*", default=[], help="List of values to add as columns to the output dataframe")
    
    
    args = parser.parse_args()

    assert len(args.keys) == len(args.values), "The number of keys and values must be the same"
    
    with open(args.xios_output_file, "r") as f:
        df = extract_performance_xios_output(f)

        for key, value in zip(args.keys, args.values):
            df[key] = value
        print(df)