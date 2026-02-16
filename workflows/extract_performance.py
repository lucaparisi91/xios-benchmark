import sys
import pandas as pd
import re
import argparse


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
    
    parser = argparse.ArgumentParser(description="Extract performance metrics from xios output file")
    parser.add_argument("xios_output_file", type=str, help="Path to the xios output file")
    parser.add_argument("--output_file", type=str, default=sys.stdout, help="Path to the output data file (dsv format)")
    args = parser.parse_args()
    
    with open(args.xios_output_file, "r") as f:
        df = extract_performance_xios_output(f)

        
        df.to_csv(args.output_file, sep=" ", index=False)