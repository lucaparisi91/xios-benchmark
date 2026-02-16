import pandas as pd
import sys

def collect_results(shared_dir) -> pd.DataFrame:
    """ Aggregate performances metrics from xios benchmark runs into a single file.

    Find all perofrmance.txt files in the shared directory, load them as pandas dataframes, concatenate them and save the resulting dataframe in the shared directory.

    :shared_dir: path to the shared directory where the performance metrics are stored

    """

    import pandas as pd
    import glob
    import os

    performance_files = glob.glob(os.path.join(shared_dir, "xios_benchmark_*/performance.txt"))
    dataframes = []
    for performance_file in performance_files:
        df = pd.read_csv(performance_file, delimiter=r"\s+")
        dataframes.append(df)

    if len(dataframes) > 0:
        result_df = pd.concat(dataframes, ignore_index=True)
        return result_df

    return pd.DataFrame()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Collect performance metrics from xios benchmark runs")
    parser.add_argument("shared_dir", type=str, help="Path to the shared directory where the performance metrics are stored")
    parser.add_argument("--output_file", type=str, help="Path to the output file where the collected performance metrics will be saved")
    
    args = parser.parse_args()

    df = collect_results(args.shared_dir)

    # If output file is specified, save the collected performance metrics to stdout
    output=args.output_file

    
    if args.output_file is None:
        output=sys.stdout
    
    
    df.to_csv(output, index=False, sep=" ")