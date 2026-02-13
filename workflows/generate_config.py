import f90nml
import copy

def get_default_namelist():
    nml_default = {
        "info": {
            "n_proc": [1, 28, 28, 6],
            "shape": [70, 896, 896, 6],
            "nfields": 4,
            "field_base_name": "data4D",
            "file_name": "data/axis",
            "operation": "read",
            "check": False,
            "par_access": "collective",
            "log": 1,
            "nfiles": 1,
            "warmup_operations": 0
        }
    }

    return copy.deepcopy(nml_default)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Generate config file for xios benchmark')
    parser.add_argument('--nfields', type=int, default=4, help='Number of fields to process')
    parser.add_argument('--operation', type=str, default="read", help='Operation to perform (read/write)')
    parser.add_argument('--output', type=str, default="config.nml", help='Output namelist file name')
    
    args = parser.parse_args()
    
    # Update the default namelist with the provided arguments
    nml = get_default_namelist()
    nml["info"]["nfields"] = args.nfields
    nml["info"]["operation"] = args.operation
    f90nml.write(nml, args.output)