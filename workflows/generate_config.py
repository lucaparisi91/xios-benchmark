import f90nml
import copy
import tools
from tools.decomp import get_optimal_decomposition
import numpy as np
import sys

def get_default_namelist():
    nml_default = {
        "info": {
            "n_proc": [1, 2, 2, 1],
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
    parser.add_argument('--output', type=str, default=sys.stdout, help='Output namelist file name')
    parser.add_argument('--client_processes', type=int, default=6, help='Number of client processes')
    
    args = parser.parse_args()
    
    # Update the default namelist with the provided arguments
    nml = get_default_namelist()
    nml["info"]["nfields"] = args.nfields
    nml["info"]["operation"] = args.operation

    # Calculate the optimal decomposition for the given number of client processes

    shape=nml["info"]["shape"]
    domain_shape=shape[1:3] # The second and third rank are the spatial dimensions
    assert args.client_processes % 6 == 0, "Number of client processes must be a multiple of 6 (the number of panels in a cubed Sphere)"

    proc_decomp= [1] + list(get_optimal_decomposition(domain_shape, args.client_processes//6)) + [6]

    nml["info"]["n_proc"] = proc_decomp

    f90nml.write(nml, args.output)