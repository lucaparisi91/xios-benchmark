# XIOS benchmarks

Contains scripts and code to benchmark xios across different versions and branches on Cirrus-ex ( a Cray EX-4000) system.
The `benchmark` directory contains the code and some submission scripts. The `build_xios.sh` script can be used to build , with options given as shell environment variables at the top of the scrips. The `xios3_dev` spack environment contains dependencies needed to build, run and profile the benchmark.