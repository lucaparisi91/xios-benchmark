source $(pwd)/../benchmark/cylc_env/bin/activate
export HOME="/work/z19/shared/lparisi/xios-benchmark/runs" # Home needs to point to the root of the cylc-run directory.
export CYLC_SITE_CONF_PATH=$(pwd)  # Point to the root of flow/global.cylc file.
