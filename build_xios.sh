
OPT="debug"
COMPILER="GNU"


# load compiler specific modules

if [ "$COMPILER" = "GNU" ]; then
    module load PrgEnv-gnu
endif


module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel/4.9.0.17


# Clone XIOS repository
git clone https://gitlab.in2p3.fr/ipsl/projets/xios-projects/xios.git xios3

# Set architecture dependent configuration files
cp -r arch/* xios3/arch/

# Build XIOS
cd xios3
./make_xios --debug --arch GCC_CRAY_EX4000 --job 16
