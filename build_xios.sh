set -e 
set -x

OPT="dev"
COMPILER="GNU"
DOWNLOAD=0
BRANCH="main"

# load compiler specific modules
set +x # Disable command echoing to limit noise 
if [ "$COMPILER" = "GNU" ]; then
    module load PrgEnv-gnu
fi


module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel/4.9.0.17
set -x # Re-enable command echoing after loading modules


XIOS_DIR=$BRANCH
if [ $BRANCH = "main" ];
then
    XIOS_DIR="xios-3"
fi

# Clone XIOS repository if DOWNLOAD is set
if [ $DOWNLOAD -eq 1 ];
then
    git clone -b $BRANCH https://gitlab.in2p3.fr/ipsl/projets/xios-projects/xios.git $XIOS_DIR
    # Set architecture dependent configuration files
    cp -r arch/* $XIOS_DIR/arch/
fi


# Build XIOS
cd $XIOS_DIR
./make_xios --$OPT --arch GCC_CRAY_EX4000 --job 16
