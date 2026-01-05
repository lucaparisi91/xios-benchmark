#include <mpi.h>
#include <sched.h>
#include <vector>
#include <iostream>
#include <thread>

// Function to get the CPU affinity of the current thread
std::vector<int> get_thread_affinity()
{
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    
    // Get the affinity of the current thread
    if (sched_getaffinity(0, sizeof(cpu_set_t), &cpuset) == -1) {
        std::cerr << "Error getting CPU affinity" << std::endl;
        return std::vector<int>();
    }
    
    // Convert the cpu_set to a vector of CPU IDs
    std::vector<int> affinity;
    for (int cpu = 0; cpu < CPU_SETSIZE; cpu++) {
        if (CPU_ISSET(cpu, &cpuset)) {
            affinity.push_back(cpu);
        }
    }
    
    return affinity;
}

int main(int argc , char **argv)
{
    int rank, nRanks;

    MPI_Init(&argc, &argv);
    
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nRanks);

    
    auto affinites = get_thread_affinity();
    std::cout << "Rank " << rank << " Thread affinity CPUs: ";
    for (const auto& cpu : affinites) {
        std::cout << cpu << " ";
    }
    std::cout << std::endl;
        

    MPI_Finalize();
    return 0;
}