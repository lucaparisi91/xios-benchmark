
from itertools import product
from typing import List
import numpy as np
import argparse

def divisor_generator(n): 
    """Create a generator that yields the divisors of n.""" 
    
    for i in range(2, n): 
        if n % i == 0: 
            yield i

def composition_cost( shape: List[int] , decomp: List[int]) -> float:
    """Calculate the amount of imbalance when the decomposition decomp is applied to a data array of shape `shape`. Uses entropy measure to measure the amount of imbalance between different components.

    ::param [shape]: the shape of the data array
    :: param [decomp]: how many parts should the corresponding data array dimension be decomposed into(e.g. decomp[0] = 4 means that the first dimension of the data array should be decomposed into 4 parts)
    :return: the cost of the decomposition, which is a measure of how imbalanced the decomposition is. A lower cost indicates a more balanced decomposition.
    
    """
    cost=0
    nelements=np.prod(shape)
    for i in range(len(shape)):
        pi=(shape[i]/decomp[i])/nelements

        cost+=-pi*np.log(pi)

    return cost
    

def get_optimal_decomposition(shape: List[int], n: int) -> List[int]:
    """
    Returns an optimal decomposition of the data array of shape `shape` into `n` parts, where the cost of the decomposition is minimized. The cost is calculated using the `composition_cost` function, which measures how imbalanced the decomposition is. A lower cost indicates a more balanced decomposition.
    
    :param shape: Shape od the data array
    :type shape: List[int]
    :param n: Numper of parts to decompose the data array into
    :type n: int
    :return: Optimal decomposition
    :rtype: List[int]
    """

    optimal_decomp=None
    min_cost = np.inf
    for i in divisor_generator(n):
        i1=i 
        i2=n//i
        #print(f"{i1} x {i2} with cost {composition_cost(shape, [i1, i2])}")
        
        cost=composition_cost(shape, [i1, i2])
        if cost < min_cost:
            optimal_decomp=[i1, i2]
            min_cost=cost
    optimal_decomp=np.sort(optimal_decomp) # Larger decomposition should be outer
    #print(f"Optimal decomposition: {optimal_decomp} with cost {min_cost}")
    
    return optimal_decomp

    

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Calculate the optimal decomposition of a data array into n parts")
    parser.add_argument("n", type=int, help="Number of parts to decompose the data array into (default: 288*4)")
    parser.add_argument("shape",type=int ,nargs="+", help="Shape of the data array)")

    args = parser.parse_args()

    n=args.n
    shape=args.shape
    optimal_decomp=get_optimal_decomposition(shape=[896,896], n=288*4)
    
    print(",".join([ str(decomp) for decomp in optimal_decomp]) )


