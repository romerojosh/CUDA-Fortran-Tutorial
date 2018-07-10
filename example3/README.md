# Example 3: Simpson integration
The following program completes a number of integrals using Simpson's rule. In this example, the
integration grids are quite small (~800 samples), in comparison to the number of integrals (~200k) to be computed. 
Examples of this operation can be found in several routines in Quantum Espresso.

We will modify the code to do the same operations on the GPU. Key concepts from this example are:
1. Limitations of CUF kernels
2. Writing your own CUDA kernels
3. Using the !@cuf sentinel in place of the `_CUDA` define for conditional compilation
