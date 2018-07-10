# Example 1: Simple DGEMM Test
The following program contains a loop with square DGEMM calls of increasing dimension. In each loop iteration, the following tasks are performed:
1. Allocate/Initialize matrices A, B and C
2. Perform DGEMM (A * B + C)
3. Check result and report time/FLOPs for current iteration

We will modify the code to do the same operations on the GPU. Key concepts from this example are:
1. The `device` variable attribute
2. Generic interface to BLAS (with `cublas` module)
3. Asynchronous kernel execution
4. Minimizing code replication via `_CUDA` compiler define and associate blocks
