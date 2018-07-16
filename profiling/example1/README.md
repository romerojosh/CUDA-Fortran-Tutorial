# Example 1: CPU and GPU DGEMMS
The following program computes 2 CPU DGEMMS followed by 200 GPU DGEMMS. 

We will use this code to show how to use NVPROF and NVTX. Key concepts from this example are:
1. Using NVTX markers to annotate code, including CPU code 
2. Ordering operations to obtain GPU/CPU concurrency
