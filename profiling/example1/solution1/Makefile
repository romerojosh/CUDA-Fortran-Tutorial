CUDA_HOME=/opt/pgi/linux86-64/2018/cuda/9.1
all: main

main: main.f90 timer.o
	pgf90 -Mpreprocess -Mcuda=cuda9.1,cc70 -Mcudalib=cublas -DUSE_NVTX nvtx.f90 main.f90 timer.o -o main -lblas -L${CUDA_HOME}/lib64 -lnvToolsExt

timer.o: timer.c
	gcc -c timer.c

clean:
	rm *.o main
