all: vectors_cpu vectors_gpu

vectors_cpu: main.f90 vector_ops.f90 vector_ops_routines_cpu.o timer.o
	pgf90 -Mpreprocess -O3 vector_ops.f90 main.f90 vector_ops_routines_cpu.o timer.o -o vectors_cpu

vectors_gpu: main.f90 vector_ops.f90 vector_ops_routines_cpu.o vector_ops_routines_gpu.o timer.o
	pgf90 -Mpreprocess -Mcuda=cuda9.1,cc70 -O3 vector_ops.f90 main.f90 vector_ops_routines_cpu.o vector_ops_routines_gpu.o timer.o -o vectors_gpu

vector_ops_routines_cpu.o: vector_ops_routines.f90
	pgf90 -Mpreprocess -O3 -c vector_ops_routines.f90 -o vector_ops_routines_cpu.o

vector_ops_routines_gpu.o: vector_ops_routines.f90
	pgf90 -Mpreprocess -Mcuda=cuda9.1,cc70 -O3 -c vector_ops_routines.f90 -o vector_ops_routines_gpu.o

timer.o: timer.c
	gcc -c timer.c

clean:
	rm *.o *.mod vectors_cpu vectors_gpu
