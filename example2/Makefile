all: vectors_cpu vectors_gpu

vectors_cpu: main.f90 vector_ops.f90 timer.o
	pgf90 -Mpreprocess -O3 vector_ops.f90 main.f90 timer.o -o vectors_cpu

vectors_gpu: main.f90 vector_ops.f90 timer.o
	pgf90 -Mpreprocess -Mcuda -O3 vector_ops.f90 main.f90 timer.o -o vectors_gpu

timer.o: timer.c
	gcc -c timer.c

clean:
	rm *.o *.mod vectors_cpu vectors_gpu
