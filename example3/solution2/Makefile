all: simpson_cpu simpson_gpu

simpson_cpu: main.f90 timer.o
	pgf90 -Mpreprocess -O3 main.f90 timer.o -o simpson_cpu

simpson_gpu: main.f90 timer.o
	pgf90 -Mpreprocess -Mcuda=cuda9.1,cc70 -O3 main.f90 timer.o -o simpson_gpu

timer.o: timer.c
	gcc -c timer.c

clean:
	rm *.o *.mod simpson_cpu simpson_gpu
