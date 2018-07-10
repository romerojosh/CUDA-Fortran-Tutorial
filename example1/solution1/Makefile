all: dgemm_cpu dgemm_gpu

dgemm_cpu: dgemm_test.f90 timer.o
	pgf90 -Mpreprocess dgemm_test.f90 timer.o -o dgemm_cpu -lblas

dgemm_gpu: dgemm_test.f90 timer.o
	pgf90 -Mpreprocess -Mcuda -Mcudalib=cublas dgemm_test.f90 timer.o -o dgemm_gpu -lblas

timer.o: timer.c
	gcc -c timer.c

clean:
	rm *.o dgemm_cpu dgemm_gpu
