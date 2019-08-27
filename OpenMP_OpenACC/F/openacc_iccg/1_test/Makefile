F90   = pgfortran
F90OPTFLAGS= -O3 -acc -Minfo=accel -ta=tesla:cc60,managed
F90FLAGS =$(F90OPTFLAGS)

.SUFFIXES:
.SUFFIXES: .o .f .f90 .c 
#
.f90.o:; $(F90) -c $(F90FLAGS)  $(F90OPTFLAG) $<
.f.o:; $(F90) -c -loglist $(F90FLAGS)  $(F90OPTFLAG) $<
#
OBJS = \
solver_ICCG_mc.o cm.o cmrcm.o rcm.o struct.o pcg.o \
boundary_cell.o cell_metrics.o \
input.o main.o poi_gen.o pointer_init.o outucd.o mc.o

TARGET = run

all: $(TARGET)

$(TARGET): $(OBJS) 
	$(F90) $(F90FLAGS) -o $(TARGET) \
	$(OBJS) \
	$(F90FLAGS)

clean:
	rm -f *.o $(TARGET) *.mod *~ PI* *.log *.lst
