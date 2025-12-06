# Fortran compiler
FC = gfortran

# Compiler flags
FFLAGS = -O2 -Wall -fcheck=bounds

# Executable location
EXEC = ../bin/cda

# Source files
SOURCES = kinds.f95 cubes.f95 cda.f95

# Object files
OBJECTS = kinds.o cubes.o cda.o

# Default target
all: $(EXEC)

# Link to create executable
$(EXEC): $(OBJECTS)
	$(FC) $(FFLAGS) -o $(EXEC) $(OBJECTS)
	@echo "Compilation successful! Executable: $(EXEC)"

# Compile modules with dependencies
kinds.o: kinds.f95
	$(FC) $(FFLAGS) -c kinds.f95

cubes.o: cubes.f95 kinds.o
	$(FC) $(FFLAGS) -c cubes.f95

cda.o: cda.f95 kinds.o cubes.o
	$(FC) $(FFLAGS) -c cda.f95

# Clean compiled files
clean:
	rm -f *.o *.mod $(EXEC)
	@echo "Cleaned all object files and executables"

# Rebuild from scratch
rebuild: clean all

.PHONY: all clean rebuild
