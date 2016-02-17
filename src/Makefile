
include ../make.inc

LINKS = \
-I$(OBJ_DIRECTORY) \
-I$(LIB_DIRECTORY)

EXECUTABLE = ./build.exe

module_allocate.o: \
module_allocate.f90
	$(FC) $(FFLAGS) -c module_allocate.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(LIB_DIRECTORY)

main.o: \
module_allocate.o \
main.f90
	$(FC) $(FFLAGS) -c main.f90 $(LINKS) \
	-o $(OBJ_DIRECTORY)/$@

exec: $(OBJECTS)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJECTS_WITH_PREFIX) $(LINKS)

run:
	make clean
	make exec; $(EXECUTABLE)

clean:
	rm -f $(OBJ_DIRECTORY)/*.o $(LIB_DIRECTORY)/*.mod
	rm -f $(EXECUTABLE)