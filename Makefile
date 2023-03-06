BINARY=bin/make_fake_vmap.exe # Binary executable plus relative path
CODEDIRS=src/f90/modules src/f90 src .  # Code directories - can be a list
MODDIR=mod
MDPREF=src/f90/modules/nr
EXT=f90

FC=gfortran
OPT=-O0
# Automatically append -J onto each .mod module directory
MODOPT=$(foreach D,$(MODDIR),-J$(D)/)
# Compilation flags
#CFLAGS=-Wall -Wextra
FFLAGS=$(OPT)
# Library flags

# For-style iteration (foreach) and regular expression completions (wildcard)
# Output each filename with given suffix in the CODEDIRS directory
FFILES=$(foreach D,$(CODEDIRS),$(wildcard $(D)/*.$(EXT)))

# Regular expression replacement: output .o from .c files
OBJECTS=$(patsubst %.$(EXT),%.o,$(FFILES))     # .o files track changes in source files

all: $(BINARY)

# Automatic variables: $@ = target - left hand side of colon; $^ = all dependencies - right hand side of colon
$(BINARY): $(OBJECTS)
	$(FC) -o $@ $(MODOPT) $^

# Automatic variables: $@ = target - left hand side of colon; $< = first dependency - right hand side of colon
%.o:%.$(EXT)
	$(FC) $(MODOPT) $(FFLAGS) -c -o $@ $<

# Both nr and nrutil modules depend on nrtype module
$(MDPREF).o:$(MDPREF)type.o
	$(FC) $(MODOPT) $(FFLAGS) -c -o $(MDPREF).o $(MDPREF).f90
$(MDPREF)util.o:$(MDPREF)type.o
	$(FC) $(MODOPT) $(FFLAGS) -c -o $(MDPREF)util.o $(MDPREF)util.f90

clean:
	rm -rf $(OBJECTS)

# Create a tarball of files for easy distribution
distribute: clean
	tar zcvf dist.tgz *

# add .PHONY so that the non-targetfile rules work even if a file with the same name exists.
.PHONY: all clean distribute diff
