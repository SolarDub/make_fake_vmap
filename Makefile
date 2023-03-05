BINARY=./bin/mdi_fake_build_f90.exe # Binary executable plus relative path
CODEDIRS=. ./src ./src/f90 # Code directories - can be a list
MODDIR=./mod2
MDIRS=$($(CODEDIRS)/modules) # Module directories - can be a list
EXT=f90
MODPREF=$($(MDIRS)/nrtype.$(EXT))

FC=gfortran
OPT=-O0
# Automatically append -J onto each .mod module directory
MODOPT=$(foreach D,$(MODDIR),-J$(D)/)
# Compilation flags
#CFLAGS=-Wall -Wextra $(MODOPT) #$(OPT)
FFLAGS=$(OPT)
# Library flags
#LFLAGS=-L. -lcfitsio -lm -lcurl

# For-style iteration (foreach) and regular expression completions (wildcard)
# Output each filename with given suffix in the CODEDIRS directory
MFILES=$(foreach D,$(MDIRS),$(wildcard $(D)/*.$(EXT)))
FFILES=$(MODPREF) $(MFILES) $(foreach D,$(CODEDIRS),$(wildcard $(D)/*.$(EXT)))
# Regular expression replacement: output .o from .c files
OBJECTS=$(patsubst %.$(EXT),%.o,$(FFILES))     # .o files track changes in source files

all: $(BINARY)
	echo $(MDIRS)

# Automatic variables: $@ = target - left hand side of colon; $^ = all dependencies - right hand side of colon
$(BINARY): $(OBJECTS)
	$(FC) $(MODOPT) -o $@ $^ #$(LFLAGS)

# Automatic variables: $@ = target - left hand side of colon; $< = first dependency - right hand side of colon
%.o:%.$(EXT)
	$(FC) $(MODOPT) $(FFLAGS) -c -o $@ $<

clean:
	rm -rf $(OBJECTS) # $(DEPFILES)

# Create a tarball of files for easy distribution
distribute: clean
	tar zcvf dist.tgz *

# add .PHONY so that the non-targetfile rules work even if a file with the same name exists.
.PHONY: all clean distribute diff
