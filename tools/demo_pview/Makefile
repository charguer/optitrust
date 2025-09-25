
PVIEW=../../tools/pview
PVIEW_EXE=../../_build/default/tools/pview/pview.exe

all: build run

force:
	rm -f LOG_BIN

# view

build: demo_pview.exe

demo_pview.exe: demo_pview.cpp atomic.cpp microtime.cpp workerlocal.cpp logging.cpp  pcmdline.cpp atomic.hpp microtime.hpp callback.hpp callback.cpp  workerlocal.hpp logging.hpp  pcmdline.hpp
	g++  -o $@  -fmax-errors=1 -fopenmp -O2 -DLOGGING -I . atomic.cpp microtime.cpp workerlocal.cpp logging.cpp callback.cpp pcmdline.cpp demo_pview.cpp

run: force LOG_BIN

LOG_BIN: demo_pview.exe
	./demo_pview.exe -proc 10

pview: $(PVIEW_EXE)

$(PVIEW_EXE):
	make -C $(PVIEW)

view: pview LOG_BIN
	$(PVIEW_EXE)

runview: run view

clean:
	dune clean
	@rm -f LOG LOG_BIN *.exe

