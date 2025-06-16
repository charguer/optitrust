
PVIEW=../../tools/pview
PVIEW_EXE=../../_build/default/tools/pview/pview.exe

all: build run view

build: demo_pview.exe

demo_pview.exe: demo_pview.cpp atomic.cpp microtime.cpp workerlocal.cpp logging.cpp  pcmdline.cpp atomic.hpp microtime.hpp workerlocal.hpp logging.hpp  pcmdline.hpp
	g++  -o $@  -fmax-errors=1 -fopenmp -O3 -DLOGGING -I . atomic.cpp microtime.cpp workerlocal.cpp logging.cpp  pcmdline.cpp demo_pview.cpp

force:

run: LOG_BIN force

LOG_BIN: demo_pview.exe
	./demo_pview.exe

pview: $(PVIEW_EXE)

$(PVIEW_EXE):
	make -C $(PVIEW)

view: pview LOG_BIN
	$(PVIEW_EXE)

runview: run view

clean:
	dune clean
	@rm -f LOG LOG_BIN *.exe

