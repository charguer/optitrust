
PVIEW=../../tools/pview

all: build run view

build: demo_pview.exe

demo_pview.exe: demo_pview.cpp
	g++  -o $@  -fmax-errors=1 -fopenmp -O3 -DLOGGING -I . atomic.cpp microtime.cpp workerlocal.cpp logging.cpp  pcmdline.cpp $<

force:

run: LOG_BIN force

LOG_BIN: demo_pview.exe
	./demo_pview.exe

pview: $(PVIEW)/pview
	make -C $(PVIEW)

view: pview LOG_BIN
	$(PVIEW)/pview

runview: run view

clean:
	dune clean
	@rm -f LOG LOG_BIN *.exe

