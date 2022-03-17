#ifndef PIC_DEMO_AUX_H
#define PIC_DEMO_AUX_H

void loadParameters(int argc, char** argv);
void allocateStructures();
void deallocateStructures();
void createParticles();
void computeFieldFromRho();
void reportPerformance(double timeStart);
// void reportParticlesState();

#endif