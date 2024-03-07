#include <random>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <array>


struct Particle{
    double x,y,z;
    double weight;
    double fx,fy,fz;
    double vx, vy, vz;
};


class Cell{
    std::vector<Particle> particles;

public:
    Cell(){}

    void addParticle(const Particle& inPart){
        particles.push_back(inPart);
    }

    void selfCompute(){
        for(int idxSrc = 0 ; idxSrc < int(particles.size()) ; ++idxSrc){
            for(int idxTgt = idxSrc+1 ; idxTgt < int(particles.size()) ; ++idxTgt){
                double dx = particles[idxSrc].x - particles[idxTgt].x;
                double dy = particles[idxSrc].y - particles[idxTgt].y;
                double dz = particles[idxSrc].z - particles[idxTgt].z;

                double inv_square_distance = double(1.0) / (dx*dx + dy*dy + dz*dz);
                double inv_distance = std::sqrt(inv_square_distance);

                inv_square_distance *= inv_distance;
                inv_square_distance *= particles[idxTgt].weight * particles[idxSrc].weight;

                dx *= inv_square_distance;
                dy *= inv_square_distance;
                dz *= inv_square_distance;

                particles[idxSrc].x += dx;
                particles[idxSrc].y += dy;
                particles[idxSrc].z += dz;

                particles[idxTgt].x -= dx;
                particles[idxTgt].y -= dy;
                particles[idxTgt].z -= dz;
            }
        }
    }

    void neighCompute(const Cell& inCell){
        if(this == &inCell){
            return;
        }
        
        for(int idxSrc = 0 ; idxSrc < int(particles.size()) ; ++idxSrc){
            for(int idxTgt = 0 ; idxTgt < int(inCell.particles.size()) ; ++idxTgt){
                double dx = particles[idxSrc].x - inCell.particles[idxTgt].x;
                double dy = particles[idxSrc].y - inCell.particles[idxTgt].y;
                double dz = particles[idxSrc].z - inCell.particles[idxTgt].z;

                double inv_square_distance = double(1.0) / (dx*dx + dy*dy + dz*dz);
                double inv_distance = std::sqrt(inv_square_distance);

                inv_square_distance *= inv_distance;
                inv_square_distance *= inCell.particles[idxTgt].weight * particles[idxSrc].weight;

                dx *= inv_square_distance;
                dy *= inv_square_distance;
                dz *= inv_square_distance;

                particles[idxSrc].x += dx;
                particles[idxSrc].y += dy;
                particles[idxSrc].z += dz;

                // V2
                // inCell.particles[idxTgt].x -= dx;
                // inCell.particles[idxTgt].y -= dy;
                // inCell.particles[idxTgt].z -= dz;
                // ENDV
            }
        }
    }

    void update(const double timeStep){
        for(int idxSrc = 0 ; idxSrc < int(particles.size()) ; ++idxSrc){
            particles[idxSrc].vx += (particles[idxSrc].fx / particles[idxSrc].weight) * timeStep;
            particles[idxSrc].vy += (particles[idxSrc].fy / particles[idxSrc].weight) * timeStep;
            particles[idxSrc].vz += (particles[idxSrc].fz / particles[idxSrc].weight) * timeStep;

            particles[idxSrc].x += particles[idxSrc].vx * timeStep;
            particles[idxSrc].y += particles[idxSrc].vy * timeStep;
            particles[idxSrc].z += particles[idxSrc].vz * timeStep;
        }
    }

    std::vector<Particle> removeOutOfIntervals(const std::array<double,3>& minPosCell,
                                               const std::array<double,3>& maxPosCell){
        std::vector<Particle> particlesToRemove;

        size_t idxPart = 0;
        while(idxPart != particles.size()){
            if(particles[idxPart].x < minPosCell[0]
                    || particles[idxPart].y < minPosCell[1]
                    || particles[idxPart].z < minPosCell[2]
                    || maxPosCell[0] <= particles[idxPart].x
                    || maxPosCell[1] <= particles[idxPart].y
                    || maxPosCell[2] <= particles[idxPart].z){
                particlesToRemove.push_back(particles[idxPart]);
                particles[idxPart] = particles.back();
                particles.pop_back();
            }
            else{
                idxPart += 1;
            }
        }

        return particlesToRemove;
    }
};


class Grid{
    const double boxWidth;
    const double cellWidth;
    const int nbCellsPerDim;
    std::vector<Cell> cells;

    std::array<int,3> cellCoordFromPosition(const Particle& inPart) const {
        return std::array<int,3>{{int(inPart.x/cellWidth),
                                  int(inPart.y/cellWidth),
                                  int(inPart.z/cellWidth)}};
    }

    int cellIdxFromPosition(const Particle& inPart) const {
        const std::array<int,3> coord = cellCoordFromPosition(inPart);
        return (coord[0]*nbCellsPerDim + coord[1])*nbCellsPerDim + coord[2];
    }

public:
    Grid(const double inBoxWidth, const double inCellWidth,
         const std::vector<Particle>& inParticles)
        : boxWidth(inBoxWidth), cellWidth(inCellWidth),
          nbCellsPerDim(inBoxWidth/inCellWidth) {

        cells.resize(nbCellsPerDim*nbCellsPerDim*nbCellsPerDim);

        for(int idxPart = 0 ; idxPart < int(inParticles.size()) ; ++idxPart){
            const int cellIdx = cellIdxFromPosition(inParticles[idxPart]);
            cells[cellIdx].addParticle(inParticles[idxPart]);
        }
    }

    void compute(){
        for(int idxX = 0 ; idxX < nbCellsPerDim ; ++idxX){
            for(int idxY = 0 ; idxY < nbCellsPerDim ; ++idxY){
                for(int idxZ = 0 ; idxZ < nbCellsPerDim ; ++idxZ){

                    const int cellIdx = (idxX*nbCellsPerDim + idxY)*nbCellsPerDim + idxZ;
                    // V1
                    // cell mut be private in the task !!
                    Cell& cell = cells[cellIdx];
                    cell.selfCompute();
                    // V2
                    // cellIdx must be private in the task !!
                    // TODO cells[cellIdx].selfCompute();
                    // ENDV

                    for(int idxNeighX = -1 ; idxNeighX <= 1 ; ++idxNeighX){
                        for(int idxNeighY = -1 ; idxNeighY <= 1 ; ++idxNeighY){
                            for(int idxNeighZ = -1 ; idxNeighZ <= 1 ; ++idxNeighZ){
                                const long int otherCellIdx = (((idxX+idxNeighX+nbCellsPerDim)%nbCellsPerDim)*nbCellsPerDim
                                                              + ((idxY+idxNeighY+nbCellsPerDim)%nbCellsPerDim))*nbCellsPerDim
                                                              + ((idxZ+idxNeighZ+nbCellsPerDim)%nbCellsPerDim);
                                // V1
                                // otherCell mut be private in the task !!
                                Cell& otherCell = cells[otherCellIdx];
                                cell.neighCompute(otherCell);
                                // V2
                                //if(cellIdx < otherCellIdx){
                                //    // otherCellIdx must be private in the task !!
                                //    cell.neighCompute(cells[otherCellIdx]);
                                //}
                                // ENDV
                            }
                        }
                    }

                }
            }
        }
    }

    void update(const double timeStep){
        std::vector<Particle> particles;

        for(int idxX = 0 ; idxX < nbCellsPerDim ; ++idxX){
            for(int idxY = 0 ; idxY < nbCellsPerDim ; ++idxY){
                for(int idxZ = 0 ; idxZ < nbCellsPerDim ; ++idxZ){
                    const int cellIdx = (idxX*nbCellsPerDim + idxY)*nbCellsPerDim + idxZ;

                    const std::array<double,3> minPosCell {{idxX*cellWidth,
                                                            idxY*cellWidth,
                                                            idxZ*cellWidth}};
                    const std::array<double,3> maxPosCell {{(idxX+1)*cellWidth,
                                                            (idxY+1)*cellWidth,
                                                            (idxZ+1)*cellWidth}};

                    // V1
                    // cell mut be private in the task !!
                    Cell& cell = cells[cellIdx];
                    cell.update(timeStep);
                    std::vector<Particle> movedParticles = cell.removeOutOfIntervals(minPosCell, maxPosCell);
                    particles.insert(particles.end(), movedParticles.begin(), movedParticles.end());
                    // V2
                    // cellIdx must be private in the task !!
                    // cells[cellIdx].update(timeStep);
                    // std::vector<Particle> movedParticles = cells[cellIdx].removeOutOfIntervals(minPosCell, maxPosCell);
                    // particles.insert(particles.end(), movedParticles.begin(), movedParticles.end());
                    // ENDV
                }
            }
        }

        for(int idxPart = 0 ; idxPart < int(particles.size()) ; ++idxPart){
            while( particles[idxPart].x < 0){
                particles[idxPart].x += boxWidth;
            }
            while( boxWidth <= particles[idxPart].x ){
                particles[idxPart].x -= boxWidth;
            }

            while( particles[idxPart].y < 0){
                particles[idxPart].y += boxWidth;
            }
            while( boxWidth <= particles[idxPart].y ){
                particles[idxPart].y -= boxWidth;
            }

            while( particles[idxPart].z < 0){
                particles[idxPart].z += boxWidth;
            }
            while( boxWidth <= particles[idxPart].z ){
                particles[idxPart].z -= boxWidth;
            }

            const int cellIdx = cellIdxFromPosition(particles[idxPart]);
            cells[cellIdx].addParticle(particles[idxPart]);
        }
    }
};


int main(){
    const double BoxWidth = 1;
    const int Size = 20000;
    std::vector<Particle> particles(Size);

    std::mt19937 gen(0);
    std::uniform_real_distribution<> dis(0, BoxWidth);

    for(int idxPart = 0 ; idxPart < Size ; ++idxPart){
        particles[idxPart].x = dis(gen);
        particles[idxPart].y = dis(gen);
        particles[idxPart].z = dis(gen);
        particles[idxPart].weight = 1;
        particles[idxPart].fx = dis(gen);
        particles[idxPart].fy = dis(gen);
        particles[idxPart].fz = dis(gen);
        particles[idxPart].vx = 0;
        particles[idxPart].vy = 0;
        particles[idxPart].vz = 0;
    }

    const int NbSteps = 50;
    const double TimeStep = 0.001;
    const double CellWidth = 0.20;

    Grid grid(BoxWidth, CellWidth, std::move(particles));

    for(int idxTime = 0 ; idxTime < NbSteps ; ++idxTime){
        grid.compute();
        grid.update(TimeStep);
    }

    return 0;
}
