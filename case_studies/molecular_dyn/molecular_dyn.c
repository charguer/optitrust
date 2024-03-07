#include <stdio.h>
#include <stdlib.h>

typedef struct {
  double x, y, z;
  double weight;
  double fx, fy, fz;
  double vx, vy, vz;
} Particle;

typedef struct {
  Particle * particles;
  size_t size, capacity;
} Cell;

Cell * cell_create(size_t capacity) {
  Cell * new_cell = (Cell *) malloc(sizeof(Cell));
  if(!new_cell) { return NULL; }
  new_cell->particles = (Particle *) malloc(capacity * sizeof(Particle));
  if(!new_cell->particles) { return NULL; }
  new_cell->capacity = capacity;
  new_cell->size = 0L;
  return new_cell;
}

Cell * cell_add_particle(Cell * cell, Particle particle) {
  if(!cell) { return NULL; }
  if(!cell->particles) { return NULL; }
  if(cell->size == cell->capacity) {
    cell->particles = realloc(cell->particles, 1024 * sizeof(Particle));
    if(!cell->particles) { return NULL; }
    cell->capacity += 1024;
  }
  cell->particles[cell->size] = particle;
  cell->size++;
  return cell;
}

Cell * cell_self_compute(Cell * cell) {
  if(!cell) { return NULL; }
  
  for(size_t idxSrc = 0; idxSrc < cell->size; idxSrc++) {
    for(size_t idxTgt = idxSrc + 1; idxTgt < cell->size; idxTgt++) {
      double dx = cell->particles[idxSrc].x - cell->particles[idxTgt].x;
      double dy = cell->particles[idxSrc].y - cell->particles[idxTgt].y;
      double dz = cell->particles[idxSrc].z - cell->particles[idxTgt].z;

      double inv_square_distance = (double) 1.0 / (dx * dx + dy * dy + dz * dz);
      double inv_distance = sqrt(inv_square_distance);

      inv_square_distance *= inv_distance;
      inv_square_distance *=
        cell->particles[idxTgt].weight * cell->particles[idxSrc].weight;

      dx *= inv_square_distance;
      dy *= inv_square_distance;
      dz *= inv_square_distance;

      cell->particles[idxSrc].x += dx;
      cell->particles[idxSrc].y += dy;
      cell->particles[idxSrc].z += dz;

      cell->particles[idxTgt].x -= dx;
      cell->particles[idxTgt].y -= dy;
      cell->particles[idxTgt].z -= dz;
    }
  }

  return cell;
}

Cell * cell_neighbor_compute(Cell * me, Cell * neighbor) {
  if(!cell || !neighbor) { return NULL; }
  if(me == neighbor) { return me; }
  
  for(size_t idxSrc = 0; idxSrc < me->size; idxSrc++) {
    for(size_t idxTgt = 0; idxTgt < neighbor->size; idxTgt++) {
      double dx = me->particles[idxSrc].x - neighbor->particles[idxTgt].x;
      double dy = me->particles[idxSrc].y - neighbor->particles[idxTgt].y;
      double dz = me->particles[idxSrc].z - neighbor->particles[idxTgt].z;

      double inv_square_distance = (double) 1.0 / (dx * dx + dy * dy + dz * dz);
      double inv_distance = sqrt(inv_square_distance);

      inv_square_distance *= inv_distance;
      inv_square_distance *=
        neighbor->particles[idxTgt].weight * me->particles[idxSrc].weight;

      dx *= inv_square_distance;
      dy *= inv_square_distance;
      dz *= inv_square_distance;

      me->particles[idxSrc].x += dx;
      me->particles[idxSrc].y += dy;
      me->particles[idxSrc].z += dz;
    }
  }

  return me;
}

Cell * cell_update(Cell * cell, double time_step){
  if(!cell) { return NULL; }
  
  for(size_t idxSrc = 0; idxSrc < cell->size; idxSrc++) {
    cell->particles[idxSrc].vx +=
      (cell->particles[idxSrc].fx / cell->particles[idxSrc].weight) * time_step;
    cell->particles[idxSrc].vy +=
      (cell->particles[idxSrc].fy / cell->particles[idxSrc].weight) * time_step;
    cell->particles[idxSrc].vz +=
      (cell->particles[idxSrc].fz / cell->particles[idxSrc].weight) * time_step;
    
    cell->particles[idxSrc].x += cell->particles[idxSrc].vx * time_step;
    cell->particles[idxSrc].y += cell->particles[idxSrc].vy * time_step;
    cell->particles[idxSrc].z += cell->particles[idxSrc].vz * time_step;
  }

  return cell;
}

Cell * cell_remove_ot_of_intervals(Cell * cell, Cell ** particles_to_remove,
                                   double minPosCell[3], double maxPosCell[3]) {
  if(!cell) { return NULL; }
  
  *particles_to_remove = cell_create(cell->size);

  size_t idxPart = 0;
  while(idxPart != cell->size) {
    if(cell->particles[idxPart].x < minPosCell[0]
       || cell->particles[idxPart].y < minPosCell[1]
       || cell->particles[idxPart].z < minPosCell[2]
       || maxPosCell[0] <= cell->particles[idxPart].x
       || maxPosCell[1] <= cell->particles[idxPart].y
       || maxPosCell[2] <= cell->particles[idxPart].z) {
      *particles_to_remove =
        cell_add_particle(*particles_to_remove, cell->particles[idxPart]);
      cell->particles[idxPart] = cell->particles[--cell->size];
    } else {
      idxPart++;
    }
  }
  
  return cell;
}

typedef struct {
  double box_width, cell_width;
  size_t nb_cells_per_dim, size, capacity;
  Cell ** cells;
} Grid;

int grid_cell_idx_from_position(Grid * grid, Particle particle) {
  if(!grid) { return -1; }
  
  int x = (int) (particle.x / grid->cellWidth),
    y = (int) (particle.y / grid->cellWidth),
    z = (int) (particle.z / grid->cellWidth);
  return (x * grid->nbCellsPerDim + y) * grid->nbCellsPerDim + z;
}

Grid * grid_create(double box_width, double cell_width, Cell * cell) {
  if(!cell) { return NULL; }
  
  Grid * new_grid = (Grid *) malloc(sizeof(Grid));
  if(!new_grid) { return NULL; }

  new_grid->box_width = box_width;
  new_grid->cell_width = cell_width;
  new_grid->nb_cells_per_dim = (size_t) (box_width / cell_width);
  new_grid->capacity =
    new_grid->nb_cells_per_dim *
    new_grid->nb_cells_per_dim *
    new_grid->nb_cells_per_dim;
  
  new_grid->cells = (Cell **) malloc(new_grid->capacity * sizeof(Cell *));
  if(!new_grid->cells) { return NULL; }
  
  new_grid->size = 0L;

  for(size_t idxPart = 0; idxPart < cell->size; idxPart++) {
    int cell_idx =
      grid_cell_idx_from_position(new_grid, cell->particles[idxPart]);
    if(cell_idx < 0) { return NULL; }
    new_grid->cells[cell_idx] = cell_add_particle(new_grid->cells[cell_idx],
                                                  cell->particles[idxPart]);
    if(!new_grid->cells[cell_idx]) { return NULL; }
  }
  
  return new_grid;
}

Grid * grid_add_cell(Grid * grid, Cell * cell) {
  if(!grid) { return NULL; }
  if(!grid->cells) { return NULL; }
  if(grid->size == grid->capacity) {
    grid->cells = realloc(grid->cells, 1024 * sizeof(Cell *));
    if(!grid->cells) { return NULL; }
    grid->capacity += 1024;
  }
  grid->cells[grid->size] = cell;
  grid->size++;
  return grid;
}

class Grid{
    const double boxWidth;
    const double cellWidth;
    const int nbCellsPerDim;
    std::vector<Cell> cells;


public:

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
