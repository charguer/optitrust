#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "tools.hpp"

typedef struct {
  double x, y, z;
  double weight;
  double vx, vy, vz;
} Particle_symb;

typedef struct {
  double fx, fy, fz;
} Particle_forces;

typedef struct {
  Particle_symb * particles_symb;
  Particle_forces * particles_forces;
  size_t size, capacity;
} Cell;

Cell cell_create(size_t capacity) {
  Cell new_cell;
  memset(&new_cell, 0, sizeof(Cell));

  new_cell.particles_symb = (Particle_symb *) malloc(capacity * sizeof(Particle_symb));
  if(!new_cell.particles_symb) { return new_cell; }

  new_cell.particles_forces = (Particle_forces *) malloc(capacity * sizeof(Particle_forces));
  if(!new_cell.particles_forces) { return new_cell; }

  new_cell.capacity = capacity;
  new_cell.size = 0L;

  return new_cell;
}

void cell_destroy(Cell* cell) {
  if(!cell) { return; }

  if(cell->particles_symb) {
    free(cell->particles_symb);
  }
  if(cell->particles_forces) {
    free(cell->particles_forces);
  }
  cell->capacity = 0;
  cell->size = 0;
}

void cell_add_particle(Cell * cell, Particle_symb particle_symb, Particle_forces particle_forces) {
  if(!cell) { return; }

  if(cell->size == cell->capacity) {
    size_t new_capacity = cell->capacity * 2;

    cell->particles_symb = (Particle_symb *)
      realloc(cell->particles_symb, new_capacity * sizeof(Particle_symb));
    if(!cell->particles_symb) { return; }

    cell->particles_forces = (Particle_forces *)
      realloc(cell->particles_forces, new_capacity * sizeof(Particle_forces));
    if(!cell->particles_forces) { return; }

    cell->capacity = new_capacity;
  }
  cell->particles_symb[cell->size] = particle_symb;
  cell->particles_forces[cell->size] = particle_forces;
  cell->size += 1;
}

void cell_self_compute(Particle_symb* particles_symb, Particle_forces* particles_forces, const size_t size) {  
  for(size_t idxTgt = 0; idxTgt < size; idxTgt++) {
    for(size_t idxSrc = 0; idxSrc < idxTgt+1; idxSrc++) {
      const double dx = particles_symb[idxSrc].x - particles_symb[idxTgt].x;
      const double dy = particles_symb[idxSrc].y - particles_symb[idxTgt].y;
      const double dz = particles_symb[idxSrc].z - particles_symb[idxTgt].z;

      const double square_distance = (dx * dx + dy * dy + dz * dz);
      const double distance = sqrt(square_distance);
      const double cube_distance = square_distance * distance;

      const double coef = particles_symb[idxTgt].weight * particles_symb[idxSrc].weight / cube_distance;

      const double fx = dx * coef;
      const double fy = dy * coef;
      const double fz = dz * coef;

      particles_forces[idxTgt].fx += fx;
      particles_forces[idxTgt].fy += fy;
      particles_forces[idxTgt].fz += fz;

      particles_forces[idxSrc].fx -= fx;
      particles_forces[idxSrc].fy -= fy;
      particles_forces[idxSrc].fz -= fz;
    }
  }
}

void cell_neighbor_compute(Particle_symb* particles_symb, Particle_forces* particles_forces, const size_t size,
                             Particle_symb* particles_symbNeigh, const size_t sizeNeighbor) {  
  for(size_t idxTgt = 0; idxTgt < size; idxTgt++) {
    for(size_t idxSrc = 0; idxSrc < sizeNeighbor; idxSrc++) {
      const double dx = particles_symbNeigh[idxSrc].x - particles_symb[idxTgt].x;
      const double dy = particles_symbNeigh[idxSrc].y - particles_symb[idxTgt].y;
      const double dz = particles_symbNeigh[idxSrc].z - particles_symb[idxTgt].z;

      const double square_distance = (dx * dx + dy * dy + dz * dz);
      const double distance = sqrt(square_distance);
      const double cube_distance = square_distance * distance;

      const double coef = particles_symb[idxTgt].weight * particles_symbNeigh[idxSrc].weight / cube_distance;

      const double fx = dx * coef;
      const double fy = dy * coef;
      const double fz = dz * coef;

      particles_forces[idxTgt].fx += fx;
      particles_forces[idxTgt].fy += fy;
      particles_forces[idxTgt].fz += fz;
    }
  }
}

void cell_update(Particle_symb* particles_symb, Particle_forces* particles_forces, const size_t size,
                   double time_step){  
  for(size_t idxPart = 0; idxPart < size; idxPart++) {
    particles_symb[idxPart].vx +=
      (particles_forces[idxPart].fx / particles_symb[idxPart].weight) * time_step;
    particles_symb[idxPart].vy +=
      (particles_forces[idxPart].fy / particles_symb[idxPart].weight) * time_step;
    particles_symb[idxPart].vz +=
      (particles_forces[idxPart].fz / particles_symb[idxPart].weight) * time_step;
    
    particles_symb[idxPart].x += particles_symb[idxPart].vx * time_step;
    particles_symb[idxPart].y += particles_symb[idxPart].vy * time_step;
    particles_symb[idxPart].z += particles_symb[idxPart].vz * time_step;
  }
}

void cell_remove_ot_of_intervals( Particle_symb* particles_symb, Particle_forces* particles_forces, size_t* size,
                                  Cell* removed_particles,
                                  double minPosCell[3], double maxPosCell[3]) {
  size_t idxPart = 0;
  while(idxPart != *size) {
    if(particles_symb[idxPart].x < minPosCell[0]
       || particles_symb[idxPart].y < minPosCell[1]
       || particles_symb[idxPart].z < minPosCell[2]
       || maxPosCell[0] <= particles_symb[idxPart].x
       || maxPosCell[1] <= particles_symb[idxPart].y
       || maxPosCell[2] <= particles_symb[idxPart].z) {
      cell_add_particle(removed_particles,
                        particles_symb[idxPart], particles_forces[idxPart]);
      particles_symb[idxPart] = particles_symb[(*size)-1];
      particles_forces[idxPart] = particles_forces[(*size)-1];
      (*size)--;
    } else {
      idxPart++;
    }
  }
}

typedef struct {
  double box_width, cell_width;
  size_t nb_cells_per_dim, capacity;
  Cell* cells;
} Grid;

int grid_cell_idx_from_position(Grid * grid, Particle_symb particle) {
  if(!grid) { return -1; }
  
  int x = (int) (particle.x / grid->cell_width),
      y = (int) (particle.y / grid->cell_width),
      z = (int) (particle.z / grid->cell_width);
  return (x * grid->nb_cells_per_dim + y) * grid->nb_cells_per_dim + z;
}

Grid grid_create(double box_width, double cell_width, Cell* cell) {  
  Grid new_grid;
  memset(&new_grid, 0, sizeof(Grid));

  new_grid.box_width = box_width;
  new_grid.cell_width = cell_width;
  new_grid.nb_cells_per_dim = (size_t) (box_width / cell_width);
  new_grid.capacity =
    new_grid.nb_cells_per_dim *
    new_grid.nb_cells_per_dim *
    new_grid.nb_cells_per_dim;
  
  new_grid.cells = (Cell*) calloc(new_grid.capacity, sizeof(Cell));
  for(size_t idxCell = 0 ; idxCell < new_grid.capacity ; idxCell++) {
    new_grid.cells[idxCell] = cell_create(10);
  }

  for(size_t idxPart = 0; idxPart < cell->size; idxPart++) {
    int cell_idx =
      grid_cell_idx_from_position(&new_grid, cell->particles_symb[idxPart]);
    cell_add_particle(&new_grid.cells[cell_idx],
                      cell->particles_symb[idxPart], cell->particles_forces[idxPart]);
  }
    
  return new_grid;
}

void grid_destroy(Grid * grid) {
  if(grid) {
    if(grid->cells) {
      for(size_t idx = 0; idx < grid->capacity; idx++) {
        cell_destroy(&grid->cells[idx]);
      }
      free(grid->cells);
    }
  }
}

void grid_compute(Grid * grid) {
  if(!grid) { return; }

  size_t me, neighbor;
  for(size_t idx_x = 0; idx_x < grid->nb_cells_per_dim; idx_x++) {
    for(size_t idx_y = 0; idx_y < grid->nb_cells_per_dim; idx_y++) {
      for(size_t idx_z = 0; idx_z < grid->nb_cells_per_dim; idx_z++) {
        me = (idx_x * grid->nb_cells_per_dim + idx_y)
              * grid->nb_cells_per_dim + idx_z;
        
        cell_self_compute(grid->cells[me].particles_symb, grid->cells[me].particles_forces, grid->cells[me].size);

        for(int idx_x_neigh = -1; idx_x_neigh <= 1; idx_x_neigh++) {
          for(int idx_y_neigh = -1; idx_y_neigh <= 1; idx_y_neigh++) {
            for(int idx_z_neigh = -1; idx_z_neigh <= 1; idx_z_neigh++) {
              neighbor = 
                (((idx_x + idx_x_neigh + grid->nb_cells_per_dim) 
                    % grid->nb_cells_per_dim) * grid->nb_cells_per_dim
                + ((idx_y + idx_y_neigh + grid->nb_cells_per_dim) 
                    % grid->nb_cells_per_dim)) * grid->nb_cells_per_dim
                + ((idx_z + idx_z_neigh + grid->nb_cells_per_dim) 
                    % grid->nb_cells_per_dim);
              
              cell_neighbor_compute(grid->cells[me].particles_symb, grid->cells[me].particles_forces, grid->cells[me].size,
                                    grid->cells[neighbor].particles_symb, grid->cells[neighbor].size);
            }
          }
        }
      }
    }
  }
}

void grid_update(Grid * grid, double time_step) {
  Cell removed_particles = cell_create(10);
  double minPosCell[3], maxPosCell[3];
  size_t cell;

  for(size_t idx_x = 0; idx_x < grid->nb_cells_per_dim; idx_x++) {
    for(size_t idx_y = 0; idx_y < grid->nb_cells_per_dim; idx_y++) {
      for(size_t idx_z = 0; idx_z < grid->nb_cells_per_dim; idx_z++) {
        cell = (idx_x * grid->nb_cells_per_dim + idx_y)
               * grid->nb_cells_per_dim + idx_z;
        minPosCell[0] = idx_x * grid->cell_width;
        minPosCell[1] = idx_y * grid->cell_width;
        minPosCell[2] = idx_z * grid->cell_width;
        maxPosCell[0] = (idx_x + 1) * grid->cell_width;
        maxPosCell[1] = (idx_y + 1) * grid->cell_width;
        maxPosCell[2] = (idx_z + 1) * grid->cell_width;
        cell_update(grid->cells[cell].particles_symb, grid->cells[cell].particles_forces, grid->cells[cell].size, time_step);
        cell_remove_ot_of_intervals(
          grid->cells[cell].particles_symb, grid->cells[cell].particles_forces, &grid->cells[cell].size,
          &removed_particles, minPosCell, maxPosCell
        );
      }
    }
  }

  for(size_t idx = 0; idx < removed_particles.size; idx++) {
    while(removed_particles.particles_symb[idx].x < 0){
      removed_particles.particles_symb[idx].x += grid->box_width;
    }

    while(grid->box_width <= removed_particles.particles_symb[idx].x){
      removed_particles.particles_symb[idx].x -= grid->box_width;
    }

    while(removed_particles.particles_symb[idx].y < 0){
      removed_particles.particles_symb[idx].y += grid->box_width;
    }

    while(grid->box_width <= removed_particles.particles_symb[idx].y){
      removed_particles.particles_symb[idx].y -= grid->box_width;
    }

    while(removed_particles.particles_symb[idx].z < 0){
      removed_particles.particles_symb[idx].z += grid->box_width;
    }

    while(grid->box_width <= removed_particles.particles_symb[idx].z){
      removed_particles.particles_symb[idx].z -= grid->box_width;
    }

    size_t cell_idx = grid_cell_idx_from_position(
      grid, removed_particles.particles_symb[idx]
    );
    cell_add_particle(
      &grid->cells[cell_idx], removed_particles.particles_symb[idx], removed_particles.particles_forces[idx]
    );
  }

  cell_destroy(&removed_particles);
}

void fill_cell_with_rand_particles(Cell* inCell, double box_width, size_t size){
  initialize_random_number_generator(box_width);

  for(size_t idx = 0 ; idx < size ; idx++){
    Particle_symb particle;
    particle.x = random_number();
    particle.y = random_number();
    particle.z = random_number();
    particle.weight = 1.0;
    particle.vx = 0.0;
    particle.vy = 0.0;
    particle.vz = 0.0;
    Particle_forces particle_forces;
    particle_forces.fx = random_number();
    particle_forces.fy = random_number();
    particle_forces.fz = random_number();
    cell_add_particle(inCell, particle, particle_forces);
  }
}

int main() {
  const double box_width = 1;
  const double cell_width = 0.20;
  const size_t steps = 5;
  const double time_step = 0.001;
  const size_t size = 20000;

  Cell cell = cell_create(size);
  fill_cell_with_rand_particles(&cell, box_width, size);
  
  Grid grid = grid_create(box_width, cell_width, &cell);
  cell_destroy(&cell);

  for(size_t idx = 0 ; idx < steps ; idx++){
    grid_compute(&grid);
    grid_update(&grid, time_step);
  }

  grid_destroy(&grid);
  return 0;
}
