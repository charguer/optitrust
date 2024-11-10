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
  int size, capacity;
} Cell;

Cell cell_create(int capacity) {
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
    int new_capacity = cell->capacity * 2;

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

void cell_self_compute(const Particle_symb* particles_symb, Particle_forces* particles_forces, const int size) {  
  for(int idxTgt = 0; idxTgt < size; idxTgt++) {
    for(int idxSrc = 0; idxSrc < idxTgt+1; idxSrc++) {
      const double dx = particles_symb[idxSrc].x - particles_symb[idxTgt].x;
      const double dy = particles_symb[idxSrc].y - particles_symb[idxTgt].y;
      const double dz = particles_symb[idxSrc].z - particles_symb[idxTgt].z;

      const double square_distance = (dx * dx + dy * dy + dz * dz + 0.00001);
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

void cell_neighbor_compute(const Particle_symb* particles_symb, Particle_forces* particles_forces, const int size,
                             const Particle_symb* particles_symbNeigh, const int sizeNeighbor) {  
  for(int idxTgt = 0; idxTgt < size; idxTgt++) {
    for(int idxSrc = 0; idxSrc < sizeNeighbor; idxSrc++) {
      const double dx = particles_symbNeigh[idxSrc].x - particles_symb[idxTgt].x;
      const double dy = particles_symbNeigh[idxSrc].y - particles_symb[idxTgt].y;
      const double dz = particles_symbNeigh[idxSrc].z - particles_symb[idxTgt].z;

      const double square_distance = (dx * dx + dy * dy + dz * dz + 0.00001);
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

void cell_update(Particle_symb* particles_symb, Particle_forces* particles_forces, const int size,
                   double time_step){  
  for(int idxPart = 0; idxPart < size; idxPart++) {
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

typedef struct {
  double box_width, cell_width;
  int nb_cells_per_dim, capacity;
  Cell* cells;
} Grid;

int grid_cell_idx_from_position(const double cell_width, const int nb_cells_per_dim, Particle_symb particle) {  
  int x = (int) (particle.x / cell_width),
      y = (int) (particle.y / cell_width),
      z = (int) (particle.z / cell_width);
  return (x * nb_cells_per_dim + y) * nb_cells_per_dim + z;
}

int grid_create(double box_width, double cell_width, const Particle_symb* src_particles_symb, const Particle_forces* src_particles_forces,
                const int src_size,
                 int** sizes, Particle_symb*** particles_symb, Particle_forces*** particles_forces) {  
  const int nb_cells_per_dim = (int) (box_width / cell_width);
  const int capacity =
    nb_cells_per_dim *
    nb_cells_per_dim *
    nb_cells_per_dim;

  *sizes = (int*)calloc(capacity, sizeof(int));
  *particles_symb = (Particle_symb**)calloc(capacity, sizeof(Particle_symb*));
  *particles_forces = (Particle_forces**)calloc(capacity, sizeof(Particle_forces*));

  for(int idxPart = 0; idxPart < src_size; idxPart++) {
    int cell_idx =
      grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[idxPart]);
    (*sizes)[cell_idx]++;
  }
  for(int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for(int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for(int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        const int cell_idx = (idx_x * nb_cells_per_dim + idx_y)
              * nb_cells_per_dim + idx_z;
        (*particles_symb)[cell_idx] = (Particle_symb*)calloc((*sizes)[cell_idx], sizeof(Particle_symb));
        (*particles_forces)[cell_idx] = (Particle_forces*)calloc((*sizes)[cell_idx], sizeof(Particle_forces));
      }
    }
  }

  int* cpt = (int*)calloc(capacity, sizeof(int));

  for(int idxPart = 0; idxPart < src_size; idxPart++) {
    int cell_idx =
      grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[idxPart]);
    (*particles_symb)[cell_idx][cpt[cell_idx]] = src_particles_symb[idxPart];
    (*particles_forces)[cell_idx][cpt[cell_idx]] = src_particles_forces[idxPart];
    cpt[cell_idx]++;
  }
  free(cpt);
    
  return nb_cells_per_dim;
}

void grid_destroy(const int nb_cells_per_dim, int** sizes, Particle_symb*** particles_symb, Particle_forces*** particles_forces) {
  for(int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for(int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for(int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        const int me = (idx_x * nb_cells_per_dim + idx_y)
              * nb_cells_per_dim + idx_z;
        free((*particles_symb)[me]);
        free((*particles_forces)[me]);
      }
    }
  }
  free(*sizes);
  free(*particles_symb);
  free(*particles_forces);
  *sizes = NULL;
  *particles_symb = NULL;
  *particles_forces = NULL;
}

void grid_compute(const int nb_cells_per_dim, int* sizes, Particle_symb** particles_symb, Particle_forces** particles_forces) {
  if(!sizes || !particles_symb || !particles_forces ) { return; }

  int me, neighbor;
  for(int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for(int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for(int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        me = (idx_x * nb_cells_per_dim + idx_y)
              * nb_cells_per_dim + idx_z;
        const Particle_symb * me_particles_symb = particles_symb[me];
        Particle_forces * me_particles_forces = particles_forces[me];
        
        cell_self_compute(me_particles_symb, me_particles_forces, sizes[me]);

        for(int idx_x_neigh = -1; idx_x_neigh <= 1; idx_x_neigh++) {
          for(int idx_y_neigh = -1; idx_y_neigh <= 1; idx_y_neigh++) {
            for(int idx_z_neigh = -1; idx_z_neigh <= 1; idx_z_neigh++) {
              neighbor = 
                (((idx_x + idx_x_neigh + nb_cells_per_dim) 
                    % nb_cells_per_dim) * nb_cells_per_dim
                + ((idx_y + idx_y_neigh + nb_cells_per_dim) 
                    % nb_cells_per_dim)) * nb_cells_per_dim
                + ((idx_z + idx_z_neigh + nb_cells_per_dim) 
                    % nb_cells_per_dim);
              const Particle_symb * neighbor_particles_symb = particles_symb[neighbor];
              
              cell_neighbor_compute(me_particles_symb, me_particles_forces, sizes[me],
                                    neighbor_particles_symb, sizes[neighbor]);
            }
          }
        }
      }
    }
  }
}

void grid_update(const int nb_particles, const int nb_cells_per_dim, const double box_width, const double cell_width, double time_step,
                 int** sizes, Particle_symb*** particles_symb, Particle_forces*** particles_forces) {
  int* src_sizes = *sizes;
  Particle_symb** src_particles_symb = *particles_symb;
  Particle_forces** src_particles_forces = *particles_forces;

  const int capacity =
    nb_cells_per_dim *
    nb_cells_per_dim *
    nb_cells_per_dim;

  *sizes = (int*)calloc(capacity, sizeof(int));
  *particles_symb = (Particle_symb**)calloc(capacity, sizeof(Particle_symb*));
  *particles_forces = (Particle_forces**)calloc(capacity, sizeof(Particle_forces*));

  for(int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for(int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for(int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        const int cell_idx = (idx_x * nb_cells_per_dim + idx_y)
              * nb_cells_per_dim + idx_z;
        for(int idxPart = 0; idxPart < src_sizes[cell_idx]; idxPart++) {
          // Update position
          src_particles_symb[cell_idx][idxPart].vx +=
            (src_particles_forces[cell_idx][idxPart].fx / src_particles_symb[cell_idx][idxPart].weight) * time_step;
          src_particles_symb[cell_idx][idxPart].vy +=
            (src_particles_forces[cell_idx][idxPart].fy / src_particles_symb[cell_idx][idxPart].weight) * time_step;
          src_particles_symb[cell_idx][idxPart].vz +=
            (src_particles_forces[cell_idx][idxPart].fz / src_particles_symb[cell_idx][idxPart].weight) * time_step;
          src_particles_symb[cell_idx][idxPart].x += src_particles_symb[cell_idx][idxPart].vx * time_step;
          src_particles_symb[cell_idx][idxPart].y += src_particles_symb[cell_idx][idxPart].vy * time_step;
          src_particles_symb[cell_idx][idxPart].z += src_particles_symb[cell_idx][idxPart].vz * time_step;

          while(src_particles_symb[cell_idx][idxPart].x < 0){
            src_particles_symb[cell_idx][idxPart].x += box_width;
          }
          while(src_particles_symb[cell_idx][idxPart].x >= box_width){
            src_particles_symb[cell_idx][idxPart].x -= box_width;
          }
          while(src_particles_symb[cell_idx][idxPart].y < 0){
            src_particles_symb[cell_idx][idxPart].y += box_width;
          }
          while(src_particles_symb[cell_idx][idxPart].y >= box_width){
            src_particles_symb[cell_idx][idxPart].y -= box_width;
          }
          while(src_particles_symb[cell_idx][idxPart].z < 0){
            src_particles_symb[cell_idx][idxPart].z += box_width;
          }
          while(src_particles_symb[cell_idx][idxPart].z >= box_width){
            src_particles_symb[cell_idx][idxPart].z -= box_width;
          }

          // Compute new target cell
          int up_cell_idx =
            grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[cell_idx][idxPart]);
          (*sizes)[up_cell_idx]++;
        }
      }
    }
  }

  for(int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for(int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for(int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        const int cell_idx = (idx_x * nb_cells_per_dim + idx_y)
              * nb_cells_per_dim + idx_z;
        (*particles_symb)[cell_idx] = (Particle_symb*)calloc((*sizes)[cell_idx], sizeof(Particle_symb));
        (*particles_forces)[cell_idx] = (Particle_forces*)calloc((*sizes)[cell_idx], sizeof(Particle_forces));
      }
    }
  }

  int* cpt = (int*)calloc(capacity, sizeof(int));

  for(int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for(int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for(int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        const int cell_idx = (idx_x * nb_cells_per_dim + idx_y)
              * nb_cells_per_dim + idx_z;
        for(int idxPart = 0; idxPart < src_sizes[cell_idx]; idxPart++) {
          int up_cell_idx =
            grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[cell_idx][idxPart]);
          (*particles_symb)[up_cell_idx][cpt[up_cell_idx]] = src_particles_symb[cell_idx][idxPart];
          (*particles_forces)[up_cell_idx][cpt[up_cell_idx]] = src_particles_forces[cell_idx][idxPart];
          cpt[up_cell_idx]++;
        }
      }
    }
  }
  free(cpt);

  grid_destroy(nb_cells_per_dim, &src_sizes, &src_particles_symb, &src_particles_forces);
}

void fill_cell_with_rand_particles(Cell* inCell, double box_width, int size){
  initialize_random_number_generator(box_width);

  for(int idx = 0 ; idx < size ; idx++){
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
  const int steps = 5;
  const double time_step = 0.001;
  const int size = 20000;

  Cell cell = cell_create(size);
  fill_cell_with_rand_particles(&cell, box_width, size);
  
  int* sizes = NULL;
  Particle_symb** particles_symb = NULL;
  Particle_forces** particles_forces = NULL;
  const int nb_cells_per_dim = grid_create(box_width, cell_width,
                                            cell.particles_symb, cell.particles_forces, cell.size,
                                           &sizes, &particles_symb, &particles_forces);
  cell_destroy(&cell);

  for(int idx = 0 ; idx < steps ; idx++){
    grid_compute(nb_cells_per_dim, sizes, particles_symb, particles_forces);
    grid_update(size, nb_cells_per_dim, box_width, cell_width, time_step, &sizes, &particles_symb, &particles_forces);
  }

  grid_destroy(nb_cells_per_dim, &sizes, &particles_symb, &particles_forces);
  return 0;
}
