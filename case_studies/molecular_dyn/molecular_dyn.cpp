#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "tools.hpp"

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

void cell_destroy(Cell * cell) {
  if(cell) {
    if(cell->particles) {
      free(cell->particles);
    }
    free(cell);
  }
}

Cell * cell_add_particle(Cell * cell, Particle particle) {
  if(!cell) { return NULL; }
  if(!cell->particles) { return NULL; }
  if(cell->size == cell->capacity) {
    size_t new_capacity = cell->capacity * 2;
    cell->particles = (Particle *)
      realloc(cell->particles, new_capacity * sizeof(Particle));
    if(!cell->particles) { return NULL; }
    cell->capacity = new_capacity;
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
  if(!me || !neighbor) { return NULL; }
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

Cell * cell_remove_ot_of_intervals(Cell * cell, Cell ** removed_particles,
                                   double minPosCell[3], double maxPosCell[3]) {
  if(!cell) { return NULL; }

  size_t idxPart = 0;
  while(idxPart != cell->size) {
    if(cell->particles[idxPart].x < minPosCell[0]
       || cell->particles[idxPart].y < minPosCell[1]
       || cell->particles[idxPart].z < minPosCell[2]
       || maxPosCell[0] <= cell->particles[idxPart].x
       || maxPosCell[1] <= cell->particles[idxPart].y
       || maxPosCell[2] <= cell->particles[idxPart].z) {
      (*removed_particles) =
        cell_add_particle((*removed_particles), cell->particles[idxPart]);
      cell->particles[idxPart] = cell->particles[--cell->size];
    } else {
      idxPart++;
    }
  }
  
  return cell;
}

typedef struct {
  double box_width, cell_width;
  size_t nb_cells_per_dim, capacity;
  Cell ** cells;
} Grid;

int grid_cell_idx_from_position(Grid * grid, Particle particle) {
  if(!grid) { return -1; }
  
  int x = (int) (particle.x / grid->cell_width),
      y = (int) (particle.y / grid->cell_width),
      z = (int) (particle.z / grid->cell_width);
  return (x * grid->nb_cells_per_dim + y) * grid->nb_cells_per_dim + z;
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
  
  new_grid->cells = (Cell **) calloc(new_grid->capacity, sizeof(Cell *));
  if(!new_grid->cells) { return NULL; }

  for(size_t idxPart = 0; idxPart < cell->size; idxPart++) {
    int cell_idx =
      grid_cell_idx_from_position(new_grid, cell->particles[idxPart]);
    if(cell_idx < 0) { return NULL; }
    if(!new_grid->cells[cell_idx]) {
      new_grid->cells[cell_idx] = cell_create(10);
    }

    new_grid->cells[cell_idx] = cell_add_particle(new_grid->cells[cell_idx],
                                                  cell->particles[idxPart]);
    if(!new_grid->cells[cell_idx]) { return NULL; }
  }

  for(size_t idx = 0; idx < new_grid->capacity; idx++) {
    if(!new_grid->cells[idx]) {
      printf("Unallocated cell %lu\n", idx);
    }
  }

  
  return new_grid;
}

void grid_destroy(Grid * grid) {
  if(grid) {
    if(grid->cells) {
      for(size_t idx = 0; idx < grid->capacity; idx++) {
        cell_destroy(grid->cells[idx]);
      }
      free(grid->cells);
    }
    free(grid);
  }
}

Grid * grid_compute(Grid * grid) {
  size_t me, neighbor;
  for(size_t idx_x = 0; idx_x < grid->nb_cells_per_dim; idx_x++) {
    for(size_t idx_y = 0; idx_y < grid->nb_cells_per_dim; idx_y++) {
      for(size_t idx_z = 0; idx_z < grid->nb_cells_per_dim; idx_z++) {
        me = (idx_x * grid->nb_cells_per_dim + idx_y)
              * grid->nb_cells_per_dim + idx_z;
        
        grid->cells[me] = cell_self_compute(grid->cells[me]);

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
              
              grid->cells[me] = 
                cell_neighbor_compute(grid->cells[me], grid->cells[neighbor]);
            }
          }
        }
      }
    }
  }

  return grid;
}

Grid * grid_update(Grid * grid, double time_step) {
  Cell * removed_particles = cell_create(10);
  if(!removed_particles) { return NULL; }
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
        grid->cells[cell] = cell_update(grid->cells[cell], time_step);
        grid->cells[cell] = cell_remove_ot_of_intervals(
          grid->cells[cell], &removed_particles, minPosCell, maxPosCell
        );
      }
    }
  }

  for(size_t idx = 0; idx < removed_particles->size; idx++) {
    while(removed_particles->particles[idx].x < 0){
      removed_particles->particles[idx].x += grid->box_width;
    }

    while(grid->box_width <= removed_particles->particles[idx].x){
      removed_particles->particles[idx].x -= grid->box_width;
    }

    while(removed_particles->particles[idx].y < 0){
      removed_particles->particles[idx].y += grid->box_width;
    }

    while(grid->box_width <= removed_particles->particles[idx].y){
      removed_particles->particles[idx].y -= grid->box_width;
    }

    while(removed_particles->particles[idx].z < 0){
      removed_particles->particles[idx].z += grid->box_width;
    }

    while(grid->box_width <= removed_particles->particles[idx].z){
      removed_particles->particles[idx].z -= grid->box_width;
    }

    size_t cell_idx = grid_cell_idx_from_position(
      grid, removed_particles->particles[idx]
    );
    grid->cells[cell_idx] = cell_add_particle(
      grid->cells[cell_idx], removed_particles->particles[idx]
    );
  }

  cell_destroy(removed_particles);
  return grid;
}

int main() {
  const double box_width = 1;
  const size_t size = 20000;
  Cell * cell = cell_create(size);

  initialize_random_number_generator(box_width);

  for(size_t idx = 0 ; idx < size ; idx++){
    Particle particle;
    particle.x = random_number();
    particle.y = random_number();
    particle.z = random_number();
    particle.weight = 1.0;
    particle.fx = random_number();
    particle.fy = random_number();
    particle.fz = random_number();
    particle.vx = 0.0;
    particle.vy = 0.0;
    particle.vz = 0.0;
    cell = cell_add_particle(cell, particle);
  }

  const size_t steps = 50;
  const double time_step = 0.001;
  const double cell_width = 0.20;

  Grid * grid = grid_create(box_width, cell_width, cell);

  for(size_t idx = 0 ; idx < steps ; idx++){
    grid = grid_compute(grid);
    grid = grid_update(grid, time_step);
  }

  cell_destroy(cell);
  grid_destroy(grid);
  return 0;
}
