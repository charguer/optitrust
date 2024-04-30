#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "tools.hpp"

typedef struct {
  double x;
  double y;
  double z;
  double weight;
  double fx;
  double fy;
  double fz;
  double vx;
  double vy;
  double vz;
} Particle;

typedef struct {
  Particle* particles;
  size_t size;
  size_t capacity;
} Cell;

Cell* cell_create(size_t capacity) {
  Cell* new_cell = (Cell*)malloc(sizeof(Cell));
  if (!new_cell) {
    return NULL;
  }
  new_cell->particles = (Particle*)malloc(capacity * sizeof(Particle));
  if (!new_cell->particles) {
    return NULL;
  }
  new_cell->capacity = capacity;
  new_cell->size = 0;
  return new_cell;
}

void cell_destroy(Cell* cell) {
  if (cell) {
    if (cell->particles) {
      free(cell->particles);
    }
    free(cell);
  }
}

Cell* cell_add_particle(Cell* cell, Particle particle) {
  if (!cell) {
    return NULL;
  }
  if (!cell->particles) {
    return NULL;
  }
  if (cell->size == cell->capacity) {
    size_t new_capacity = cell->capacity * 2;
    cell->particles = (Particle*)realloc(cell->particles, new_capacity * sizeof(Particle));
    if (!cell->particles) {
      return NULL;
    }
    cell->capacity = new_capacity;
  }
  cell->particles[cell->size] = particle;
  cell->size++;
  return cell;
}

Cell* cell_self_compute(Cell* cell) {
  if (!cell) {
    return NULL;
  }
  for (int idxSrc = 0; idxSrc < cell->size; idxSrc++) {
    for (int idxTgt = idxSrc + 1; idxTgt < cell->size; idxTgt++) {
      double dx = cell->particles[idxSrc].x - cell->particles[idxTgt].x;
      double dy = cell->particles[idxSrc].y - cell->particles[idxTgt].y;
      double dz = cell->particles[idxSrc].z - cell->particles[idxTgt].z;
      double inv_square_distance = (double)1. / (dx * dx + dy * dy + dz * dz);
      double inv_distance = sqrt(inv_square_distance);
      inv_square_distance *= inv_distance;
      inv_square_distance *= cell->particles[idxTgt].weight * cell->particles[idxSrc].weight;
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

Cell* cell_neighbor_compute(Cell* me, Cell* neighbor) {
  if (!me || !neighbor) {
    return NULL;
  }
  if (me == neighbor) {
    return me;
  }
  for (int idxSrc = 0; idxSrc < me->size; idxSrc++) {
    for (int idxTgt = 0; idxTgt < neighbor->size; idxTgt++) {
      double dx = me->particles[idxSrc].x - neighbor->particles[idxTgt].x;
      double dy = me->particles[idxSrc].y - neighbor->particles[idxTgt].y;
      double dz = me->particles[idxSrc].z - neighbor->particles[idxTgt].z;
      double inv_square_distance = (double)1. / (dx * dx + dy * dy + dz * dz);
      double inv_distance = sqrt(inv_square_distance);
      inv_square_distance *= inv_distance;
      inv_square_distance *= neighbor->particles[idxTgt].weight * me->particles[idxSrc].weight;
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

Cell* cell_update(Cell* cell, double time_step) {
  if (!cell) {
    return NULL;
  }
  for (int idxSrc = 0; idxSrc < cell->size; idxSrc++) {
    cell->particles[idxSrc].vx += cell->particles[idxSrc].fx / cell->particles[idxSrc].weight * time_step;
    cell->particles[idxSrc].vy += cell->particles[idxSrc].fy / cell->particles[idxSrc].weight * time_step;
    cell->particles[idxSrc].vz += cell->particles[idxSrc].fz / cell->particles[idxSrc].weight * time_step;
    cell->particles[idxSrc].x += cell->particles[idxSrc].vx * time_step;
    cell->particles[idxSrc].y += cell->particles[idxSrc].vy * time_step;
    cell->particles[idxSrc].z += cell->particles[idxSrc].vz * time_step;
  }
  return cell;
}

Cell* cell_remove_ot_of_intervals(Cell* cell, Cell** removed_particles, double minPosCell[3], double maxPosCell[3]) {
  if (!cell) {
    return NULL;
  }
  size_t idxPart = 0;
  while (idxPart != cell->size) {
    if (cell->particles[idxPart].x < minPosCell[0] || cell->particles[idxPart].y < minPosCell[1] || cell->particles[idxPart].z < minPosCell[2] || maxPosCell[0] <= cell->particles[idxPart].x || maxPosCell[1] <= cell->particles[idxPart].y || maxPosCell[2] <= cell->particles[idxPart].z) {
      *removed_particles = cell_add_particle(*removed_particles, cell->particles[idxPart]);
      cell->particles[idxPart] = cell->particles[--cell->size];
    } else {
      idxPart++;
    }
  }
  return cell;
}

typedef struct {
  double box_width;
  double cell_width;
  size_t nb_cells_per_dim;
  size_t capacity;
  Cell** cells;
} Grid;

int grid_cell_idx_from_position(Grid* grid, Particle particle) {
  if (!grid) {
    return -1;
  }
  int x = (int)(particle.x / grid->cell_width), y = (int)(particle.y / grid->cell_width), z = (int)(particle.z / grid->cell_width);
  return (x * grid->nb_cells_per_dim + y) * grid->nb_cells_per_dim + z;
}

Grid* grid_create(const double box_width, const double cell_width, const Cell* const cell) {
  Grid* __apac_result;
#pragma omp taskgroup
  {
    if (!cell) {
      __apac_result = NULL;
      goto __apac_exit;
    }
    Grid* new_grid = (Grid*)malloc(sizeof(Grid));
    if (!new_grid) {
      __apac_result = NULL;
      goto __apac_exit;
    }
    new_grid->box_width = box_width;
    new_grid->cell_width = cell_width;
    new_grid->nb_cells_per_dim = (size_t)(box_width / cell_width);
    new_grid->capacity = new_grid->nb_cells_per_dim * new_grid->nb_cells_per_dim * new_grid->nb_cells_per_dim;
    new_grid->cells = (Cell**)calloc(new_grid->capacity, sizeof(Cell*));
    if (!new_grid->cells) {
      __apac_result = NULL;
      goto __apac_exit;
    }
    for (int idx = 0; idx < new_grid->capacity; idx++) {
      if (!new_grid->cells[idx]) {
        printf("Unallocated cell %lu\n", idx);
      }
    }
#pragma omp taskwait depend(in : (*cell)[0])
    for (int idxPart = 0; idxPart < cell->size; idxPart++) {
      int* cell_idx = new int(grid_cell_idx_from_position(new_grid, cell->particles[idxPart]));
#pragma omp taskwait depend(in : cell_idx[0])
      if (*cell_idx < 0) {
#pragma omp taskwait
        __apac_result = NULL;
        goto __apac_exit;
      }
#pragma omp taskwait depend(in : cell_idx[0]) depend(inout : (*new_grid.cells)[*cell_idx])
      if (!new_grid->cells[*cell_idx]) {
#pragma omp task default(shared) depend(in : cell_idx) depend(inout : (*new_grid.cells)[cell_idx])
        new_grid->cells[*cell_idx] = cell_create(10);
      }
#pragma omp task default(shared) depend(in : (*cell.particles)[idxPart], cell_idx[0]) depend(inout : (*new_grid.cells)[*cell_idx]) firstprivate(idxPart)
      new_grid->cells[*cell_idx] = cell_add_particle(new_grid->cells[*cell_idx], cell->particles[idxPart]);
#pragma omp taskwait depend(in : (*new_grid.cells)[*cell_idx], cell_idx[0])
      if (!new_grid->cells[*cell_idx]) {
#pragma omp taskwait
        __apac_result = NULL;
        goto __apac_exit;
      }
#pragma omp task default(shared) depend(inout : cell_idx[0])
      delete cell_idx;
    }
#pragma omp taskwait
    __apac_result = new_grid;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

void grid_destroy(Grid* grid) {
  if (grid) {
    if (grid->cells) {
      for (int idx = 0; idx < grid->capacity; idx++) {
        cell_destroy(grid->cells[idx]);
      }
      free(grid->cells);
    }
    free(grid);
  }
}

Grid* grid_compute(Grid* grid) {
  Grid* __apac_result;
#pragma omp taskgroup
  {
    size_t me, neighbor;
    for (int idx_x = 0; idx_x < grid->nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < grid->nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < grid->nb_cells_per_dim; idx_z++) {
          me = (idx_x * grid->nb_cells_per_dim + idx_y) * grid->nb_cells_per_dim + idx_z;
          grid->cells[me] = cell_self_compute(grid->cells[me]);
          for (int idx_x_neigh = -1; idx_x_neigh <= 1; idx_x_neigh++) {
            for (int idx_y_neigh = -1; idx_y_neigh <= 1; idx_y_neigh++) {
              for (int idx_z_neigh = -1; idx_z_neigh <= 1; idx_z_neigh++) {
                neighbor = ((idx_x + idx_x_neigh + grid->nb_cells_per_dim) % grid->nb_cells_per_dim * grid->nb_cells_per_dim + (idx_y + idx_y_neigh + grid->nb_cells_per_dim) % grid->nb_cells_per_dim) * grid->nb_cells_per_dim + (idx_z + idx_z_neigh + grid->nb_cells_per_dim) % grid->nb_cells_per_dim;
                grid->cells[me] = cell_neighbor_compute(grid->cells[me], grid->cells[neighbor]);
              }
            }
          }
        }
      }
    }
    __apac_result = grid;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

Grid* grid_update(Grid* grid, const double time_step) {
  Grid* __apac_result;
#pragma omp taskgroup
  {
    Cell* removed_particles = cell_create(10);
    double minPosCell, maxPosCell;
    size_t cell;
    if (!removed_particles) {
      __apac_result = NULL;
      goto __apac_exit;
    }
    for (int idx_x = 0; idx_x < grid->nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < grid->nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < grid->nb_cells_per_dim; idx_z++) {
          minPosCell[0] = idx_x * grid->cell_width;
          minPosCell[1] = idx_y * grid->cell_width;
          minPosCell[2] = idx_z * grid->cell_width;
          maxPosCell[0] = (idx_x + 1) * grid->cell_width;
          maxPosCell[1] = (idx_y + 1) * grid->cell_width;
          maxPosCell[2] = (idx_z + 1) * grid->cell_width;
          cell = (idx_x * grid->nb_cells_per_dim + idx_y) * grid->nb_cells_per_dim + idx_z;
          grid->cells[cell] = cell_update(grid->cells[cell], time_step);
          grid->cells[cell] = cell_remove_ot_of_intervals(grid->cells[cell], &removed_particles, minPosCell, maxPosCell);
        }
      }
    }
#pragma omp taskwait depend(in : removed_particles[0])
    for (int idx = 0; idx < removed_particles->size; idx++) {
#pragma omp taskwait depend(in : idx) depend(inout : (*removed_particles.particles)[idx])
      while (removed_particles->particles[idx].x < 0) {
#pragma omp taskwait depend(in : idx)
        removed_particles->particles[idx].x += grid->box_width;
      }
#pragma omp taskwait depend(in : (*grid)[0], idx) depend(inout : (*removed_particles.particles)[idx])
      while (grid->box_width <= removed_particles->particles[idx].x) {
#pragma omp taskwait depend(in : idx)
        removed_particles->particles[idx].x -= grid->box_width;
      }
#pragma omp taskwait depend(in : idx) depend(inout : (*removed_particles.particles)[idx])
      while (removed_particles->particles[idx].y < 0) {
#pragma omp taskwait depend(in : idx)
        removed_particles->particles[idx].y += grid->box_width;
      }
#pragma omp taskwait depend(in : (*grid)[0], idx) depend(inout : (*removed_particles.particles)[idx])
      while (grid->box_width <= removed_particles->particles[idx].y) {
#pragma omp taskwait depend(in : idx)
        removed_particles->particles[idx].y -= grid->box_width;
      }
#pragma omp taskwait depend(in : idx) depend(inout : (*removed_particles.particles)[idx])
      while (removed_particles->particles[idx].z < 0) {
#pragma omp taskwait depend(in : idx)
        removed_particles->particles[idx].z += grid->box_width;
      }
#pragma omp taskwait depend(in : (*grid)[0], idx) depend(inout : (*removed_particles.particles)[idx])
      while (grid->box_width <= removed_particles->particles[idx].z) {
#pragma omp taskwait depend(in : idx)
        removed_particles->particles[idx].z -= grid->box_width;
      }
      size_t* cell_idx = new size_t(grid_cell_idx_from_position(grid, removed_particles->particles[idx]));
#pragma omp task default(shared) depend(in : (*removed_particles.particles)[idx], cell_idx[0]) depend(inout : (*grid.cells)[*cell_idx]) firstprivate(idx)
      grid->cells[*cell_idx] = cell_add_particle(grid->cells[*cell_idx], removed_particles->particles[idx]);
#pragma omp task default(shared) depend(inout : cell_idx[0])
      delete cell_idx;
    }
#pragma omp task default(shared) depend(inout : removed_particles[0])
    cell_destroy(removed_particles);
#pragma omp taskwait
    __apac_result = grid;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int main() {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    const double box_width = 1;
    const size_t size = 20000;
    Cell* cell = cell_create(size);
    initialize_random_number_generator(box_width);
    for (int idx = 0; idx < size; idx++) {
      Particle* particle = new Particle();
      particle->x = random_number();
      particle->y = random_number();
      particle->z = random_number();
      particle->weight = 1.;
      particle->fx = random_number();
      particle->fy = random_number();
      particle->fz = random_number();
      particle->vx = 0.;
      particle->vy = 0.;
      particle->vz = 0.;
      cell = cell_add_particle(cell, *particle);
#pragma omp task default(shared) depend(inout : particle[0])
      delete particle;
    }
    const size_t steps = 50;
    const double time_step = 0.001;
    const double cell_width = 0.2;
    Grid* grid = grid_create(box_width, cell_width, cell);
#pragma omp taskwait depend(in : steps)
    for (int idx = 0; idx < steps; idx++) {
#pragma omp task default(shared) depend(in : time_step) depend(inout : grid, grid[0])
      {
        grid = grid_compute(grid);
        grid = grid_update(grid, time_step);
      }
    }
#pragma omp task default(shared) depend(inout : cell[0])
    cell_destroy(cell);
#pragma omp task default(shared) depend(inout : grid[0])
    grid_destroy(grid);
#pragma omp taskwait
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
