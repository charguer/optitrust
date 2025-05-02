#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "apac_taskify_molecular_dyn.hpp"

typedef struct {
  double x;
  double y;
  double z;
  double weight;
  double vx;
  double vy;
  double vz;
} Particle_symb;

typedef struct {
  double fx;
  double fy;
  double fz;
} Particle_forces;

typedef struct {
  Particle_symb* particles_symb;
  Particle_forces* particles_forces;
  int size;
  int capacity;
} Cell;

Cell cell_create(int capacity) {
  Cell new_cell;
  memset(&new_cell, 0, sizeof(Cell));
  new_cell.particles_symb = (Particle_symb*)malloc(capacity * sizeof(Particle_symb));
  if (!new_cell.particles_symb) {
    return new_cell;
  }
  new_cell.particles_forces = (Particle_forces*)malloc(capacity * sizeof(Particle_forces));
  if (!new_cell.particles_forces) {
    return new_cell;
  }
  new_cell.capacity = capacity;
  new_cell.size = 0;
  return new_cell;
}

void cell_destroy(Cell* cell) {
  if (!cell) {
    return;
  }
  if (cell->particles_symb) {
    free(cell->particles_symb);
  }
  if (cell->particles_forces) {
    free(cell->particles_forces);
  }
  cell->capacity = 0;
  cell->size = 0;
}

void cell_add_particle(Cell* cell, Particle_symb particle_symb, Particle_forces particle_forces) {
  if (!cell) {
    return;
  }
  if (cell->size == cell->capacity) {
    int new_capacity = cell->capacity * 2;
    cell->particles_symb = (Particle_symb*)realloc(cell->particles_symb, new_capacity * sizeof(Particle_symb));
    if (!cell->particles_symb) {
      return;
    }
    cell->particles_forces = (Particle_forces*)realloc(cell->particles_forces, new_capacity * sizeof(Particle_forces));
    if (!cell->particles_forces) {
      return;
    }
    cell->capacity = new_capacity;
  }
  cell->particles_symb[cell->size] = particle_symb;
  cell->particles_forces[cell->size] = particle_forces;
  cell->size += 1;
}

void cell_self_compute(const Particle_symb* particles_symb, Particle_forces* particles_forces, const int size) {
  for (int idxTgt = 0; idxTgt < size; idxTgt++) {
    for (int idxSrc = 0; idxSrc < idxTgt + 1; idxSrc++) {
      const double dx = particles_symb[idxSrc].x - particles_symb[idxTgt].x;
      const double dy = particles_symb[idxSrc].y - particles_symb[idxTgt].y;
      const double dz = particles_symb[idxSrc].z - particles_symb[idxTgt].z;
      const double square_distance = dx * dx + dy * dy + dz * dz + 1e-05;
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

void cell_neighbor_compute(const Particle_symb* particles_symb, Particle_forces* particles_forces, const int size, const Particle_symb* particles_symbNeigh, const int sizeNeighbor) {
  for (int idxTgt = 0; idxTgt < size; idxTgt++) {
    for (int idxSrc = 0; idxSrc < sizeNeighbor; idxSrc++) {
      const double dx = particles_symbNeigh[idxSrc].x - particles_symb[idxTgt].x;
      const double dy = particles_symbNeigh[idxSrc].y - particles_symb[idxTgt].y;
      const double dz = particles_symbNeigh[idxSrc].z - particles_symb[idxTgt].z;
      const double square_distance = dx * dx + dy * dy + dz * dz + 1e-05;
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

void cell_update(Particle_symb* particles_symb, const Particle_forces* particles_forces, const int size, double time_step) {
  for (int idxPart = 0; idxPart < size; idxPart++) {
    particles_symb[idxPart].vx += particles_forces[idxPart].fx / particles_symb[idxPart].weight * time_step;
    particles_symb[idxPart].vy += particles_forces[idxPart].fy / particles_symb[idxPart].weight * time_step;
    particles_symb[idxPart].vz += particles_forces[idxPart].fz / particles_symb[idxPart].weight * time_step;
    particles_symb[idxPart].x += particles_symb[idxPart].vx * time_step;
    particles_symb[idxPart].y += particles_symb[idxPart].vy * time_step;
    particles_symb[idxPart].z += particles_symb[idxPart].vz * time_step;
  }
}

typedef struct {
  double box_width;
  double cell_width;
  int nb_cells_per_dim;
  int capacity;
  Cell* cells;
} Grid;

int grid_cell_idx_from_position(const double cell_width, const int nb_cells_per_dim, Particle_symb particle) {
  int x = (int)(particle.x / cell_width);
  int y = (int)(particle.y / cell_width);
  int z = (int)(particle.z / cell_width);
  return (x * nb_cells_per_dim + y) * nb_cells_per_dim + z;
}

int grid_create(double box_width, double cell_width, const Particle_symb* src_particles_symb, const Particle_forces* src_particles_forces, const int src_size, int** sizes, Particle_symb*** particles_symb, Particle_forces*** particles_forces) {
  int __apac_result;
#pragma omp taskgroup
  {
    const int nb_cells_per_dim = (int)(box_width / cell_width);
    const int capacity = nb_cells_per_dim * nb_cells_per_dim * nb_cells_per_dim;
    *sizes = (int*)calloc(capacity, sizeof(int));
    *particles_symb = (Particle_symb**)calloc(capacity, sizeof(Particle_symb*));
    *particles_forces = (Particle_forces**)calloc(capacity, sizeof(Particle_forces*));
    for (int idxPart = 0; idxPart < src_size; idxPart++) {
      int* cell_idx = new int();
#pragma omp task default(shared) depend(in : cell_width, nb_cells_per_dim, src_particles_symb, src_particles_symb[idxPart]) depend(inout : cell_idx[0]) firstprivate(idxPart) firstprivate(cell_idx)
      *cell_idx = grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[idxPart]);
#pragma omp taskwait depend(in : cell_idx[0])
#pragma omp taskwait depend(in : *sizes, cell_idx[0], sizes) depend(inout : (*sizes)[*cell_idx])
      (*sizes)[*cell_idx]++;
#pragma omp task default(shared) depend(inout : cell_idx[0]) firstprivate(cell_idx)
      delete cell_idx;
    }
    for (int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
          const int cell_idx = (idx_x * nb_cells_per_dim + idx_y) * nb_cells_per_dim + idx_z;
#pragma omp taskwait depend(in : (*sizes)[cell_idx], *particles_symb, *sizes, cell_idx, particles_symb, sizes) depend(inout : (*particles_symb)[cell_idx])
          (*particles_symb)[cell_idx] = (Particle_symb*)calloc((*sizes)[cell_idx], sizeof(Particle_symb));
#pragma omp taskwait depend(in : (*sizes)[cell_idx], *particles_forces, *sizes, cell_idx, particles_forces, sizes) depend(inout : (*particles_forces)[cell_idx])
          (*particles_forces)[cell_idx] = (Particle_forces*)calloc((*sizes)[cell_idx], sizeof(Particle_forces));
        }
      }
    }
    int* cpt;
    cpt = (int*)calloc(capacity, sizeof(int));
    for (int idxPart = 0; idxPart < src_size; idxPart++) {
      int* cell_idx = new int();
#pragma omp task default(shared) depend(in : cell_width, nb_cells_per_dim, src_particles_symb, src_particles_symb[idxPart]) depend(inout : cell_idx[0]) firstprivate(idxPart) firstprivate(cell_idx)
      *cell_idx = grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[idxPart]);
#pragma omp taskwait depend(in : cell_idx[0])
#pragma omp taskwait depend(in : (*particles_symb)[*cell_idx], *particles_symb, cell_idx[0], cpt, cpt[*cell_idx], idxPart, particles_symb, src_particles_symb, src_particles_symb[idxPart]) depend(inout : (*particles_symb)[*cell_idx][cpt[*cell_idx]])
      (*particles_symb)[*cell_idx][cpt[*cell_idx]] = src_particles_symb[idxPart];
#pragma omp taskwait depend(in : (*particles_forces)[*cell_idx], *particles_forces, cell_idx[0], cpt, cpt[*cell_idx], idxPart, particles_forces, src_particles_forces, src_particles_forces[idxPart]) depend(inout : (*particles_forces)[*cell_idx][cpt[*cell_idx]])
      (*particles_forces)[*cell_idx][cpt[*cell_idx]] = src_particles_forces[idxPart];
#pragma omp taskwait depend(in : cell_idx[0], cpt) depend(inout : cpt[*cell_idx])
      cpt[*cell_idx]++;
#pragma omp task default(shared) depend(inout : cell_idx[0]) firstprivate(cell_idx)
      delete cell_idx;
    }
    free(cpt);
    __apac_result = nb_cells_per_dim;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

void grid_destroy(const int nb_cells_per_dim, int** sizes, Particle_symb*** particles_symb, Particle_forces*** particles_forces) {
  for (int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
    for (int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
      for (int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
        const int me = (idx_x * nb_cells_per_dim + idx_y) * nb_cells_per_dim + idx_z;
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

void grid_compute(const int nb_cells_per_dim, const int* sizes, const Particle_symb* const* particles_symb, Particle_forces** particles_forces) {
#pragma omp taskgroup
  {
    if (!sizes || !particles_symb || !particles_forces) {
      goto __apac_exit;
    }
    int me;
    int neighbor;
    for (int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
#pragma omp taskwait depend(in : idx_x, idx_y, idx_z, nb_cells_per_dim) depend(inout : me)
          me = (idx_x * nb_cells_per_dim + idx_y) * nb_cells_per_dim + idx_z;
          const Particle_symb* me_particles_symb = particles_symb[me];
#pragma omp taskwait depend(in : me, particles_forces, particles_forces[me])
          Particle_forces* me_particles_forces = particles_forces[me];
#pragma omp task default(shared) depend(in : me, particles_symb, sizes, sizes[me]) depend(inout : particles_forces)
          cell_self_compute(me_particles_symb, me_particles_forces, sizes[me]);
          for (int idx_x_neigh = -1; idx_x_neigh <= 1; idx_x_neigh++) {
            for (int idx_y_neigh = -1; idx_y_neigh <= 1; idx_y_neigh++) {
              for (int idx_z_neigh = -1; idx_z_neigh <= 1; idx_z_neigh++) {
#pragma omp taskwait depend(in : idx_x, idx_x_neigh, idx_y, idx_y_neigh, idx_z, idx_z_neigh, nb_cells_per_dim) depend(inout : neighbor)
                neighbor = ((idx_x + idx_x_neigh + nb_cells_per_dim) % nb_cells_per_dim * nb_cells_per_dim + (idx_y + idx_y_neigh + nb_cells_per_dim) % nb_cells_per_dim) * nb_cells_per_dim + (idx_z + idx_z_neigh + nb_cells_per_dim) % nb_cells_per_dim;
                const Particle_symb* neighbor_particles_symb = particles_symb[neighbor];
#pragma omp task default(shared) depend(in : me, neighbor, particles_symb, sizes, sizes[me], sizes[neighbor]) depend(inout : particles_forces)
                cell_neighbor_compute(me_particles_symb, me_particles_forces, sizes[me], neighbor_particles_symb, sizes[neighbor]);
              }
            }
          }
        }
      }
    }
  __apac_exit:;
  }
}

void grid_update(const int nb_particles, const int nb_cells_per_dim, const double box_width, const double cell_width, double time_step, int** sizes, Particle_symb*** particles_symb, Particle_forces*** particles_forces) {
#pragma omp taskgroup
  {
    int* src_sizes = *sizes;
    Particle_symb** src_particles_symb = *particles_symb;
    Particle_forces** src_particles_forces = *particles_forces;
    const int capacity = nb_cells_per_dim * nb_cells_per_dim * nb_cells_per_dim;
    *sizes = (int*)calloc(capacity, sizeof(int));
    *particles_symb = (Particle_symb**)calloc(capacity, sizeof(Particle_symb*));
    *particles_forces = (Particle_forces**)calloc(capacity, sizeof(Particle_forces*));
    for (int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
          const int* const cell_idx = new const int((idx_x * nb_cells_per_dim + idx_y) * nb_cells_per_dim + idx_z);
          for (int idxPart = 0; idxPart < src_sizes[*cell_idx]; idxPart++) {
#pragma omp taskwait depend(in : cell_idx[0], idxPart, particles_forces, time_step) depend(inout : particles_symb)
            src_particles_symb[*cell_idx][idxPart].vx += src_particles_forces[*cell_idx][idxPart].fx / src_particles_symb[*cell_idx][idxPart].weight * time_step;
            src_particles_symb[*cell_idx][idxPart].vy += src_particles_forces[*cell_idx][idxPart].fy / src_particles_symb[*cell_idx][idxPart].weight * time_step;
            src_particles_symb[*cell_idx][idxPart].vz += src_particles_forces[*cell_idx][idxPart].fz / src_particles_symb[*cell_idx][idxPart].weight * time_step;
            src_particles_symb[*cell_idx][idxPart].x += src_particles_symb[*cell_idx][idxPart].vx * time_step;
            src_particles_symb[*cell_idx][idxPart].y += src_particles_symb[*cell_idx][idxPart].vy * time_step;
            src_particles_symb[*cell_idx][idxPart].z += src_particles_symb[*cell_idx][idxPart].vz * time_step;
#pragma omp taskwait depend(in : cell_idx[0]) depend(inout : particles_symb)
            while (src_particles_symb[*cell_idx][idxPart].x < 0) {
#pragma omp taskwait depend(in : box_width, cell_idx[0], idxPart) depend(inout : particles_symb)
              src_particles_symb[*cell_idx][idxPart].x += box_width;
            }
#pragma omp taskwait depend(in : box_width, cell_idx[0]) depend(inout : particles_symb)
            while (src_particles_symb[*cell_idx][idxPart].x >= box_width) {
#pragma omp taskwait depend(in : box_width, cell_idx[0], idxPart) depend(inout : particles_symb)
              src_particles_symb[*cell_idx][idxPart].x -= box_width;
            }
#pragma omp taskwait depend(in : cell_idx[0]) depend(inout : particles_symb)
            while (src_particles_symb[*cell_idx][idxPart].y < 0) {
#pragma omp taskwait depend(in : box_width, cell_idx[0], idxPart) depend(inout : particles_symb)
              src_particles_symb[*cell_idx][idxPart].y += box_width;
            }
#pragma omp taskwait depend(in : box_width, cell_idx[0]) depend(inout : particles_symb)
            while (src_particles_symb[*cell_idx][idxPart].y >= box_width) {
#pragma omp taskwait depend(in : box_width, cell_idx[0], idxPart) depend(inout : particles_symb)
              src_particles_symb[*cell_idx][idxPart].y -= box_width;
            }
#pragma omp taskwait depend(in : cell_idx[0]) depend(inout : particles_symb)
            while (src_particles_symb[*cell_idx][idxPart].z < 0) {
#pragma omp taskwait depend(in : box_width, cell_idx[0], idxPart) depend(inout : particles_symb)
              src_particles_symb[*cell_idx][idxPart].z += box_width;
            }
#pragma omp taskwait depend(in : box_width, cell_idx[0]) depend(inout : particles_symb)
            while (src_particles_symb[*cell_idx][idxPart].z >= box_width) {
#pragma omp taskwait depend(in : box_width, cell_idx[0], idxPart) depend(inout : particles_symb)
              src_particles_symb[*cell_idx][idxPart].z -= box_width;
            }
            int* up_cell_idx = new int();
#pragma omp task default(shared) depend(in : cell_width, nb_cells_per_dim, particles_symb) depend(inout : up_cell_idx[0]) firstprivate(up_cell_idx)
            *up_cell_idx = grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[*cell_idx][idxPart]);
#pragma omp taskwait depend(in : up_cell_idx[0])
#pragma omp taskwait depend(in : *sizes, sizes, up_cell_idx[0]) depend(inout : (*sizes)[*up_cell_idx])
            (*sizes)[*up_cell_idx]++;
#pragma omp task default(shared) depend(inout : up_cell_idx[0]) firstprivate(up_cell_idx)
            delete up_cell_idx;
          }
#pragma omp task default(shared) depend(inout : cell_idx[0]) firstprivate(cell_idx)
          delete cell_idx;
        }
      }
    }
    for (int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
          const int cell_idx = (idx_x * nb_cells_per_dim + idx_y) * nb_cells_per_dim + idx_z;
          (*particles_symb)[cell_idx] = (Particle_symb*)calloc((*sizes)[cell_idx], sizeof(Particle_symb));
          (*particles_forces)[cell_idx] = (Particle_forces*)calloc((*sizes)[cell_idx], sizeof(Particle_forces));
        }
      }
    }
    int* cpt;
    cpt = (int*)calloc(capacity, sizeof(int));
    for (int idx_x = 0; idx_x < nb_cells_per_dim; idx_x++) {
      for (int idx_y = 0; idx_y < nb_cells_per_dim; idx_y++) {
        for (int idx_z = 0; idx_z < nb_cells_per_dim; idx_z++) {
          const int* const cell_idx = new const int((idx_x * nb_cells_per_dim + idx_y) * nb_cells_per_dim + idx_z);
          for (int idxPart = 0; idxPart < src_sizes[*cell_idx]; idxPart++) {
            int* up_cell_idx = new int();
#pragma omp task default(shared) depend(in : cell_width, nb_cells_per_dim, particles_symb) depend(inout : up_cell_idx[0]) firstprivate(up_cell_idx)
            *up_cell_idx = grid_cell_idx_from_position(cell_width, nb_cells_per_dim, src_particles_symb[*cell_idx][idxPart]);
#pragma omp taskwait depend(in : up_cell_idx[0])
#pragma omp taskwait depend(in : (*particles_symb)[*up_cell_idx], *particles_symb, cpt, cpt[*up_cell_idx], particles_symb, up_cell_idx[0]) depend(inout : (*particles_symb)[*up_cell_idx][cpt[*up_cell_idx]])
            (*particles_symb)[*up_cell_idx][cpt[*up_cell_idx]] = src_particles_symb[*cell_idx][idxPart];
#pragma omp taskwait depend(in : (*particles_forces)[*up_cell_idx], *particles_forces, cpt, cpt[*up_cell_idx], particles_forces, up_cell_idx[0]) depend(inout : (*particles_forces)[*up_cell_idx][cpt[*up_cell_idx]])
            (*particles_forces)[*up_cell_idx][cpt[*up_cell_idx]] = src_particles_forces[*cell_idx][idxPart];
#pragma omp taskwait depend(in : cpt, up_cell_idx[0]) depend(inout : cpt[*up_cell_idx])
            cpt[*up_cell_idx]++;
#pragma omp task default(shared) depend(inout : up_cell_idx[0]) firstprivate(up_cell_idx)
            delete up_cell_idx;
          }
#pragma omp task default(shared) depend(inout : cell_idx[0]) firstprivate(cell_idx)
          delete cell_idx;
        }
      }
    }
    free(cpt);
#pragma omp task default(shared) depend(in : nb_cells_per_dim) depend(inout : particles_forces, particles_symb, sizes)
    grid_destroy(nb_cells_per_dim, &src_sizes, &src_particles_symb, &src_particles_forces);
  __apac_exit:;
  }
}

void fill_cell_with_rand_particles(Cell* inCell, double box_width, int size) {
#pragma omp taskgroup
  {
    initialize_random_number_generator(box_width);
    for (int idx = 0; idx < size; idx++) {
      Particle_symb* particle = new Particle_symb();
#pragma omp taskwait depend(inout : particle[0])
      particle->x = random_number();
      particle->y = random_number();
      particle->z = random_number();
      particle->weight = 1.;
      particle->vx = 0.;
      particle->vy = 0.;
      particle->vz = 0.;
      Particle_forces* particle_forces = new Particle_forces();
#pragma omp taskwait depend(inout : particle_forces[0])
      particle_forces->fx = random_number();
      particle_forces->fy = random_number();
      particle_forces->fz = random_number();
#pragma omp task default(shared) depend(in : inCell, particle[0], particle_forces[0]) depend(inout : inCell[0]) firstprivate(particle) firstprivate(particle_forces)
      cell_add_particle(inCell, *particle, *particle_forces);
#pragma omp task default(shared) depend(inout : particle[0]) firstprivate(particle)
      delete particle;
#pragma omp task default(shared) depend(inout : particle_forces[0]) firstprivate(particle_forces)
      delete particle_forces;
    }
  __apac_exit:;
  }
}

int main() {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    const double box_width = 1;
    const double cell_width = 0.2;
    const int steps = 5;
    const double time_step = 0.001;
    const int size = 20000;
    Cell cell;
#pragma omp task default(shared) depend(in : size) depend(inout : cell)
    cell = cell_create(size);
#pragma omp task default(shared) depend(in : box_width, size) depend(inout : cell)
    fill_cell_with_rand_particles(&cell, box_width, size);
    int* sizes = NULL;
    Particle_symb** particles_symb = NULL;
    Particle_forces** particles_forces = NULL;
    int nb_cells_per_dim;
#pragma omp task default(shared) depend(in : box_width, cell, cell_width) depend(inout : nb_cells_per_dim, particles_forces, particles_forces[0], particles_forces[0][0], particles_symb, particles_symb[0], particles_symb[0][0], sizes, sizes[0])
    nb_cells_per_dim = grid_create(box_width, cell_width, cell.particles_symb, cell.particles_forces, cell.size, &sizes, &particles_symb, &particles_forces);
#pragma omp task default(shared) depend(inout : cell)
    cell_destroy(&cell);
    for (int idx = 0; idx < steps; idx++) {
#pragma omp task default(shared) depend(in : box_width, cell_width, nb_cells_per_dim, size, time_step) depend(inout : particles_forces, particles_forces[0], particles_forces[0][0], particles_symb, particles_symb[0], particles_symb[0][0], sizes, sizes[0])
      {
        grid_compute(nb_cells_per_dim, sizes, particles_symb, particles_forces);
        grid_update(size, nb_cells_per_dim, box_width, cell_width, time_step, &sizes, &particles_symb, &particles_forces);
      }
    }
#pragma omp task default(shared) depend(in : nb_cells_per_dim) depend(inout : particles_forces, particles_forces[0], particles_forces[0][0], particles_symb, particles_symb[0], particles_symb[0][0], sizes, sizes[0])
    grid_destroy(nb_cells_per_dim, &sizes, &particles_symb, &particles_forces);
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
