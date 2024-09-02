typedef struct {
  int x;
  int y;
} Point;

typedef struct {
  Point p1;
  Point p2;
} Segment;

typedef struct {
  Point* pts;
  long unsigned int angles;
} Polygone;

Point move_point(Point pt) {
  pt.x = pt.x * 2;
  pt.y *= 2;
  return pt;
}

void add_point(Point* pt1, const Point* pt2) {
  pt1->x += pt2->x;
  pt1->y += pt2->y;
}

void move_segment(Segment* sgmt) {
#pragma omp taskgroup
  {
#pragma omp task default(shared) depend(inout : sgmt)
    sgmt->p1 = move_point(sgmt->p1);
#pragma omp task default(shared) depend(inout : sgmt)
    add_point(&sgmt->p1, &sgmt->p2);
#pragma omp taskwait
  __apac_exit:;
  }
}

void move_polygone(Polygone* poly) {
#pragma omp taskgroup
  {
    for (int i = 1; i < poly->angles; i++) {
#pragma omp task default(shared) depend(inout : poly) firstprivate(i)
      poly->pts[i] = move_point(poly->pts[i]);
#pragma omp task default(shared) depend(inout : poly) firstprivate(i)
      add_point(&poly->pts[i - 1], &poly->pts[i]);
    }
#pragma omp taskwait
  __apac_exit:;
  }
}

void add_polygone(Polygone* poly1, Polygone* poly2) {
#pragma omp taskgroup
  {
    for (int i = 0; i < poly1->angles; i++) {
#pragma omp task default(shared) depend(in : poly2) depend(inout : poly1) firstprivate(i)
      add_point(&poly1->pts[i], &poly2->pts[i]);
    }
#pragma omp taskwait
  __apac_exit:;
  }
}

void mul_polygone(Polygone* poly1, Polygone* poly2) {
  for (int i = 0; i < poly1->angles; i++) {
    poly1->pts[i] = poly2->pts[i];
  }
}
