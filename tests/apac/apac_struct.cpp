#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int x, y;
} Point;

typedef struct {
  Point p1, p2;
} Segment;

typedef struct {
  Point * pts;
  size_t angles;
} Polygone;

Point move_point(Point pt) {
  pt.x = pt.x * 2;
  pt.y *= 2;
  return pt;
}

void test_point(Point * pt) {
  Point tmp;
  tmp.x = 1;
  tmp.y = 10;
  tmp = move_point(tmp);
  pt->x = tmp.x * 2;
  pt->y = tmp.y / 5;
  (*pt) = move_point(*pt);  
}

void test_segment(Segment * sgmt) {
  sgmt->p1 = move_point(sgmt->p1);
  test_point(&sgmt->p2);
}

void test_polygone(Polygone * poly) {
  for(size_t i = 0; i < poly->angles; i++) {
    (*(poly->pts + i)).x = i;
    poly->pts[i] = move_point(poly->pts[i]);
    test_point(&poly->pts[i]);
  }
}