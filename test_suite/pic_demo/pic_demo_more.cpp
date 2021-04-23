
// TODO: should be able to replace
       particle p2 = { speed2, pos2 };
        bag* b2 = &bagsNext[idCell2];
        bag_push(b2, p2);
// with
  bag_push(&bagsNext[idCell2], { speed2, pos2 })  
  

  =========================



/*

for (int idCell...)

<= flattening of the nest loops into one loop
=> tiling

  for( int x = 0; x < gridX; x++) {
      for (int y = 0; y < gridY; y++) {
        for (int z = 0; z < gridZ; z++) {}
           int idCell = x * gridY * gridZ + y * gridY + z;
        }
      }
    }
  */
 /* LATEr: bag& b = bagsCur[idCell];   put back this in the code */





// Particle description
const double charge = 1.0;
 //TODO:later vect_mul(charge, field));
 // (LATER: linked lists of fixed-sized chunks)

 // For simplicity, we assume in this file that a given cell never
// contains more than bagCapacity particles. The real implementation,
// uses a linked list of fixed-capacity bags.

particle items[bagCapacity]; //TODO: check on unit test hte inlining of type particle in case of multidimensional array


bag_push
// assert(b.nb < bagCapacity);

// Note: in the real code, bags are linked lists,
  // so this operation only involves a pointer assignment,
  // not a deep copy of an array.
  

  // LATER: in the real code, the charge is associated not to each cell,
// but to each corner of a cell in the grid.

   //TODO:later vect_mul(charge, field));

// inlining
        //vect v1,v2;
        //---compare with vect v3= vect_add(v1,v2);
        //vect v3 = v_add(v1,v2);
        // vect v3;
        // v3 = v_add(v1,v2);
        

        /* goal is to generate:
        speed2.x = p.speed.x + charge * field.x;
        ..
        */


      