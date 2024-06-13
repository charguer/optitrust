

// =========================================================
// Core loop


  int nbParts = bagsSize[MINDEX2(2, nbCells, idStep, idCell)];
  for (int idPart = 0; idPart < nbParts; idPart++) {
    // __reads bags[idStep] // but then __modifies for loop splitting
    // __modifies bags[idStep+1]

    particle* p = &bags[MINDEX3(2, nbCells, maxPartsPerCell, idStep, idCell, idPart)];

    // Interpolate the field based on the position relative to the corners of the cell
    double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
    vect fieldAtPos = matrix_vect_mul(coeffs, fieldAtCorners);

    // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
    vect accel = vect_mul(p->charge / p->mass, fieldAtPos);

    // Compute the new speed and position for the particle.
    vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
    vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
    particle p2 = { pos2, speed2 };

    // Compute the location of the cell that now contains the particle
    int idCellNext = idCellOfPos(pos2);

    // Push the updated particle into the bag associated with its target cell
    int idPartNext = bagsSize[MINDEX2(2, nbCells, (idStep+1)%2, idCellNext)]++;
    bags[MINDEX3(2, nbCells, maxPartsPerCell, (idStep+1)%2, idCellNext, idPartNext)] = p2;
  }
  bagsSize[MINDEX2(2, nbCells, idStep%2, idCell)] = 0;


// =========================================================

// 1. dérouler matmuls
// 2. mise à l'échelle données
//   -- http://www.chargueraud.org/research/2022/optitrust/optitrust.pdf sect 4.5

// --optional
// 3. AoS -> SoA
// 4. AoSoA

// --optional
// 5. loop split
// 6. add modulo-2 on bag structures
