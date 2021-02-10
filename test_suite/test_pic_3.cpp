/*
Idea introduce parrallelism using the coloring scheme

This transformation is a variant of tiling

This transformation should be done after x,y, z fields are exposed

*/
typedef struct{
    double x,y,z;
} vect;


const int gridSize = 64;
const int nbCells = gridSize * gridSize;

const int nbColors = 8;
const int nbCellsPerTile = 8;
const int nbCellsPerColor = nbCells / nbColors;  // must be exact
const int nbTiles = nbCells / nbCellsPerTile; // must be exact
const int nbTilesPerColor = nbTiles / nbColors;  // must be exact

vect fields[nbCells];

int nbSteps = 100;

//int idCellsOfTile[nbTiles][nbCellsPerTile];
int main() {
    for(int step=0; step<nbSteps; step++){
        int cur = step % 2;
        int next = (step + 1) % 2;

        //for each cell from the gri
        for(int idCell = 0; idCell < nbCells; idCell++){
            vect field = fields[idCell];

            
        }
    }   
    return 0;
}