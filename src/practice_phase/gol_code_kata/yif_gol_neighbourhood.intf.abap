INTERFACE yif_gol_neighbourhood
  PUBLIC .
  TYPES tt_neighbours TYPE TABLE OF REF TO yif_gol_cell WITH DEFAULT KEY.
  TYPES: BEGIN OF ts_coordinates,
           x TYPE int1,
           y TYPE int1,
         END OF ts_coordinates.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Create neighbourhoud for a specific cell
  "! @parameter grid | <p class="shorttext synchronized" lang="en">Grid which is given</p>
  "! @parameter cell_coordinates | <p class="shorttext synchronized" lang="en">Coordinates of cell</p>
  METHODS create IMPORTING grid             TYPE yif_gol_grid=>tt_grid
                           cell_coordinates TYPE ts_coordinates.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Counting the living cells in the neighbourhood
  "! @parameter result | <p class="shorttext synchronized" lang="en">Number of living neighbours</p>
  METHODS count_living RETURNING VALUE(result) TYPE int1.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get neighbours of a cell
  "! @parameter result | <p class="shorttext synchronized" lang="en">Neighbours of cell</p>
  METHODS get_neighbours RETURNING VALUE(result) TYPE tt_neighbours.

ENDINTERFACE.
