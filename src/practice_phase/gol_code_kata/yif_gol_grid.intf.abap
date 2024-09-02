INTERFACE yif_gol_grid
  PUBLIC .
  TYPES: BEGIN OF ts_coordinates,
           x TYPE int1,
           y TYPE int1,
         END OF ts_coordinates.

  TYPES: BEGIN OF ts_grid,
           coordinates TYPE ts_coordinates,
           cell        TYPE REF TO yif_gol_cell,
         END OF ts_grid,
         tt_grid TYPE STANDARD TABLE OF ts_grid WITH DEFAULT KEY.

  "! <p class="shorttext synchronized" lang="en">Creating the grid with a given size</p>
  "!
  "! @parameter size | <p class="shorttext synchronized" lang="en">Size of grid (size x size)</p>
  METHODS create IMPORTING size TYPE int1.

  METHODS display IMPORTING handler TYPE REF TO yif_gol_game.
  METHODS get_grid RETURNING VALUE(result) TYPE yif_gol_grid=>tt_grid.

ENDINTERFACE.
