INTERFACE yif_gol_grid_monitor
  PUBLIC .
  TYPES: BEGIN OF ts_grid_alv,
           x_1 TYPE flag,
           x_2 TYPE flag,
           x_3 TYPE flag,
           x_4 TYPE flag,
           x_5 TYPE flag,
         END OF ts_grid_alv,
         tt_grid_alv TYPE STANDARD TABLE OF ts_grid_alv WITH DEFAULT KEY.

  METHODS display IMPORTING grid TYPE yif_gol_grid=>tt_grid.

ENDINTERFACE.
