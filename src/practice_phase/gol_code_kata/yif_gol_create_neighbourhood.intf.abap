INTERFACE yif_gol_create_neighbourhood
  PUBLIC .

  METHODS create IMPORTING grid             TYPE yif_gol_grid=>tt_grid
                           cell_coordinates TYPE yif_gol_neighbourhood=>ts_coordinates
                 RETURNING VALUE(result)    TYPE REF TO yif_gol_neighbourhood.
ENDINTERFACE.
