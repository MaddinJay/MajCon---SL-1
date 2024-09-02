INTERFACE yif_gol_start_game
  PUBLIC .
  METHODS create IMPORTING size          TYPE int1
                 RETURNING VALUE(result) TYPE REF TO yif_gol_game.
ENDINTERFACE.
