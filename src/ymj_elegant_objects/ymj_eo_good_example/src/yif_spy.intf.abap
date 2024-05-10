INTERFACE yif_spy
  PUBLIC .
  METHODS notice_square IMPORTING square TYPE REF TO yif_square.
  METHODS report_obstacle_coordinates
    RETURNING
      VALUE(result) TYPE yif_coordinates=>coordinates.

ENDINTERFACE.
