INTERFACE yif_square
  PUBLIC .
  METHODS scan4obstacle.
  METHODS read_coordinates RETURNING VALUE(result) TYPE yif_coordinates=>coordinates.
ENDINTERFACE.
