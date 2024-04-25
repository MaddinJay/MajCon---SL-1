INTERFACE yif_marsrover
  PUBLIC .
  TYPES: BEGIN OF ENUM cardinal_point,
           north,
           east,
           south,
           west,
         END OF ENUM cardinal_point.

  METHODS scan_and_move.
ENDINTERFACE.
