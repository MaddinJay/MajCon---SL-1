INTERFACE yif_coordinates
  PUBLIC .
  TYPES: BEGIN OF coordinates,
           coordinate_x TYPE int1,
           coordinate_y TYPE c LENGTH 1,
         END OF coordinates.
  METHODS determine_coord_neighbour IMPORTING coordinates   TYPE coordinates
                                              direction     TYPE yif_sequence=>direction
                                    RETURNING VALUE(result) TYPE coordinates.
ENDINTERFACE.
