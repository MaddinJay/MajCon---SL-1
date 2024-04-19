INTERFACE yif_neighbour_hood
  PUBLIC .
  TYPES: BEGIN OF ENUM direction,
           left ,
           right,
           up,
           down,
         END OF ENUM direction.
  METHODS add_neighbour IMPORTING direction TYPE direction
                                  neighbour TYPE REF TO ycl_square.
  METHODS go IMPORTING direction     TYPE direction
             RETURNING VALUE(result) TYPE REF TO ycl_square.
  METHODS scan_neighbour IMPORTING direction     TYPE direction
                         RETURNING VALUE(result) TYPE abap_bool.
  METHODS move2neighbour IMPORTING direction     TYPE direction
                         RETURNING VALUE(result) TYPE REF TO ycl_square.

ENDINTERFACE.
