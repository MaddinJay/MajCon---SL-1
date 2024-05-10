CLASS ycl_detection DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_detection.

  PRIVATE SECTION.
    DATA coordinates TYPE yif_coordinates=>coordinates.

ENDCLASS.

CLASS ycl_detection IMPLEMENTATION.

  METHOD yif_detection~obstacle_found.
    me->coordinates = coordinates.
  ENDMETHOD.

ENDCLASS.
