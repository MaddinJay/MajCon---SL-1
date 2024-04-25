CLASS ycl_square DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_square.

    METHODS constructor IMPORTING has_obstacle TYPE abap_bool
                                  coordinates  TYPE yif_planet=>coordinates.

  PRIVATE SECTION.
    DATA has_obstacle TYPE abap_bool.
    DATA coordinates TYPE yif_planet=>coordinates.
    DATA neighbours TYPE REF TO ycl_neighbour_hood.
ENDCLASS.

CLASS ycl_square IMPLEMENTATION.

  METHOD constructor.
    me->has_obstacle = has_obstacle.
    me->coordinates = coordinates.
  ENDMETHOD.
  METHOD yif_square~scan.
    result = me->has_obstacle.

  ENDMETHOD.

ENDCLASS.
