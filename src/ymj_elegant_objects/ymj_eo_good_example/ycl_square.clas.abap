CLASS ycl_square DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_square.
    ALIASES: scan4obstacle FOR yif_square~scan4obstacle.

    METHODS constructor IMPORTING has_obstacle TYPE abap_bool
                                  coordinate_x TYPE int1
                                  coordinate_y TYPE c.

  PRIVATE SECTION.
    DATA has_obstacle TYPE abap_bool.
    DATA coordinate_x TYPE int1.
    DATA coordinate_y TYPE c.
    DATA neighbours TYPE REF TO ycl_neighbour_hood.
ENDCLASS.

CLASS ycl_square IMPLEMENTATION.

  METHOD constructor.
    me->has_obstacle = has_obstacle.
    me->coordinate_x = coordinate_x.
    me->coordinate_y = coordinate_y.
  ENDMETHOD.

  METHOD yif_square~scan4obstacle.
    result = me->has_obstacle.
  ENDMETHOD.

ENDCLASS.
