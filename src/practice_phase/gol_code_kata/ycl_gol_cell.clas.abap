CLASS ycl_gol_cell DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_cell.

    METHODS constructor IMPORTING alive       TYPE abap_bool
                                  coordinates TYPE yif_gol_neighbourhood=>ts_coordinates.

  PRIVATE SECTION.
    DATA alive          TYPE abap_bool.
    DATA coordinates    TYPE yif_gol_neighbourhood=>ts_coordinates.
    DATA neighbourhood  TYPE REF TO yif_gol_neighbourhood.

    METHODS count_living_neighbours RETURNING VALUE(result) TYPE int1.

ENDCLASS.

CLASS ycl_gol_cell IMPLEMENTATION.

  METHOD constructor.
    me->alive         = alive.
    me->coordinates   = coordinates.
  ENDMETHOD.

  METHOD yif_gol_cell~process.
    me->alive = COND #( WHEN count_living_neighbours( ) < 2 THEN abap_false
                        WHEN count_living_neighbours( ) = 2
                        OR   count_living_neighbours( ) = 3 THEN abap_true
                        WHEN count_living_neighbours( ) > 3 THEN abap_false ).
  ENDMETHOD.

  METHOD yif_gol_cell~is_alive.
    result = me->alive.
  ENDMETHOD.

  METHOD count_living_neighbours.
    result = neighbourhood->count_living( ).
  ENDMETHOD.

  METHOD yif_gol_cell~get_neighbours.
    result = neighbourhood.
  ENDMETHOD.

  METHOD yif_gol_cell~set_neighbours.
    me->neighbourhood = neighbourhood.
  ENDMETHOD.

ENDCLASS.

