CLASS ycl_square DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_square.
    ALIASES: scan4obstacle FOR yif_square~scan4obstacle,
             read_coordinates FOR yif_square~read_coordinates.

    METHODS constructor IMPORTING has_obstacle TYPE abap_bool
                                  coordinates  TYPE yif_coordinates=>coordinates
                                  spy          TYPE REF TO yif_spy.

  PRIVATE SECTION.
    DATA has_obstacle TYPE abap_bool.
    DATA coordinates  TYPE yif_coordinates=>coordinates.
    DATA spy          TYPE REF TO yif_spy.

ENDCLASS.

CLASS ycl_square IMPLEMENTATION.

  METHOD constructor.
    me->has_obstacle = has_obstacle.
    me->coordinates  = coordinates.
    me->spy          = spy.
  ENDMETHOD.

  METHOD yif_square~scan4obstacle.
    IF has_obstacle = abap_true.
      spy->notice_square( me ).
    ENDIF.
  ENDMETHOD.

  METHOD yif_square~read_coordinates.
    result = coordinates.
  ENDMETHOD.

ENDCLASS.

