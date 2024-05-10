CLASS ycl_spy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_spy.

  PRIVATE SECTION.
    DATA square TYPE REF TO yif_square.

ENDCLASS.

CLASS ycl_spy IMPLEMENTATION.

  METHOD yif_spy~report_obstacle_coordinates.
    result = COND #( WHEN square IS BOUND THEN square->read_coordinates( ) ).
  ENDMETHOD.

  METHOD yif_spy~notice_square.
    me->square = square.
  ENDMETHOD.

ENDCLASS.
