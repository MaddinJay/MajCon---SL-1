CLASS ycl_marsrover DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_marsrover.
    METHODS constructor IMPORTING geo_data TYPE REF TO yif_geo_data
                                  map      TYPE REF TO yif_map
                                  spy      TYPE REF TO yif_spy.

  PRIVATE SECTION.
    DATA map           TYPE REF TO yif_map.
    DATA actual_square TYPE REF TO yif_square.
    DATA geo_data      TYPE REF TO yif_geo_data.
    DATA spy           TYPE REF TO yif_spy.

    METHODS move IMPORTING index TYPE int1.

    METHODS move_next_and_scan IMPORTING index         TYPE int1
                               RETURNING VALUE(result) TYPE yif_coordinates=>coordinates.
    METHODS move_back2previous_coordinates RETURNING VALUE(result) TYPE  yif_coordinates=>coordinates.
    METHODS position_start_point_on_map.
    METHODS calibrate_map.
    METHODS move_back IMPORTING index TYPE int1.
    METHODS scan_square.
    METHODS obstacle_found RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS ycl_marsrover IMPLEMENTATION.

  METHOD constructor.
    me->geo_data = geo_data.
    me->map      = map.
    me->spy      = spy.
  ENDMETHOD.

  METHOD yif_marsrover~drive.
    calibrate_map( ).
    move_next_and_scan( index = 1 ).
  ENDMETHOD.

  METHOD calibrate_map.
    map->calibrate( ).
  ENDMETHOD.

  METHOD position_start_point_on_map.
    map->set_starting_point( geo_data ).
  ENDMETHOD.

  METHOD move.
    actual_square = map->move2neighbour( index ).
  ENDMETHOD.

  METHOD move_next_and_scan.
    move( index ).
    scan_square( ).
    IF obstacle_found( ). " Obstacle found -> Move back and stop
      move_back( index ).
      RETURN.
    ENDIF.

    move_next_and_scan( index + 1 ).
  ENDMETHOD.

  METHOD obstacle_found.
    result = xsdbool( spy->report_obstacle_coordinates( ) IS NOT INITIAL ).
  ENDMETHOD.

  METHOD scan_square.
    actual_square->scan4obstacle( ).
  ENDMETHOD.

  METHOD move_back2previous_coordinates.
    result = map->read_previous_coordinates( ).
  ENDMETHOD.

  METHOD move_back.
    actual_square = map->move_back( index ).
  ENDMETHOD.

ENDCLASS.
