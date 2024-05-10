CLASS ltcl_marsrover DEFINITION DEFERRED.
CLASS ycl_marsrover DEFINITION LOCAL FRIENDS ltcl_marsrover.

CLASS ltcl_marsrover DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_marsrover.
    DATA spy TYPE REF TO yif_spy.

    METHODS:
      setup,
      create_map_double RETURNING VALUE(result) TYPE REF TO yif_map,
      spy_should_report_position_1_1   FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_marsrover IMPLEMENTATION.

  METHOD setup.
    spy = NEW ycl_spy( ).
    DATA(map) = create_map_double( ).
    cut = NEW ycl_marsrover( geo_data = NEW ycl_geo_data( cardinal_point = yif_geo_data=>c_cardinal_point-north
                                              sequence_list  = VALUE yif_sequence=>tt_list( ( direction = yif_sequence=>c_direction-left ) )
                                              starting_point = VALUE yif_coordinates=>coordinates( coordinate_x = 2 coordinate_y = 1 ) )
                 map      = map
                 spy      = spy ).
    cut->drive( ).
  ENDMETHOD.

  METHOD create_map_double.
    DATA(square) = NEW ycl_square( coordinates  = VALUE #( coordinate_x = 1 coordinate_y = 1 )
                                   has_obstacle = abap_true
                                   spy          = spy ).
    result ?= cl_abap_testdouble=>create( 'yif_map' ).

    cl_abap_testdouble=>configure_call( result )->returning( square  ).
    result->move2neighbour( 1 ).

    cl_abap_testdouble=>configure_call( result )->returning( VALUE yif_coordinates=>coordinates( coordinate_x = 2 coordinate_y = 1 )  ).
    result->read_previous_coordinates( ).
  ENDMETHOD.

  METHOD spy_should_report_position_1_1.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1 coordinate_y = 1 )
                                        act = spy->report_obstacle_coordinates( ) ).
  ENDMETHOD.

ENDCLASS.
