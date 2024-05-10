CLASS ltd_square DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS create_square_with_obstacle RETURNING VALUE(result) TYPE REF TO yif_spy.
    CLASS-METHODS create_square_wout_obstacle RETURNING VALUE(result) TYPE REF TO yif_spy.

ENDCLASS.

CLASS ltd_square IMPLEMENTATION.

  METHOD create_square_with_obstacle.
    result       = NEW ycl_spy( ).
    DATA(square) = NEW ycl_square( has_obstacle = abap_true
                                   coordinates  = VALUE #( coordinate_x = 1
                                                           coordinate_y = 1 )
                                   spy          = result ).
    square->scan4obstacle( ).
  ENDMETHOD.

  METHOD create_square_wout_obstacle.
    result       = NEW ycl_spy( ).
    DATA(square) = NEW ycl_square( has_obstacle = abap_false
                                   coordinates  = VALUE #( coordinate_x = 2
                                                           coordinate_y = 1 )
                                   spy          = result ).
    square->scan4obstacle( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_square DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_have_obstacle        FOR TESTING,
      should_not_has_obstacle     FOR TESTING.

ENDCLASS.

CLASS ltcl_square IMPLEMENTATION.

  METHOD should_have_obstacle.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1
                                                                                  coordinate_y = 1 )
                                        act = ltd_square=>create_square_with_obstacle( )->report_obstacle_coordinates( ) ).
  ENDMETHOD.

  METHOD should_not_has_obstacle.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates(  )
                                        act = ltd_square=>create_square_wout_obstacle( )->report_obstacle_coordinates( ) ).
  ENDMETHOD.

ENDCLASS.
