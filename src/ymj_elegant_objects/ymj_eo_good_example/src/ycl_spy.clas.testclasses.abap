CLASS ltd_spy DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS create_spy_with_found_obst RETURNING VALUE(result) TYPE REF TO yif_spy.

ENDCLASS.

CLASS ltd_spy IMPLEMENTATION.

  METHOD create_spy_with_found_obst.
    result = NEW ycl_spy( ).
    result->notice_square( NEW ycl_square( coordinates  = VALUE #( coordinate_x = 1 coordinate_y = 1 )
                                           has_obstacle = abap_true
                                           spy          = result ) ).
  ENDMETHOD.

ENDCLASS.
CLASS ltcl_spy DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_coordinates FOR TESTING.

ENDCLASS.

CLASS ltcl_spy IMPLEMENTATION.

  METHOD should_return_coordinates.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1 coordinate_y = 1 )
                                        act = ltd_spy=>create_spy_with_found_obst( )->report_obstacle_coordinates(  ) ).
  ENDMETHOD.

ENDCLASS.
