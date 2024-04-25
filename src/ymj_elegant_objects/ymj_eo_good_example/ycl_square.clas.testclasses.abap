CLASS ltcl_square DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_has_obstacle     FOR TESTING,
      should_not_has_obstacle FOR TESTING.
ENDCLASS.


CLASS ltcl_square IMPLEMENTATION.

  METHOD should_has_obstacle.
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = NEW ycl_square( has_obstacle = abap_true
                                                                              coordinates = VALUE #( coordinate_x = 1 coordinate_y = 1 ) )->yif_square~scan( ) ).
  ENDMETHOD.

  METHOD should_not_has_obstacle.
    cl_abap_unit_assert=>assert_equals( exp = abap_false act = NEW ycl_square( has_obstacle = abap_false
                                                                               coordinates = VALUE #( coordinate_x = 2 coordinate_y = 1 ) )->yif_square~scan( ) ).
  ENDMETHOD.

ENDCLASS.
