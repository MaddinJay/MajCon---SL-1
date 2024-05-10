CLASS ltcl_detection DEFINITION DEFERRED.
CLASS ycl_detection DEFINITION LOCAL FRIENDS ltcl_detection.

CLASS ltcl_detection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_detection.
    METHODS:
      setup,
      should_have_coordinates FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_detection IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_detection( ).
  ENDMETHOD.

  METHOD should_have_coordinates.
    cut->yif_detection~obstacle_found( VALUE #( coordinate_x = 1 coordinate_y = 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1 coordinate_y = 1 )
                                        act = cut->coordinates  ).
  ENDMETHOD.

ENDCLASS.
