CLASS ltcl_newtons_method DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_newtons_method.

    METHODS:
      setup,
      sqrt_is_correct_for_2         FOR TESTING,
      sqrt_is_correct_for_137       FOR TESTING,
      sqrt_is_correct_for_sqrt_1000 FOR TESTING .
ENDCLASS.


CLASS ltcl_newtons_method IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_newtons_method( ).
  ENDMETHOD.

  METHOD sqrt_is_correct_for_2.
    cl_abap_unit_assert=>assert_equals( exp = '1.4142'
                                        act = CONV dec15_4( cut->square_root( 2 ) ) ).
  ENDMETHOD.

  METHOD sqrt_is_correct_for_137.
    cl_abap_unit_assert=>assert_equals( exp = '11.7047'
                                        act = CONV dec15_4( cut->square_root( 137 ) ) ).
  ENDMETHOD.

  METHOD sqrt_is_correct_for_sqrt_1000.
    cl_abap_unit_assert=>assert_equals( exp = '1000.0000'
                                        act = CONV dec15_4( cut->square_root( 1000 ) * cut->square_root( 1000 ) ) ).
  ENDMETHOD.

ENDCLASS.
