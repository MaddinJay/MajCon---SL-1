CLASS ltcl_tostringaction DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ymj_if_fizzbuzzaction.
    METHODS:
      should_return_tostringresult FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_tostringaction IMPLEMENTATION.

  METHOD should_return_tostringresult.
    cut = NEW ycl_tostringaction( ).
    cl_abap_unit_assert=>assert_true( xsdbool( cut->act( input =  NEW ymj_input( 1 ) ) IS INSTANCE OF ycl_tostringresult ) ).
  ENDMETHOD.

ENDCLASS.
