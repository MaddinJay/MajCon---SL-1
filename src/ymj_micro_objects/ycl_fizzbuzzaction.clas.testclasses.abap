CLASS ltcl_fizzbuzzaction DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_fizzbuzzresult FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_fizzbuzzaction IMPLEMENTATION.

  METHOD should_return_fizzbuzzresult.
    DATA(cut) = NEW ycl_fizzbuzzaction( next_action = NEW ycl_tostringaction( ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( cut->act( NEW ymj_input( 15 ) ) IS INSTANCE OF ycl_fizzbuzzresult ) ).
  ENDMETHOD.

ENDCLASS.
