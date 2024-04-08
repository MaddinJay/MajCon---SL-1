CLASS ltcl_buzzaction DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_buzzresult FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_buzzaction IMPLEMENTATION.

  METHOD should_return_buzzresult.
    DATA(cut) = NEW ycl_buzzaction( next_action = NEW ycl_tostringaction( ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( cut->act( NEW ymj_input( 5 ) ) IS INSTANCE OF ycl_buzzresult ) ).
  ENDMETHOD.

ENDCLASS.
