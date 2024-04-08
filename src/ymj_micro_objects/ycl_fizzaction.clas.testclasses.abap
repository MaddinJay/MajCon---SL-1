CLASS ltcl_fizzaction DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_fizzresult FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_fizzaction IMPLEMENTATION.

  METHOD should_return_fizzresult.
    DATA(cut) = NEW ycl_fizzaction( next_action = NEW ycl_tostringaction( ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( cut->act( NEW ymj_input( 3 ) ) IS INSTANCE OF ycl_fizzresult ) ).
  ENDMETHOD.

ENDCLASS.
