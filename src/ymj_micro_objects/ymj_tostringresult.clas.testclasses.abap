CLASS ltcl_fizzresult DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_1_as_string FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_fizzresult IMPLEMENTATION.

  METHOD should_return_1_as_string.
    DATA(cut) = NEW ymj_tostringresult( NEW ymj_input( 1 ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1' act = cut->convert2string( ) ).
  ENDMETHOD.

ENDCLASS.
