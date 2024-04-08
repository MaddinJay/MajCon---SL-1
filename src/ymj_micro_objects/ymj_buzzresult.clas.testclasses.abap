CLASS ltcl_fizzresult DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_string_buzz FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_fizzresult IMPLEMENTATION.

  METHOD should_return_string_buzz.
    DATA(cut) = NEW ymj_buzzresult( ).
    cl_abap_unit_assert=>assert_equals( exp = 'Buzz' act = cut->convert2string( ) ).
  ENDMETHOD.

ENDCLASS.
