CLASS ltcl_input DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_1_as_integer FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_input IMPLEMENTATION.

  METHOD should_return_1_as_integer.
    DATA(cut) = NEW ycl_input( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->int( ) ).
  ENDMETHOD.

ENDCLASS.
