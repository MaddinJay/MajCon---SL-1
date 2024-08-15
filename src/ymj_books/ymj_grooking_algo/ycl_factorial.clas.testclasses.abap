CLASS ltcl_factorial DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      fac_4_should_result_in_24 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_factorial IMPLEMENTATION.

  METHOD fac_4_should_result_in_24.
    cl_abap_unit_assert=>assert_equals( exp = 24
                                        act = NEW ycl_factorial( )->fact( 4 ) ).
  ENDMETHOD.

ENDCLASS.
