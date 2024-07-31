CLASS ltcl_sum_recursive DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "GIVEN: Empty list of numbers WHEN: SUM UP executed THEN: ...
      should_return_sum_zero FOR TESTING,
      "GIVEN: List with one number 7 WHEN: SUM UP is executed THEN: ...
      should_return_sum_7 FOR TESTING,
      "GIVEN: List of numbers WHEN: SUM UP is executed THEN: ...
      shoudl_return_sum_12 FOR TESTING.
ENDCLASS.


CLASS ltcl_sum_recursive IMPLEMENTATION.

  METHOD should_return_sum_zero.
    DATA(cut) = NEW ycl_sum_recursive( ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = cut->sum_up( VALUE int4_table( ( ) ) ) ).
  ENDMETHOD.

  METHOD should_return_sum_7.
    DATA(cut) = NEW ycl_sum_recursive( ).
    cl_abap_unit_assert=>assert_equals( exp = 7
                                        act = cut->sum_up( VALUE int4_table( ( 7 ) ) ) ).
  ENDMETHOD.

  METHOD shoudl_return_sum_12.
    DATA(cut) = NEW ycl_sum_recursive( ).
    cl_abap_unit_assert=>assert_equals( exp = 12
                                        act = cut->sum_up( VALUE int4_table( ( 2 ) ( 4 ) ( 6 ) ) ) ).
  ENDMETHOD.

ENDCLASS.
