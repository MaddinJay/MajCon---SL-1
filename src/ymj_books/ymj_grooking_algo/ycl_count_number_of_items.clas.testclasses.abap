CLASS ltcl_count_number_of_items DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_count_number_of_items.

    METHODS:
      setup,
      "GIVEN: Empty list WHEN: Count is executed THEN: ...
      should_return_no_zero FOR TESTING,
      "GIVEN: List of three items WHEN: Count is executed THEN: ...
      should_return_no_three FOR TESTING.

ENDCLASS.

CLASS ltcl_count_number_of_items IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_count_number_of_items( ).
  ENDMETHOD.

  METHOD should_return_no_zero.
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = cut->count( VALUE int4_table( ) ) ).
  ENDMETHOD.

  METHOD should_return_no_three.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->count( VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ) ) ).
  ENDMETHOD.

ENDCLASS.
