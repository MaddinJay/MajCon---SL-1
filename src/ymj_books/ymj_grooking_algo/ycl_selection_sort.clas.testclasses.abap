CLASS ltcl_selection_sort DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_selection_sort.

    METHODS:
      setup,
      " GIVEN: Liste of int4, random order WHEN: Selection Sort is executed THEN:
      should_sort_list FOR TESTING.

ENDCLASS.

CLASS ltcl_selection_sort IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD should_sort_list.
    cl_abap_unit_assert=>assert_equals( exp =                      VALUE int4_table( ( 2 ) ( 3 ) ( 5 ) ( 6 ) ( 10 ) )
                                        act = cut->selection_sort( VALUE int4_table( ( 5 ) ( 3 ) ( 6 ) ( 2 ) ( 10 ) ) ) ).
  ENDMETHOD.

ENDCLASS.
