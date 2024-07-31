CLASS ltcl_quick_sort DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "GIVEN: List with one element WHEN: Quick Sort is executed THEN: ...
      should_return_list FOR TESTING,
      "GIVEN: List with five elements WHEN: Quick Sort is executed THEN: ...
      shoudl_return_sorted_list FOR TESTING.
ENDCLASS.

CLASS ltcl_quick_sort IMPLEMENTATION.

  METHOD should_return_list.
    DATA cut TYPE REF TO ycl_quick_sort.
    cut = NEW ycl_quick_sort( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE int4_table( ( 1 ) )
                                        act = cut->quicksort( VALUE int4_table( ( 1 ) ) ) ).
  ENDMETHOD.

  METHOD shoudl_return_sorted_list.
    DATA cut TYPE REF TO ycl_quick_sort.
    cut = NEW ycl_quick_sort( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
                                        act = cut->quicksort( VALUE int4_table( ( 3 ) ( 5 ) ( 2 ) ( 1 ) ( 4 ) ) ) ).
  ENDMETHOD.

ENDCLASS.
