CLASS ltcl_binary_search DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_binary_search.
    METHODS:
      setup,

      " GIVEN: List of one name WHEN: Entry is Alfred
      should_return_position_one FOR TESTING,
      " GIVEN: List of two names WHEN: Searching for Alfred
      should_return_position_two FOR TESTING,
      " GIVEN: List of numbers WHEN: Searching for 3
      should_ret_pos_two_of_numlist FOR TESTING.

ENDCLASS.


CLASS ltcl_binary_search IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD should_return_position_one.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = cut->process( list = VALUE string_t( ( |Alfred| ) )
                                                            item = |Alfred| ) ).
  ENDMETHOD.

  METHOD should_return_position_two.
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = cut->process( list = VALUE string_t( ( |Albert| )
                                                                                   ( |Alfred| ) )
                                                            item = |Alfred| ) ).
  ENDMETHOD.

  METHOD should_ret_pos_two_of_numlist.
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = cut->process( list = VALUE string_t( ( |1| )
                                                                                   ( |3| )
                                                                                   ( |5| )
                                                                                   ( |7| )
                                                                                   ( |9| ) )
                                                            item = |3| ) ).
  ENDMETHOD.

ENDCLASS.
