CLASS ltcl_breadth_first_search DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_breadth_first_search.

    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_breadth_first_search IMPLEMENTATION.

  METHOD first_test.
    cut = NEW #( queue = VALUE ycl_breadth_first_search=>tt_queue(
                                 ( name = 'YOU' friend = 'alice' )
                                 ( name = 'YOU' friend = 'bob' )
                                 ( name = 'YOU' friend = 'claire' )
                                 ( name = 'alice' friend = 'peggy' )
                                 ( name = 'bob'   friend = 'peggy' )
                                 ( name = 'claire' friend = 'thom' )
                                 ( name = 'claire' friend = 'jonny' ) )
                 starting_name = 'YOU' ).
    cl_abap_unit_assert=>assert_equals( exp = 'thom is a mango seller!'
                                        act = cut->search( ) ).
  ENDMETHOD.

ENDCLASS.
