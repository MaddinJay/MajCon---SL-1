CLASS ltcl_hash_tables DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      should_return_let_them_vote FOR TESTING RAISING cx_static_check,
      should_return_kick_them_out FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_hash_tables IMPLEMENTATION.

  METHOD should_return_let_them_vote.
    DATA cut TYPE REF TO ycl_hash_tables.
    cut = NEW #( ).
    cl_abap_unit_assert=>assert_equals( exp = 'Let them vote!'
                                        act = cut->check_voter( 'Hans' ) ).
  ENDMETHOD.

  METHOD should_return_kick_them_out.
    DATA cut TYPE REF TO ycl_hash_tables.
    cut = NEW #( ).
    cut->check_voter( 'Hans' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Kick them out!'
                                        act = cut->check_voter( 'Hans' ) ).
  ENDMETHOD.
ENDCLASS.
