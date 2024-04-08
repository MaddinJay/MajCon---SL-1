*"* use this source file for your ABAP unit test classes
CLASS ltc_fizzbuzz DEFINITION FOR TESTING
                   RISK LEVEL HARMLESS
                   DURATION SHORT.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO ycl_fizzbuzz_foundation_level.

    METHODS:
      setup,
      check_get_fizzbuzz FOR TESTING.
ENDCLASS.

CLASS ltc_fizzbuzz IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD check_get_fizzbuzz.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Error'
        act                  = mo_cut->get_fizzbuzz( -1 )
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Error'
        act                  = mo_cut->get_fizzbuzz( 0 )
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 1
        act                  = mo_cut->get_fizzbuzz( 1 )
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Fizz'
        act                  = mo_cut->get_fizzbuzz( 3 )
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 4
        act                  = mo_cut->get_fizzbuzz( 4 )
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Buzz'
        act                  = mo_cut->get_fizzbuzz( 5 )
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'FizzBuzz'
        act                  = mo_cut->get_fizzbuzz( 15 )
    ).

  ENDMETHOD.
ENDCLASS.
