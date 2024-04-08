CLASS ltcl_fizzbuzz DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_fizzbuzz.

    METHODS:
      should_return_string1_for_1   FOR TESTING,
      should_return_fizz_for_3      FOR TESTING,
      should_return_buzz_for_5      FOR TESTING,
      should_return_fizzbuzz_for_15 FOR TESTING,
      should_return_buzz_for_100    FOR TESTING.
ENDCLASS.


CLASS ltcl_fizzbuzz IMPLEMENTATION.

  METHOD should_return_string1_for_1.
    cl_abap_unit_assert=>assert_equals( exp = '1'
                                        act = NEW ycl_fizzbuzz( )->convert( NEW ymj_input( 1 ) )->convert2string( ) ).
  ENDMETHOD.

  METHOD should_return_fizz_for_3.
    cl_abap_unit_assert=>assert_equals( exp = 'Fizz'
                                        act = NEW ycl_fizzbuzz( )->convert( NEW ymj_input( 3 ) )->convert2string( ) ).
  ENDMETHOD.

  METHOD should_return_buzz_for_5.
    cl_abap_unit_assert=>assert_equals( exp = 'Buzz'
                                        act = NEW ycl_fizzbuzz( )->convert( NEW ymj_input( 5 ) )->convert2string( ) ).
  ENDMETHOD.

  METHOD should_return_fizzbuzz_for_15.
    cl_abap_unit_assert=>assert_equals( exp = 'FizzBuzz'
                                        act = NEW ycl_fizzbuzz( )->convert( NEW ymj_input( 15 ) )->convert2string( ) ).
  ENDMETHOD.

  METHOD should_return_buzz_for_100.
    cl_abap_unit_assert=>assert_equals( exp = 'Buzz'
                                        act = NEW ycl_fizzbuzz( )->convert( NEW ymj_input( 100 ) )->convert2string( ) ).
  ENDMETHOD.

ENDCLASS.
