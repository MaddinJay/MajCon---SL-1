*&---------------------------------------------------------------------*
*& Report y_mit_expressions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_expressions.

CLASS lcl_expression DEFINITION.

  PUBLIC SECTION.
    METHODS square IMPORTING number        TYPE int4
                   RETURNING VALUE(result) TYPE int4.
    METHODS sum_of_squares IMPORTING number1       TYPE int4
                                     number2       TYPE int4
                           RETURNING VALUE(result) TYPE int4.
    METHODS function IMPORTING number        TYPE int4
                     RETURNING VALUE(result) TYPE int4.
    METHODS absolut IMPORTING number        TYPE int4
                    RETURNING VALUE(result) TYPE int4.
    METHODS greater_or_equal IMPORTING number1       TYPE int4
                                       number2       TYPE int4
                             RETURNING VALUE(result) TYPE abap_bool.
    METHODS greater_or_equal_alt IMPORTING number1       TYPE int4
                                           number2       TYPE int4
                                 RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_expression IMPLEMENTATION.

  METHOD square.
    result = number * number.
  ENDMETHOD.

  METHOD sum_of_squares.
    result = square( number1 ) + square( number2 ).
  ENDMETHOD.

  METHOD function.
    result = sum_of_squares( number1 = number + 1 number2 = number * 2 ).
  ENDMETHOD.

  METHOD absolut.
    result = COND #( WHEN number > 0 THEN number
                     WHEN number = 0 THEN 0
                     ELSE - number ).
  ENDMETHOD.

  METHOD greater_or_equal.
    result = xsdbool( number1 > number2 OR number1 = number2 ). " OR in javascript ||, AND in javascript &&
  ENDMETHOD.

  METHOD greater_or_equal_alt.
    result = xsdbool( xsdbool( number1 < number2 ) <> abap_true ). " inverse in javascript !
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_expressions DEFINITION FINAL FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
        cut TYPE REF TO lcl_expression.
    METHODS:
      setup,
      square_method_arg_as_sum FOR TESTING,
      square_method_arg_square FOR TESTING,
      sum_of_squares FOR TESTING,
      test_function FOR TESTING,
      test_absolute FOR TESTING,
      test_greater_or_equal FOR TESTING.
ENDCLASS.


CLASS ltcl_expressions IMPLEMENTATION.

  METHOD setup.
    cut = NEW lcl_expression( ).
  ENDMETHOD.

  METHOD square_method_arg_as_sum.
    cl_abap_unit_assert=>assert_equals( exp = 49 act = cut->square( ( 5 + 2 ) ) ).
  ENDMETHOD.

  METHOD square_method_arg_square.
    cl_abap_unit_assert=>assert_equals( exp = 81 act = cut->square( cut->square( 3 ) ) ).
  ENDMETHOD.

  METHOD sum_of_squares.
    cl_abap_unit_assert=>assert_equals( exp = 25 act = cut->sum_of_squares( number1 = 3 number2 = 4 ) ).
  ENDMETHOD.

  METHOD test_function.
    cl_abap_unit_assert=>assert_equals( exp = 136 act = cut->function( number = 5 ) ).
  ENDMETHOD.

  METHOD test_absolute.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = cut->absolut( 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = cut->absolut( 0 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = cut->absolut( -3 ) ).
  ENDMETHOD.

  METHOD test_greater_or_equal.
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = cut->greater_or_equal(
                                                                        number1 = 6
                                                                        number2 = 5 ) ).
  ENDMETHOD.

ENDCLASS.
