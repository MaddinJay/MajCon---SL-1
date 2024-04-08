class ycl_fizzbuzz_foundation_level definition
  public
  final
  create public .

public section.

  methods GET_FIZZBUZZ
    importing
      !IV_NUMBER type INT4
    returning
      value(RV_RESULT) type STRING .
  PROTECTED SECTION.
    DATA:
      mc_error      TYPE string VALUE 'Error',
      mc_fizz       TYPE string VALUE 'Fizz',
      mc_buzz       TYPE string VALUE 'Buzz',
      mc_fizzbuzz   TYPE string VALUE 'FizzBuzz'.

    METHODS set_result_fizzbuzz
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE string.

    METHODS is_dividable_by_15
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE abap_bool.

    METHODS set_result_fizz
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE string.

    METHODS is_dividable_by_3
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE abap_bool.

    METHODS set_result_buzz
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE string.

    METHODS is_dividable_by_5
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE abap_bool.

    METHODS set_result_as_number
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE string.

    METHODS is_numbar_equal_zero
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE abap_bool.

    METHODS check_input
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE string.

    METHODS is_number_lower_zero
      IMPORTING
        iv_number TYPE int4
      RETURNING
        value(rv_result) TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_fizzbuzz_foundation_level IMPLEMENTATION.


  METHOD CHECK_INPUT.
    IF is_numbar_equal_zero( iv_number ) = abap_true
    OR is_number_lower_zero( iv_number ) = abap_true.
      rv_result = mc_error.
    ENDIF.
  ENDMETHOD.


  METHOD GET_FIZZBUZZ. " Integrations -> Nur Methodenaufrufe

    rv_result = check_input( iv_number ).
    CHECK rv_result IS INITIAL.

    rv_result = set_result_fizzbuzz( iv_number ).
    CHECK rv_result IS INITIAL.

    rv_result = set_result_fizz( iv_number ).
    CHECK rv_result IS INITIAL.

    rv_result = set_result_buzz( iv_number ).
    CHECK rv_result IS INITIAL.

    rv_result = set_result_as_number( iv_number ).

  ENDMETHOD.


  METHOD IS_DIVIDABLE_BY_15.
    IF iv_number MOD 15 = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD IS_DIVIDABLE_BY_3.
    IF iv_number MOD 3 = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD IS_DIVIDABLE_BY_5.
    IF iv_number MOD 5 = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD IS_NUMBAR_EQUAL_ZERO.
    IF iv_number = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD IS_NUMBER_LOWER_ZERO.
    IF iv_number < 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD SET_RESULT_AS_NUMBER.
    rv_result = iv_number.
  ENDMETHOD.


  METHOD SET_RESULT_BUZZ.
    IF is_dividable_by_5( iv_number ) = abap_true.
      rv_result = mc_buzz.
    ENDIF.
  ENDMETHOD.


  METHOD SET_RESULT_FIZZ.
    IF is_dividable_by_3( iv_number ) = abap_true.
      rv_result = mc_fizz.
    ENDIF.
  ENDMETHOD.


  METHOD SET_RESULT_FIZZBUZZ.
    IF is_dividable_by_15( iv_number ) = abap_true.
      rv_result = mc_fizzbuzz.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
