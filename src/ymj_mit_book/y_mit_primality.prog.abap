*&---------------------------------------------------------------------*
*& Report y_mit_primality
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_primality.

CLASS lcl_primality DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS is_prime IMPORTING n             TYPE int4          " root n growth
                     RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS find_divisor IMPORTING n             TYPE int4
                                   test_divisor  TYPE int4
                         RETURNING VALUE(result) TYPE int4.
    METHODS divides      IMPORTING n               TYPE int4
                                   test_divisor    TYPE int4
                         RETURNING VALUE(r_result) TYPE abap_bool.
    METHODS square       IMPORTING test_divisor    TYPE int4
                         RETURNING VALUE(r_result) TYPE i.
    METHODS smallest_divisor IMPORTING n             TYPE int4
                             RETURNING VALUE(result) TYPE int4.

ENDCLASS.

CLASS lcl_primality IMPLEMENTATION.

  METHOD is_prime.
    result = boolc( smallest_divisor( n ) = n ).
  ENDMETHOD.

  METHOD smallest_divisor.
    result = find_divisor( n            = n
                           test_divisor = 2 ).
  ENDMETHOD.

  METHOD find_divisor.
    result = COND #( WHEN square( test_divisor ) > n THEN n
                    WHEN divides( n = n test_divisor = test_divisor ) THEN test_divisor
                    ELSE find_divisor( n            = n
                                       test_divisor = test_divisor + 1 ) ).
  ENDMETHOD.

  METHOD square.
    r_result = test_divisor * test_divisor.
  ENDMETHOD.

  METHOD divides.
    r_result = boolc( n MOD test_divisor = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_primality DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_primality.
    METHODS:
      setup,
      test_is_prime FOR TESTING.
ENDCLASS.


CLASS ltcl_primality IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_is_prime.
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = cut->is_prime( n = 5 ) ).
  ENDMETHOD.

ENDCLASS.
