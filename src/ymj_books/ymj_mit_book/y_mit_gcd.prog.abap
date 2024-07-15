*&---------------------------------------------------------------------*
*& Report y_mit_gcd
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_gcd.


CLASS lcl_gcd DEFINITION.
  PUBLIC SECTION.
    METHODS gcd IMPORTING a             TYPE int4 " order of growth log n
                          b             TYPE int4
                RETURNING VALUE(result) TYPE int4.


ENDCLASS.

CLASS lcl_gcd IMPLEMENTATION.

  METHOD gcd.
    result = COND #( WHEN b = 0 THEN a
                     ELSE gcd( a = b b = a MOD b ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_gcd DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_gcd.
    METHODS:
      setup,
      test_gcd_algorithm  FOR TESTING.
ENDCLASS.


CLASS ltcl_gcd IMPLEMENTATION.

  METHOD setup.
    cut = NEW lcl_gcd( ).
  ENDMETHOD.

  METHOD test_gcd_algorithm.
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = cut->gcd( a = 206
                                                        b = 40 ) ).
  ENDMETHOD.

ENDCLASS.
