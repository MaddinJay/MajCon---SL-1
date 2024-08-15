*&---------------------------------------------------------------------*
*& Report y_mit_primality_log_n
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_primality_log_n.

CLASS lcl_primality DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS fast_is_prime IMPORTING n             TYPE int4           " Log n growth
                                    times         TYPE int4
                          RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS is_even IMPORTING x             TYPE int4
                    RETURNING VALUE(result) TYPE abap_bool.
    METHODS square  IMPORTING x             TYPE int4
                    RETURNING VALUE(result) TYPE int4.
    METHODS try_it  IMPORTING a             TYPE int4
                              n             TYPE int4
                    RETURNING VALUE(result) TYPE abap_bool.
    METHODS random_integer IMPORTING n             TYPE int4
                           RETURNING VALUE(result) TYPE int4.
    METHODS fermat_test IMPORTING n TYPE int4 RETURNING VALUE(result) TYPE abap_bool.
    METHODS expmod IMPORTING base          TYPE int4
                             exp           TYPE int4
                             m             TYPE int4
                   RETURNING VALUE(result) TYPE int4.

ENDCLASS.

CLASS lcl_primality IMPLEMENTATION.

  METHOD fermat_test.
    result = try_it( a = 1 + floor( random_integer( n ) * ( n - 1 ) )
                     n = n ).
  ENDMETHOD.

  METHOD expmod.
    result = COND #( WHEN exp = 0 THEN 1
                     WHEN is_even( exp ) THEN square( expmod( base = base exp = exp / 2 m = m ) ) MOD m
                     ELSE ( base * expmod(  base = base exp = exp - 1 m = m ) ) MOD m ).
  ENDMETHOD.

  METHOD is_even.
    result = xsdbool( x MOD 2 = 0 ).
  ENDMETHOD.

  METHOD square.
    result = x * x.
  ENDMETHOD.

  METHOD try_it.
    result = expmod( base = a exp = n m = n ).
  ENDMETHOD.

  METHOD random_integer.
    result = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                          min  = 1
                                          max = n - 1 )->get_next( ).
  ENDMETHOD.

  METHOD fast_is_prime.
    result = COND #( WHEN times = 0        THEN abap_true
                     WHEN fermat_test( n ) THEN fast_is_prime( n = n times = times - 1 )
                     ELSE abap_false ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_primality DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_primality.
    METHODS:
      setup,
      test_fast_is_prime FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_primality IMPLEMENTATION.

  METHOD setup.
    cut = NEW lcl_primality( ).
  ENDMETHOD.

  METHOD test_fast_is_prime.
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = cut->fast_is_prime( n = 5 times = 10 ) ).
  ENDMETHOD.

ENDCLASS.
