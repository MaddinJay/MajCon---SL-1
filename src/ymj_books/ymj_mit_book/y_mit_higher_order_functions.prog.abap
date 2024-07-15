*&---------------------------------------------------------------------*
*& Report y_mit_higher_order_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_higher_order_functions.


CLASS ltcl_higher_order_functions DEFINITION.

  PUBLIC SECTION.
    METHODS sum_integers IMPORTING a             TYPE int4
                                   b             TYPE int4
                         RETURNING VALUE(result) TYPE int4.

    METHODS sum_integers_sigma_not IMPORTING a             TYPE int4
                                             b             TYPE int4
                                   RETURNING VALUE(result) TYPE int4.
    METHODS sum_cubes    IMPORTING a             TYPE int4
                                   b             TYPE int4
                         RETURNING VALUE(result) TYPE int4.
    METHODS sum_cubes_sigma_not    IMPORTING a             TYPE int4
                                             b             TYPE int4
                                   RETURNING VALUE(result) TYPE int4.
    METHODS pi_sum       IMPORTING a             TYPE float
                                   b             TYPE float
                         RETURNING VALUE(result) TYPE float.
    METHODS pi_sum_sigma_not       IMPORTING a             TYPE int4
                                             b             TYPE int4
                                   RETURNING VALUE(result) TYPE float.
  PRIVATE SECTION.
    METHODS cube                   IMPORTING x             TYPE int4
                                   RETURNING VALUE(result) TYPE int4.
    METHODS sum_cube                     IMPORTING term          TYPE int4
                                                   a             TYPE int4
                                                   next          TYPE int4
                                                   b             TYPE int4
                                         RETURNING VALUE(result) TYPE int4.
    METHODS sum_integer             IMPORTING term          TYPE int4
                                              a             TYPE int4
                                              next          TYPE int4
                                              b             TYPE int4
                                    RETURNING VALUE(result) TYPE int4.
    METHODS inc                     IMPORTING a             TYPE int4
                                    RETURNING VALUE(result) TYPE int4.
    METHODS next                    IMPORTING a             TYPE int4
                                    RETURNING VALUE(result) TYPE int4.

    METHODS identity                IMPORTING a             TYPE int4
                                    RETURNING VALUE(result) TYPE int4.
    METHODS sum_pi                  IMPORTING term            TYPE float
                                              a               TYPE int4
                                              next            TYPE float
                                              b               TYPE int4
                                    RETURNING VALUE(r_result) TYPE float.
    METHODS pi_term                 IMPORTING a             TYPE int4
                                    RETURNING VALUE(result) TYPE float.
    METHODS pi_next                 IMPORTING a             TYPE int4
                                    RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS ltcl_higher_order_functions IMPLEMENTATION.

  METHOD sum_integers.
    result = COND #( WHEN a > b THEN 0
                     ELSE a + sum_integers( a = a + 1
                                            b = b ) ).
  ENDMETHOD.


  METHOD sum_cubes.
    result = COND #( WHEN a > b THEN 0
                     ELSE cube( a ) + sum_cubes(
                                            a = a + 1
                                            b = b
                                          ) ).
  ENDMETHOD.


  METHOD cube.
    result = x * x * x.
  ENDMETHOD.

  METHOD pi_sum.
    result = COND #( WHEN a > b THEN 0
                     ELSE ( 1 / ( a * (  a + 2 ) ) ) + pi_sum( a = a + 4 b = b ) ).
  ENDMETHOD.

  METHOD sum_cubes_sigma_not.
    result = sum_cube( term = cube( a ) a = a next = inc( a ) b = b ).
  ENDMETHOD.

  METHOD sum_cube.
    result = COND #( WHEN a > b THEN 0
                     ELSE cube( a ) + sum_cube( term = term a = next( a ) next = next b = b   ) ). " term( a ) = cube( a ), next( a ) = inc( a )
  ENDMETHOD.

  METHOD inc.
    result = a + 1.
  ENDMETHOD.

  METHOD next.
    result = inc( a ).
  ENDMETHOD.

  METHOD sum_integers_sigma_not.
    result = sum_integer( term = identity( a ) a = a next = inc( a ) b = b ).
  ENDMETHOD.

  METHOD identity.
    result = a.
  ENDMETHOD.

  METHOD sum_integer.
    result = COND #( WHEN a > b THEN 0
                     ELSE a + sum_integer( term = term a = next( a ) next = next b = b   ) ). " term( a ) = cube( a ), next( a ) = inc( a )
  ENDMETHOD.

  METHOD pi_sum_sigma_not.
    result = sum_pi( term = pi_term( a ) a = a next = pi_next( a ) b = b  ).
  ENDMETHOD.


  METHOD sum_pi.
    r_result = COND #( WHEN a > b THEN 0
                       ELSE pi_term( a ) + sum_pi( term = term a = next( a ) next = next b = b ) ). " term( a ) = cube( a ), next( a ) = inc( a )
  ENDMETHOD.

  METHOD pi_term.
    result = 1 / ( a * ( a + 2 ) ).
  ENDMETHOD.

  METHOD pi_next.
    result = a + 4.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_higher_orderfunctions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ltcl_higher_order_functions.

    METHODS:
      setup,
      test_sum_integers FOR TESTING,
      test_sum_cubes    FOR TESTING,
      test_pi_sum       FOR TESTING,
      test_sum_cubes_sigma_not FOR TESTING RAISING cx_static_check,
      test_sum_integers_sigma_not FOR TESTING RAISING cx_static_check,
      test_pi_sum_sigma_not FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_higher_orderfunctions IMPLEMENTATION.

  METHOD setup.
    cut = NEW ltcl_higher_order_functions( ).
  ENDMETHOD.

  METHOD test_sum_integers.
    cl_abap_unit_assert=>assert_equals( exp = 55
                                        act = cut->sum_integers(
                                                a = 1
                                                b = 10
                                              ) ).
  ENDMETHOD.

  METHOD test_sum_integers_sigma_not.
    cl_abap_unit_assert=>assert_equals( exp = 55
                                        act = cut->sum_integers_sigma_not(
                                                a = 1
                                                b = 10
                                              ) ).
  ENDMETHOD.

  METHOD test_sum_cubes.
    cl_abap_unit_assert=>assert_equals( exp = 3025
                                        act = cut->sum_cubes( a = 1
                                                              b = 10 ) ).
  ENDMETHOD.

  METHOD test_sum_cubes_sigma_not.
    cl_abap_unit_assert=>assert_equals( exp = 3025
                                        act = cut->sum_cubes_sigma_not( a = 1
                                                                        b = 10 ) ).
  ENDMETHOD.

  METHOD test_pi_sum.
    cl_abap_unit_assert=>assert_equals( exp = '3.333333333333333E-01'
                                        act = cut->pi_sum( a = 1 b = 2  ) ).
  ENDMETHOD.

  METHOD test_pi_sum_sigma_not.
    cl_abap_unit_assert=>assert_equals( exp = '3.333333333333333E-01'
                                        act = cut->pi_sum_sigma_not( a = 1 b = 2  ) ).
  ENDMETHOD.
ENDCLASS.
