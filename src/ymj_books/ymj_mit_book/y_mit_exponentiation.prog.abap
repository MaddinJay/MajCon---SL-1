*&---------------------------------------------------------------------*
*& Report y_mit_exponentiation
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_exponentiation.

CLASS lcl_exponentiation DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS expt IMPORTING b             TYPE int1 " linear recursive process (linear steps, linear space)
                           n             TYPE int1
                 RETURNING VALUE(result) TYPE int4.

    METHODS expt_linear IMPORTING b             TYPE int1 " linear iterative process (linear steps, constant space)
                                  n             TYPE int1
                        RETURNING VALUE(result) TYPE int4.

    METHODS fast_expt IMPORTING b             TYPE int1 " fast recursive process (logarithmic growth)
                                n             TYPE int1
                      RETURNING VALUE(result) TYPE int4.
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS expt_iter IMPORTING b             TYPE int1
                                counter       TYPE int1
                                product       TYPE int4
                      RETURNING VALUE(result) TYPE int4.
    METHODS is_even IMPORTING n               TYPE int1
                    RETURNING VALUE(r_result) TYPE abap_bool.
    METHODS square IMPORTING b               TYPE int1
                             n               TYPE int1
                   RETURNING VALUE(r_result) TYPE i.

ENDCLASS.

CLASS lcl_exponentiation IMPLEMENTATION.

  METHOD expt.
    result = COND #( WHEN n = 0 THEN 1
                    ELSE b * expt( b = b
                                   n = n - 1 ) ).
  ENDMETHOD.

  METHOD expt_linear.
    result = expt_iter( b       = b
                        counter = n
                        product = 1 ).
  ENDMETHOD.

  METHOD expt_iter.
    result = COND #( WHEN counter = 0 THEN product
                     ELSE expt_iter( b       = b
                                     counter = counter - 1
                                     product = product * b ) ).
  ENDMETHOD.

  METHOD fast_expt.
    result = COND #( WHEN n = 0 THEN 1
                    WHEN is_even( n ) THEN square( b = b n = n / 2 )
                    ELSE b * fast_expt( b = b
                                        n = n - 1 ) ).
  ENDMETHOD.

  METHOD square.
    r_result = fast_expt( b = b
                          n = n ) * fast_expt( b = b
                                               n = n ).
  ENDMETHOD.

  METHOD is_even .
    r_result = boolc( n MOD 2 = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_exponentiation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_exponentiation.
    METHODS:
      setup,
      exponent_2_2        FOR TESTING RAISING cx_static_check,
      exponent_4_2_linear FOR TESTING RAISING cx_static_check,
      exponent_4_2_fast FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_exponentiation IMPLEMENTATION.

  METHOD setup.
    cut = NEW lcl_exponentiation( ).
  ENDMETHOD.

  METHOD exponent_2_2.
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = cut->expt( b = 2
                                                         n = 2 ) ).
  ENDMETHOD.

  METHOD exponent_4_2_linear.
    cl_abap_unit_assert=>assert_equals( exp = 16
                                        act = cut->expt( b = 4
                                                         n = 2 ) ).
  ENDMETHOD.

  METHOD exponent_4_2_fast.
    cl_abap_unit_assert=>assert_equals( exp = 16
                                        act = cut->fast_expt( b = 4
                                                              n = 2 ) ).
  ENDMETHOD.

ENDCLASS.
