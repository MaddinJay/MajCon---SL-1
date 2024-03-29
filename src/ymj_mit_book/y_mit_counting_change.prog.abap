*&---------------------------------------------------------------------*
*& Report y_mit_counting_change
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_counting_change.

CLASS lcl_counting_change DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS count_change
      IMPORTING
        amount        TYPE int4
      RETURNING
        VALUE(result) TYPE int4.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS cc IMPORTING amount         TYPE int4
                         kinds_of_coins TYPE int2
               RETURNING VALUE(result)  TYPE int4.
    METHODS first_demonination IMPORTING kinds_of_coins TYPE int2
                               RETURNING VALUE(result)  TYPE int4.

ENDCLASS.

CLASS lcl_counting_change IMPLEMENTATION.

  METHOD count_change.
    result = cc( amount         = amount
                 kinds_of_coins = 5 ).
  ENDMETHOD.

  METHOD cc.
    result = COND #( WHEN amount = 0 THEN 1
                     WHEN ( amount < 0 OR kinds_of_coins = 0 ) THEN 0
                     ELSE cc( amount         = amount
                              kinds_of_coins = kinds_of_coins - 1 )
                              + cc( amount         = amount - first_demonination( kinds_of_coins )
                                    kinds_of_coins = kinds_of_coins ) ).
  ENDMETHOD.


  METHOD first_demonination.
    result = COND #( WHEN kinds_of_coins = 1 THEN 1
                     WHEN kinds_of_coins = 2 THEN 5
                     WHEN kinds_of_coins = 3 THEN 10
                     WHEN kinds_of_coins = 4 THEN 25
                     WHEN kinds_of_coins = 5 THEN 50
                     ELSE 0 ).
  ENDMETHOD.

ENDCLASS.



CLASS ltcl_counting_change DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_counting_change.

    METHODS:
      calculate_correct_for_100 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_counting_change IMPLEMENTATION.

  METHOD calculate_correct_for_100.
    cut = NEW lcl_counting_change( ).
    cl_abap_unit_assert=>assert_equals( exp = 292
                                        act = cut->count_change( 100 ) ).
  ENDMETHOD.

ENDCLASS.
