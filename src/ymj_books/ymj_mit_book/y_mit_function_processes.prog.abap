*&---------------------------------------------------------------------*
*& Report y_mit_function_processes
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_mit_function_processes.


CLASS lcl_processes DEFINITION.

  PUBLIC SECTION.
    METHODS factorial IMPORTING number        TYPE int4
                      RETURNING VALUE(result) TYPE int4.
    METHODS factorial_iter_process IMPORTING number        TYPE int4
                                   RETURNING VALUE(result) TYPE int4.
    METHODS fib       IMPORTING n             TYPE int4
                      RETURNING VALUE(result) TYPE int4.
    METHODS fib_iterative       IMPORTING n             TYPE int4
                                RETURNING VALUE(result) TYPE int4.

  PRIVATE SECTION.
    METHODS fact_iter IMPORTING product       TYPE int4
                                counter       TYPE int4
                                max_count     TYPE int4
                      RETURNING VALUE(result) TYPE int4.
    METHODS fib_iter  IMPORTING a             TYPE int4
                                b             TYPE int4
                                count         TYPE int4
                      RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS lcl_processes IMPLEMENTATION.

  METHOD factorial.
    result = COND #( WHEN number = 1 THEN 1 ELSE number * factorial( number - 1 ) ). " linear recursive process
  ENDMETHOD.

  METHOD factorial_iter_process.
    result = fact_iter( product   = 1
                        counter   = 1
                        max_count = number ). " linear iterative process
  ENDMETHOD.

  METHOD fact_iter.
    result = COND #( WHEN counter > max_count THEN product
                     ELSE fact_iter( product   = counter * product
                                     counter   = counter + 1
                                     max_count = max_count ) ).
  ENDMETHOD.

  METHOD fib.
    result = COND #( WHEN n = 0 THEN 0
                     WHEN n = 1 THEN 1
                     ELSE fib( n - 1 ) + fib( n - 2 ) ).
  ENDMETHOD.

  METHOD fib_iterative.
    result = fib_iter(  a     = 1
                        b     = 0
                        count = n ).
  ENDMETHOD.

  METHOD fib_iter.
    result = COND #( WHEN count = 0 THEN b
                     ELSE fib_iter( a = a + b b = a count = count - 1 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_processes DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_processes.

    METHODS:
      setup,
      recursive_process  FOR TESTING,
      iterative_process  FOR TESTING,
      tree_recursive_process FOR TESTING,
      tree_iterative_process FOR TESTING.

ENDCLASS.

CLASS ltcl_processes IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD recursive_process.
    cl_abap_unit_assert=>assert_equals( exp = 720
                                        act = cut->factorial( 6  ) ).
  ENDMETHOD.

  METHOD iterative_process.
    cl_abap_unit_assert=>assert_equals( exp = 720
                                        act = cut->factorial_iter_process( 6 ) ).
  ENDMETHOD.

  METHOD tree_recursive_process.
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = cut->fib( 5 ) ).
  ENDMETHOD.

  METHOD tree_iterative_process.
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = cut->fib_iterative( 5 ) ).
  ENDMETHOD.

ENDCLASS.
