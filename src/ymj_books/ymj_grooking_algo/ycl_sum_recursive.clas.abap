CLASS ycl_sum_recursive DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS sum_up IMPORTING number_list   TYPE int4_table
                   RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS ycl_sum_recursive IMPLEMENTATION.

  METHOD sum_up.
    result = COND #( WHEN lines( number_list ) = 0 THEN 0
                     ELSE number_list[ 1 ] + sum_up( VALUE int4_table( FOR <line> IN number_list FROM 2:
                                                                         ( <line> ) ) ) ).
  ENDMETHOD.

ENDCLASS.
