CLASS ycl_count_number_of_items DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS count IMPORTING number_list   TYPE int4_table
                  RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS ycl_count_number_of_items IMPLEMENTATION.

  METHOD count.
    result = COND #( WHEN lines( number_list ) = 0 THEN 0
                     ELSE 1 + count( VALUE int4_table( FOR <line> IN number_list FROM 2:
                                                         ( <line> ) ) ) ).
  ENDMETHOD.

ENDCLASS.
