CLASS ycl_quick_sort DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS quicksort IMPORTING number_list   TYPE int4_table
                      RETURNING VALUE(result) TYPE int4_table.

  PRIVATE SECTION.
    METHODS process_recursive_case IMPORTING number_list   TYPE int4_table
                                   RETURNING VALUE(result) TYPE int4_table.
    METHODS less_sub_array         IMPORTING number_list   TYPE int4_table
                                   RETURNING VALUE(result) TYPE int4_table.
    METHODS greater_sub_array      IMPORTING number_list   TYPE int4_table
                                   RETURNING VALUE(result) TYPE int4_table.

ENDCLASS.

CLASS ycl_quick_sort IMPLEMENTATION.

  METHOD quicksort.
    result = COND #( WHEN lines( number_list ) < 2 THEN number_list " base case
                     ELSE process_recursive_case( number_list ) ).
  ENDMETHOD.

  METHOD process_recursive_case.
    APPEND LINES OF quicksort( less_sub_array( number_list ) ) TO result.
    APPEND number_list[ 1 ] TO result.
    APPEND LINES OF quicksort( greater_sub_array( number_list ) ) TO result.
  ENDMETHOD.

  METHOD greater_sub_array.
    LOOP AT number_list INTO DATA(line) FROM 2.
      IF line > number_list[ 1 ].
        APPEND line TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD less_sub_array.
    LOOP AT number_list INTO DATA(line) FROM 2.
      IF line <= number_list[ 1 ].
        APPEND line TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
