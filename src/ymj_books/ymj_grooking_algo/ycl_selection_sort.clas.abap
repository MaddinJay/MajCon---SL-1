CLASS ycl_selection_sort DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS selection_sort IMPORTING table         TYPE int4_table
                           RETURNING VALUE(result) TYPE int4_table.

  PRIVATE SECTION.
    METHODS find_smallest IMPORTING table         TYPE int4_table
                          RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS ycl_selection_sort IMPLEMENTATION.

  METHOD selection_sort.
    DATA(new_table) = VALUE int4_table( ).
    DATA(inp_table) = table.

    DO lines( table ) TIMES.
      DATA(smallest_index) = find_smallest( inp_table ).
      APPEND inp_table[ smallest_index ] TO new_table.
      DELETE inp_table INDEX smallest_index.
    ENDDO.

    result = new_table.
  ENDMETHOD.

  METHOD find_smallest.
    DATA(smallest_value) = table[ 1 ].
    DATA(smallest_index) = 1.
    DO lines( table ) TIMES.
      IF table[ sy-index ] < smallest_value.
        smallest_value = table[ sy-index ].
        smallest_index = sy-index.
      ENDIF.
    ENDDO.
    result = smallest_index.
  ENDMETHOD.

ENDCLASS.
