CLASS ycl_knapsack_problem DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tv_value TYPE curr13_2.
    TYPES: BEGIN OF ts_item,
             name   TYPE string,
             weight TYPE i,
             value  TYPE curr13_2,
           END OF ts_item,
           tt_items TYPE STANDARD TABLE OF ts_item WITH KEY name,
           BEGIN OF ts_cell,
             column TYPE int4,
             row    TYPE int4,
             items  TYPE tt_items,
           END OF ts_cell,
           tt_cell TYPE HASHED TABLE OF ts_cell WITH UNIQUE KEY row column.

    METHODS constructor IMPORTING items      TYPE tt_items
                                  max_weight TYPE int1.

    METHODS process RETURNING VALUE(result) TYPE tt_items.

  PRIVATE SECTION.
    DATA grid  TYPE tt_cell.
    DATA items TYPE tt_items.
    DATA max_weight TYPE int1.
    METHODS process_knapsack_algo.
    METHODS determine_cell_items IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                 RETURNING VALUE(result) TYPE tt_items.
    METHODS previous_cell_value  IMPORTING cell          TYPE ts_cell
                                 RETURNING VALUE(result) TYPE tv_value.
    METHODS actual_cell_value    IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                 RETURNING VALUE(result) TYPE tv_value.
    METHODS get_previous_cell_items IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                    RETURNING VALUE(result) TYPE ycl_knapsack_problem=>tt_items.
    METHODS items_remaining_space   IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                    RETURNING VALUE(result) TYPE ycl_knapsack_problem=>tt_items.
    METHODS lines_actual_cell       IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                    RETURNING VALUE(result) TYPE tt_items.
    METHODS create_initial_grid.
    METHODS get_buttom_right_cell_items RETURNING VALUE(result) TYPE ycl_knapsack_problem=>tt_items.
    METHODS is_previous_max_best_choice IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                        RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_actual_cell_items        IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                         RETURNING VALUE(result) TYPE tt_items.
    METHODS does_item_fit_in_cell        IMPORTING cell          TYPE ycl_knapsack_problem=>ts_cell
                                         RETURNING
                                         VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS ycl_knapsack_problem IMPLEMENTATION.

  METHOD constructor.
    me->items      = items.
    me->max_weight = max_weight.
  ENDMETHOD.

  METHOD process.
    create_initial_grid( ).
    process_knapsack_algo( ).

    result = get_buttom_right_cell_items( ).
  ENDMETHOD.

  METHOD get_buttom_right_cell_items.
    result = me->grid[ row = lines( me->items )  column = max_weight ]-items.
  ENDMETHOD.

  METHOD process_knapsack_algo.
    LOOP AT me->grid ASSIGNING FIELD-SYMBOL(<cell>).
      <cell>-items = determine_cell_items( <cell> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_initial_grid.
    DATA(row_index) = 1.

    LOOP AT items INTO DATA(line).
      DATA(column_index) = 1.

      DO max_weight TIMES.
        INSERT VALUE ts_cell( row = row_index column = column_index ) INTO TABLE grid.
        column_index  = column_index + 1.
      ENDDO.

      row_index = row_index + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_cell_items.
    result = COND #( WHEN is_previous_max_best_choice( cell ) THEN get_previous_cell_items( cell )
                     ELSE get_actual_cell_items( cell ) ).
  ENDMETHOD.

  METHOD previous_cell_value.
    TRY.
        DATA(previous_items) = me->grid[ row = cell-row - 1 column = cell-column ]-items.

        result = REDUCE #( INIT value TYPE tv_value
                           FOR <line> IN previous_items
                           NEXT value = value + <line>-value ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD actual_cell_value.

    IF does_item_fit_in_cell( cell ).
      result = me->items[ cell-row ]-value.
    ENDIF.

    TRY.
        DATA(column_remaining_space) = cell-column - me->items[ cell-row ]-weight.
        DATA(items_remaining_space)  = me->grid[ row = cell-row column = column_remaining_space ]-items.
        DATA(value_remaining_sapce) = REDUCE #( INIT value TYPE tv_value
                                                FOR <line> IN items_remaining_space
                                                NEXT value = value + <line>-value ).
        result = result + value_remaining_sapce.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD does_item_fit_in_cell.
    result = xsdbool( me->items[ cell-row ]-weight <= cell-column ).
  ENDMETHOD.

  METHOD get_previous_cell_items.
    result = me->grid[ row = cell-row - 1 column = cell-column ]-items.
  ENDMETHOD.

  METHOD items_remaining_space.
    TRY.
        DATA(column_remaining_space) = cell-column - me->items[ cell-row ]-weight.
        result  = me->grid[ row = cell-row - 1 column = column_remaining_space ]-items.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD lines_actual_cell.
    IF me->items[ cell-row ]-weight <= cell-column.
      APPEND me->items[ cell-row ] TO result.
    ENDIF.
  ENDMETHOD.

  METHOD is_previous_max_best_choice.
    result = xsdbool( previous_cell_value( cell ) > actual_cell_value( cell ) ).
  ENDMETHOD.

  METHOD get_actual_cell_items.
    APPEND LINES OF lines_actual_cell( cell ) TO result.
    APPEND LINES OF items_remaining_space( cell ) TO result.
  ENDMETHOD.

ENDCLASS.
