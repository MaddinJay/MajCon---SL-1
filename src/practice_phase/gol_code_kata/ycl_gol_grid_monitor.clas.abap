CLASS ycl_gol_grid_monitor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: yif_gol_grid_monitor.

    METHODS constructor IMPORTING handler TYPE REF TO yif_gol_game.

  PRIVATE SECTION.
    DATA salv_table TYPE REF TO cl_salv_table.
    DATA grid_alv TYPE yif_gol_grid_monitor=>tt_grid_alv.

    DATA alv_container TYPE REF TO cl_gui_custom_container.
    DATA handler       TYPE REF TO yif_gol_game.

    METHODS create_grid_alv4display IMPORTING grid          TYPE yif_gol_grid=>tt_grid
                                    RETURNING VALUE(result) TYPE yif_gol_grid_monitor=>tt_grid_alv.

ENDCLASS.

CLASS ycl_gol_grid_monitor IMPLEMENTATION.

  METHOD constructor.
    me->handler = handler.
  ENDMETHOD.

  METHOD yif_gol_grid_monitor~display.
    ##TODO " Sauberer programmieren
    grid_alv = create_grid_alv4display( grid ).

    TRY.
        IF alv_container IS NOT BOUND.
          alv_container = NEW #( container_name = 'CONT_MAIN'
                                 repid          = 'Y_GAME_OF_LIFE'
                                 dynnr          = '0100' ).

          cl_salv_table=>factory( EXPORTING container_name = 'CONT_MAIN'
                                            r_container    = alv_container

                                  IMPORTING r_salv_table = salv_table
                                  CHANGING  t_table      = grid_alv ).

          DATA(functions) = salv_table->get_functions( ).
          functions->set_all( abap_true ).

          TRY.
              functions->add_function(
                name     = 'PLAY'
                icon     = 'ICON_EXECUTE_OBJECT'
                text     = 'Play one round'
                tooltip  = 'Play one round'
                position = if_salv_c_function_position=>right_of_salv_functions ).
            CATCH cx_salv_wrong_call cx_salv_existing.
          ENDTRY.

          DATA(lo_events) = salv_table->get_event( ).
          SET HANDLER handler->play_round_event FOR lo_events.

          salv_table->display( ).                            " display the ALV grid
        ELSE.
          salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
          cl_gui_cfw=>flush( ).
        ENDIF.
      CATCH cx_salv_msg.
        ##TODO " Raise exception
    ENDTRY.

  ENDMETHOD.

  METHOD create_grid_alv4display.
    ##TODO " Dynamische Gridgrösse beachten
    DATA(actual_row) = 0.
    LOOP AT grid INTO DATA(cell).
      IF cell-coordinates-y <> actual_row.
        APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<row>).
        actual_row = cell-coordinates-y.
      ENDIF.
      CASE cell-coordinates-x.
        WHEN 1.
          <row>-x_1 = cell-cell->is_alive( ).
        WHEN 2.
          <row>-x_2 = cell-cell->is_alive( ).
        WHEN 3.
          <row>-x_3 = cell-cell->is_alive( ).
        WHEN 4.
          <row>-x_4 = cell-cell->is_alive( ).
        WHEN 5.
          <row>-x_5 = cell-cell->is_alive( ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
