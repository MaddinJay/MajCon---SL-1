CLASS ycl_gol_game DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_game.

    METHODS constructor.
  PRIVATE SECTION.
    DATA grid TYPE REF TO yif_gol_grid.
    DATA grid_monitor TYPE REF TO yif_gol_grid_monitor.

ENDCLASS.

CLASS ycl_gol_game IMPLEMENTATION.

  METHOD constructor.
    grid_monitor = NEW ycl_gol_grid_monitor( me ).
  ENDMETHOD.

  METHOD yif_gol_game~create_grid.
    grid = NEW ycl_gol_grid( ).
    grid->create( size ).
  ENDMETHOD.

  METHOD yif_gol_game~play_round.
    LOOP AT grid->get_grid( ) INTO DATA(grid_cell).
      grid_cell-cell->process( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD yif_gol_game~play_round_event.
    yif_gol_game~play_round( ).
    yif_gol_game~show_grid( ).
  ENDMETHOD.

  METHOD yif_gol_game~show_grid.
    grid_monitor->display( me->grid->get_grid( ) ).
  ENDMETHOD.

ENDCLASS.
