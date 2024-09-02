CLASS ycl_gol_start_game DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: yif_gol_start_game.

ENDCLASS.

CLASS ycl_gol_start_game IMPLEMENTATION.

  METHOD yif_gol_start_game~create.
    result = NEW ycl_gol_game( ).
    result->create_grid( size ).
  ENDMETHOD.

ENDCLASS.
