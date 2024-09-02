*&---------------------------------------------------------------------*
*& Report y_game_of_life
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_game_of_life.

DATA gv_ok_code TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK one.
PARAMETERS: p_size TYPE int1.
SELECTION-SCREEN END OF BLOCK one.

START-OF-SELECTION.

  DATA(game) = NEW ycl_gol_start_game( )->yif_gol_start_game~create( p_size ).
  CALL SCREEN 100.

  INCLUDE y_game_of_life_pbo.

INCLUDE y_game_of_life_pai.
