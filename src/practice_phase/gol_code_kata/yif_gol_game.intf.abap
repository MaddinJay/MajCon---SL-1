INTERFACE yif_gol_game
  PUBLIC .

  METHODS create_grid IMPORTING size TYPE int1.
  METHODS play_round.
  METHODS show_grid.

  METHODS play_round_event FOR EVENT added_function OF cl_salv_events.

ENDINTERFACE.
