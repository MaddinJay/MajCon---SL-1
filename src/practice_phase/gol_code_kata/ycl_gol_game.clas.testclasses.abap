CLASS ltcl_gol_game DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_gol_game.

    METHODS:
      " GIVEN: Start of GOL WHEN: Creating Grid and playing one round THEN: ...
      no_dump_occurs FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_gol_game IMPLEMENTATION.

  METHOD no_dump_occurs.
    cut = NEW ycl_gol_game( ).

    cut->create_grid( 3 ).
    cut->play_round( ).
  ENDMETHOD.

ENDCLASS.
