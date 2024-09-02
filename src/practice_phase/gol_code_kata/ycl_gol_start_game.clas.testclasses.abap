CLASS ltcl_start_game DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      " GIVEN: Start of game WHEN: Initializing Game THEN: ...
      should_create_game_with_grid FOR TESTING.

    METHODS create_game RETURNING VALUE(result) TYPE REF TO yif_gol_game.
ENDCLASS.

CLASS ltcl_start_game IMPLEMENTATION.

  METHOD should_create_game_with_grid.
    cl_abap_unit_assert=>assert_true( xsdbool( create_game( ) IS INSTANCE OF ycl_gol_game ) ).
  ENDMETHOD.

  METHOD create_game.
    result  = NEW ycl_gol_start_game( )->yif_gol_start_game~create( 3 ).
  ENDMETHOD.

ENDCLASS.
