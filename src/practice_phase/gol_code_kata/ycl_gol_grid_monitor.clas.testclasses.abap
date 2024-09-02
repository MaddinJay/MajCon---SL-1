CLASS ltcl_grid_monitor DEFINITION DEFERRED.
CLASS ycl_gol_grid_monitor DEFINITION LOCAL FRIENDS ltcl_grid_monitor.

CLASS ltcl_grid_monitor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_gol_grid_monitor.

    METHODS:
      should_create_alv_grid_correct FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_grid_monitor IMPLEMENTATION.

  METHOD should_create_alv_grid_correct.
    cut = NEW #( NEW ycl_gol_game( ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_gol_grid_monitor=>tt_grid_alv( ( x_1 = 'X' x_3 = 'X' ) )
                                        act = cut->create_grid_alv4display( grid = VALUE #( ( coordinates = VALUE #( x = 1 y = 1 ) cell = NEW ycl_gol_cell( alive       = 'X'
                                                                                                                                                            coordinates = VALUE #( x = 1 y = 1 ) ) )
                                                                                            ( coordinates = VALUE #( x = 2 y = 1 ) cell = NEW ycl_gol_cell( alive       = ''
                                                                                                                                                            coordinates = VALUE #( x = 2 y = 1 ) ) )
                                                                                            ( coordinates = VALUE #( x = 3 y = 1 ) cell = NEW ycl_gol_cell( alive       = 'X'
                                                                                                                                                            coordinates = VALUE #( x = 3 y = 1 ) ) )
                                      ) ) ).
  ENDMETHOD.

ENDCLASS.
