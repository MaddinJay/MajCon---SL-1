CLASS ycl_gol_grid DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_grid.

  PRIVATE SECTION.
    DATA grid TYPE yif_gol_grid=>tt_grid.
    DATA grid_monitor TYPE REF TO yif_gol_grid_monitor.

    METHODS create_row_entries              IMPORTING size TYPE int1
                                                      y    TYPE i.
    METHODS create_random_bool              RETURNING VALUE(result) TYPE abap_bool.
    METHODS create_initial_grid             IMPORTING size TYPE int1.
    METHODS add_neighbours_in_grid          IMPORTING size TYPE int1.
    METHODS add_neighbours2cell             IMPORTING coordinates TYPE yif_gol_grid=>ts_coordinates.
    METHODS create_cell_in_grid             IMPORTING coordinates TYPE yif_gol_grid=>ts_coordinates.
    METHODS create_neigbours4row_entries    IMPORTING size TYPE int1
                                                      y    TYPE i.

ENDCLASS.

CLASS ycl_gol_grid IMPLEMENTATION.

  METHOD add_neighbours2cell.
    DATA(neighbourhood) = NEW ycl_gol_create_neigbourhood( )->yif_gol_create_neighbourhood~create( grid             = grid
                                                                                                   cell_coordinates = coordinates ).

    grid[ coordinates = coordinates ]-cell->set_neighbours( neighbourhood ).
  ENDMETHOD.

  METHOD add_neighbours_in_grid.
    DATA(y) = 1.   " Row
    DO size TIMES. " #rows
      create_neigbours4row_entries( size = size
                                    y    = y ).
      y = y + 1.
    ENDDO.
  ENDMETHOD.

  METHOD create_cell_in_grid.
    APPEND VALUE yif_gol_grid=>ts_grid( coordinates = coordinates
                                        cell        = NEW ycl_gol_cell( alive       = create_random_bool( )
                                                                        coordinates = coordinates ) ) TO grid.
  ENDMETHOD.

  METHOD create_initial_grid.
    DATA(y) = 1. " Row
    DO size TIMES. " Number of rows
      create_row_entries( size = size
                          y    = y ).
      y = y + 1.   " Next row
    ENDDO.
  ENDMETHOD.

  METHOD create_neigbours4row_entries.
    DATA(x) = 1. " Column
    DO size TIMES. " #columns
      add_neighbours2cell( coordinates = VALUE #( x = x y = y ) ).
      x = x + 1.
    ENDDO.
  ENDMETHOD.

  METHOD create_random_bool.
    " Create random integer and check if it is even -> ABAP_TRUE, else ABAP_FALSE
    result = xsdbool( cl_abap_random=>create( )->int( ) MOD 2 = 0 ). ##TODO " Not working correctly
  ENDMETHOD.

  METHOD create_row_entries.
    DATA(x) = 1. " Column
    DO size TIMES. " Number of columns = Number of rows
      create_cell_in_grid( coordinates = VALUE #( y = y x = x ) ).
      x = x + 1.
    ENDDO.
  ENDMETHOD.

  METHOD yif_gol_grid~create.
    create_initial_grid( size ).
    add_neighbours_in_grid( size ).
  ENDMETHOD.

  METHOD yif_gol_grid~display.
    IF grid_monitor IS NOT BOUND.
      grid_monitor = NEW ycl_gol_grid_monitor( handler ).
    ENDIF.
    grid_monitor->display( me->grid ).
  ENDMETHOD.

  METHOD yif_gol_grid~get_grid.
    result = me->grid.
  ENDMETHOD.

ENDCLASS.
