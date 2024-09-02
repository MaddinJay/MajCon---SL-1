CLASS ycl_gol_neighbourhood DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_neighbourhood.

  PRIVATE SECTION.
    DATA neighbours TYPE yif_gol_neighbourhood=>tt_neighbours.
    DATA grid_size TYPE int1.

    METHODS upper_neighbour      IMPORTING x             TYPE int1
                                 RETURNING VALUE(result) TYPE i.
    METHODS downstairs_neighbour IMPORTING x             TYPE int1
                                 RETURNING VALUE(result) TYPE i.
    METHODS neighbour_left       IMPORTING y             TYPE int1
                                 RETURNING VALUE(result) TYPE i.
    METHODS neighbour_right      IMPORTING y             TYPE int1
                                 RETURNING VALUE(result) TYPE int1.

ENDCLASS.

CLASS ycl_gol_neighbourhood IMPLEMENTATION.



  METHOD yif_gol_neighbourhood~create.
    me->grid_size = sqrt( lines( grid ) ).
    neighbours = VALUE #( ( grid[ coordinates = VALUE #( x = upper_neighbour( cell_coordinates-x )      y = neighbour_left( cell_coordinates-y ) ) ]-cell )
                          ( grid[ coordinates = VALUE #( x = cell_coordinates-x                         y = neighbour_left( cell_coordinates-y ) ) ]-cell )
                          ( grid[ coordinates = VALUE #( x = downstairs_neighbour( cell_coordinates-x ) y = neighbour_left( cell_coordinates-y ) ) ]-cell )

                          ( grid[ coordinates = VALUE #( x = upper_neighbour( cell_coordinates-x )      y = cell_coordinates-y ) ]-cell )
                          ( grid[ coordinates = VALUE #( x = downstairs_neighbour( cell_coordinates-x ) y = cell_coordinates-y ) ]-cell )

                          ( grid[ coordinates = VALUE #( x = upper_neighbour( cell_coordinates-x )      y = neighbour_right( cell_coordinates-y ) ) ]-cell )
                          ( grid[ coordinates = VALUE #( x = cell_coordinates-x                         y = neighbour_right( cell_coordinates-y ) ) ]-cell )
                          ( grid[ coordinates = VALUE #( x = downstairs_neighbour( cell_coordinates-x ) y = neighbour_right( cell_coordinates-y ) ) ]-cell ) ).
  ENDMETHOD.

  METHOD yif_gol_neighbourhood~count_living.
    result = REDUCE #( INIT x = 0
                       FOR <line> IN neighbours
                       NEXT x = x + COND #( WHEN <line>->is_alive( ) THEN 1 ) ).
  ENDMETHOD.

  METHOD neighbour_left.
    result = COND #( WHEN y - 1 = 0 THEN grid_size
                     ELSE y - 1 ).
  ENDMETHOD.

  METHOD downstairs_neighbour.
    result = COND #( WHEN x + 1 > grid_size THEN 1
                     ELSE x + 1 ).
  ENDMETHOD.

  METHOD upper_neighbour.
    result = COND #( WHEN x - 1 = 0 THEN grid_size
                     ELSE x - 1 ) .
  ENDMETHOD.

  METHOD yif_gol_neighbourhood~get_neighbours.
    result = neighbours.
  ENDMETHOD.

  METHOD neighbour_right.
    result = COND #( WHEN y + 1 > grid_size THEN 1
                     ELSE grid_size ).
  ENDMETHOD.

ENDCLASS.
