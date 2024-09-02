CLASS ycl_gol_create_neigbourhood DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_create_neighbourhood.

ENDCLASS.

CLASS ycl_gol_create_neigbourhood IMPLEMENTATION.

  METHOD yif_gol_create_neighbourhood~create.
    result = NEW ycl_gol_neighbourhood( ).
    result->create( grid             = grid
                    cell_coordinates = VALUE #( x = cell_coordinates-x y = cell_coordinates-y ) ).
  ENDMETHOD.

ENDCLASS.
