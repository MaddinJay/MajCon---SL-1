CLASS ltcl_neighbourhood DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_gol_neighbourhood.

    METHODS:
      " GIVEN: Neighbourhood with 8 living neighbours WHEN: Counting living neighbours THEN: ...
      should_have_8_living_neighbour FOR TESTING.

    METHODS:
      create_neighbourhood IMPORTING number_of_neighbours TYPE int1
                           RETURNING VALUE(result)        TYPE REF TO yif_gol_neighbourhood.
ENDCLASS.


CLASS ltcl_neighbourhood IMPLEMENTATION.

  METHOD should_have_8_living_neighbour.
    cl_abap_unit_assert=>assert_equals( exp = 8
                                        act = create_neighbourhood( number_of_neighbours = 3 )->count_living( ) ).
  ENDMETHOD.

  METHOD create_neighbourhood.
    DATA(grid_table) = VALUE yif_gol_grid=>tt_grid( ( coordinates = VALUE #( x = 1 y = 1 ) cell = NEW ycl_gol_cell( alive       = abap_false
                                                                                                                    coordinates = VALUE #( x = 1 y = 1 ) ) )
                                                    ( coordinates = VALUE #( x = 2 y = 1 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 2 y = 1 )  ) )
                                                    ( coordinates = VALUE #( x = 3 y = 1 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 3 y = 1 ) ) )
                                                    ( coordinates = VALUE #( x = 1 y = 2 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 1 y = 2 )  ) )
                                                    ( coordinates = VALUE #( x = 2 y = 2 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 2 y = 2 )  ) )
                                                    ( coordinates = VALUE #( x = 2 y = 2 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 3 y = 2 )  ) )
                                                    ( coordinates = VALUE #( x = 1 y = 3 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 1 y = 3 )  ) )
                                                    ( coordinates = VALUE #( x = 2 y = 3 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 2 y = 3 )  ) )
                                                    ( coordinates = VALUE #( x = 3 y = 3 ) cell = NEW ycl_gol_cell( alive       = abap_true
                                                                                                                    coordinates = VALUE #( x = 3 y = 3 )  ) ) ).

    result = NEW ycl_gol_neighbourhood( ).
    result->create( grid = grid_table
                    cell_coordinates = VALUE #( x    = 1
                                                y    = 1 ) ).
  ENDMETHOD.

ENDCLASS.
