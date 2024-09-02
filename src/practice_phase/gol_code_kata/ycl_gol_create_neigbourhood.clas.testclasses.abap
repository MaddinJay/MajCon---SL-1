CLASS ltcl_gol_create_neighbourhood DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_gol_neighbourhood.

    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_gol_create_neighbourhood IMPLEMENTATION.

  METHOD first_test.
    cut = NEW ycl_gol_create_neigbourhood( )->yif_gol_create_neighbourhood~create(
                   grid             = VALUE #( ( coordinates = VALUE #( x = 1 y = 1 ) cell = NEW ycl_gol_cell( alive         = abap_false
                                                                                                               coordinates   = VALUE #( x = 1 y = 1 ) ) ) )
                   cell_coordinates = VALUE #( x = 1 y = 1 ) ).
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.

ENDCLASS.
