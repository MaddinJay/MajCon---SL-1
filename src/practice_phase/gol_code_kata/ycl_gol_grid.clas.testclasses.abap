CLASS ltcl_gol_grid DEFINITION DEFERRED.
CLASS ycl_gol_grid DEFINITION LOCAL FRIENDS ltcl_gol_grid.

CLASS ltcl_gol_grid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE  REF TO ycl_gol_grid.

    METHODS:
      "GIVEN: Size of grid is 3 WHEN: Creating the grid THEN: Last line in grid ...
      should_have_coordinates_3_3 FOR TESTING,
      "GIVEN: Grid of size 3 WHEN: Checking for neighbours THEN: cell = 3 3 (last line in grid)...
      should_have_neighbours FOR TESTING,
      ##TODO " Add Testcase for play round
      create_cut IMPORTING size          TYPE int1
                 RETURNING VALUE(result) TYPE REF TO ycl_gol_grid,
      get_last_line_of_3x3_grid RETURNING VALUE(result) TYPE yif_gol_grid=>ts_grid.
ENDCLASS.

CLASS ltcl_gol_grid IMPLEMENTATION.

  METHOD should_have_coordinates_3_3.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_last_line_of_3x3_grid( )-coordinates-x ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_last_line_of_3x3_grid( )-coordinates-y ).
  ENDMETHOD.

  METHOD should_have_neighbours.
    cl_abap_unit_assert=>assert_bound( act = get_last_line_of_3x3_grid( )-cell->get_neighbours( ) ).
  ENDMETHOD.

  METHOD create_cut.
    result = NEW ycl_gol_grid( ).
    result->yif_gol_grid~create( size ).
  ENDMETHOD.

  METHOD get_last_line_of_3x3_grid.
    DATA(cut) = create_cut( size = 3 ).
    result = cut->grid[ 9 ].
  ENDMETHOD.

ENDCLASS.
