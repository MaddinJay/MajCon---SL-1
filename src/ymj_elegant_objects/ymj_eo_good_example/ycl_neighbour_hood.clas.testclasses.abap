CLASS ltcl_neighbours DEFINITION DEFERRED.
CLASS ycl_neighbour_hood DEFINITION LOCAL FRIENDS ltcl_neighbours.
CLASS ltcl_neighbours DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_neighbour_hood.
    DATA square_left TYPE REF TO ycl_square.
    DATA square_right TYPE REF TO ycl_square.

    METHODS:
      should_add_left_neighbour FOR TESTING,
      neighbour_should_have_obst FOR TESTING,
      should_move2left_neighbour FOR TESTING,
      should_move2_right_neighbour FOR TESTING,
      setup.
ENDCLASS.


CLASS ltcl_neighbours IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_neighbour_hood( ).
    square_left = NEW ycl_square( coordinate_x = 1
                                  coordinate_y = 'A'
                                  has_obstacle = abap_true ).
    cut->add_neighbour(
      direction = yif_neighbour_hood=>left
      neighbour = square_left ).
    square_right = NEW ycl_square( coordinate_x = 3
                                  coordinate_y = 'A'
                                  has_obstacle = abap_false ).
    cut->add_neighbour(
      direction = yif_neighbour_hood=>right
      neighbour = square_right
    ).
  ENDMETHOD.

  METHOD should_add_left_neighbour.
    cl_abap_unit_assert=>assert_true( xsdbool( cut->list_of_neighbours[ direction = yif_neighbour_hood=>left ]-neighbour IS INSTANCE OF ycl_square ) ).


  ENDMETHOD.

  METHOD neighbour_should_have_obst.
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = cut->scan_neighbour( yif_neighbour_hood=>left ) ).
  ENDMETHOD.

  METHOD should_move2left_neighbour.
    cl_abap_unit_assert=>assert_equals( exp = square_left act = cut->move2neighbour( yif_neighbour_hood=>left ) ).
  ENDMETHOD.

  METHOD should_move2_right_neighbour.
    cl_abap_unit_assert=>assert_equals( exp = square_right act = cut->go( yif_neighbour_hood=>right ) ).
  ENDMETHOD.

ENDCLASS.
