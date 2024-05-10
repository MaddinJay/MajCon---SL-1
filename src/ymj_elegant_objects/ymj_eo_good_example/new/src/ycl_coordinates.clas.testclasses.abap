CLASS ltcl_coordinates DEFINITION DEFERRED.
CLASS ycl_coordinates DEFINITION LOCAL FRIENDS ltcl_coordinates.

CLASS ltcl_coordinates DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_coordinates.

    METHODS setup.

    METHODS should_have_coordinates_left  FOR TESTING.
    METHODS should_be_border_right4left   FOR TESTING.
    METHODS should_have_coordinates_right FOR TESTING.
    METHODS should_be_border_left4right   FOR TESTING.
    METHODS should_have_coordinates_up    FOR TESTING.
    METHODS should_have_coordinates_down  FOR TESTING.
    METHODS should_be_border_down4up      FOR TESTING.
    METHODS should_be_border_up4down      FOR TESTING.

    METHODS left_should_be_left4north FOR TESTING.

    METHODS left_should_be_up4east FOR TESTING.

    METHODS left_should_be_right4south FOR TESTING.

    METHODS left_should_be_down4west FOR TESTING.
ENDCLASS.


CLASS ltcl_coordinates IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_coordinates( map_size = VALUE #( length = 3 width = 3 )
                               geo_data = NEW ycl_geo_data( cardinal_point = yif_geo_data=>c_cardinal_point-north
                                                            sequence_list  = VALUE #( )
                                                            starting_point = VALUE #( ) ) ).
  ENDMETHOD.

  METHOD should_have_coordinates_left.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1 coordinate_y = 2 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 2 coordinate_y = 2 )
                                                                              direction   = yif_sequence=>c_direction-left ) ).
  ENDMETHOD.

  METHOD should_have_coordinates_right.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 3 coordinate_y = 2 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 2 coordinate_y = 2 )
                                                                              direction   = yif_sequence=>c_direction-right ) ).
  ENDMETHOD.

  METHOD should_have_coordinates_up.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 2 coordinate_y = 1 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 2 coordinate_y = 2 )
                                                                              direction   = yif_sequence=>c_direction-up ) ).
  ENDMETHOD.

  METHOD should_have_coordinates_down.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 2 coordinate_y = 3 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 2 coordinate_y = 2 )
                                                                              direction   = yif_sequence=>c_direction-down ) ).
  ENDMETHOD.

  METHOD should_be_border_right4left.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 3 coordinate_y = 3 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 1 coordinate_y = 3 )
                                                                              direction   = yif_sequence=>c_direction-left ) ).
  ENDMETHOD.

  METHOD should_be_border_left4right.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1 coordinate_y = 3 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 3 coordinate_y = 3 )
                                                                              direction   = yif_sequence=>c_direction-right ) ).
  ENDMETHOD.

  METHOD should_be_border_down4up.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 3 coordinate_y = 3 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 3 coordinate_y = 1 )
                                                                              direction   = yif_sequence=>c_direction-up ) ).
  ENDMETHOD.

  METHOD should_be_border_up4down.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 3 coordinate_y = 1 )
                                        act = cut->yif_coordinates~determine_coord_neighbour( coordinates = VALUE #( coordinate_x = 3 coordinate_y = 3 )
                                                                              direction   = yif_sequence=>c_direction-down ) ).
  ENDMETHOD.

  METHOD left_should_be_left4north.
    cl_abap_unit_assert=>assert_equals( exp = yif_sequence=>c_direction-left
                                        act = cut->transform_direction( direction = yif_sequence=>c_direction-left ) ).
  ENDMETHOD.

  METHOD left_should_be_up4east.
    cl_abap_unit_assert=>assert_equals( exp = yif_sequence=>c_direction-up
                                        act = NEW ycl_coordinates( map_size = VALUE #( length = 3 width = 3 ) geo_data = NEW ycl_geo_data( cardinal_point = yif_geo_data=>c_cardinal_point-east
                                                                                                                                   sequence_list  = VALUE #( )
                                                                                                                                   starting_point = VALUE #( ) ) )->transform_direction( direction = yif_sequence=>c_direction-left ) ).
  ENDMETHOD.

  METHOD left_should_be_right4south.
    cl_abap_unit_assert=>assert_equals( exp = yif_sequence=>c_direction-right
                                        act = NEW ycl_coordinates( map_size = VALUE #( length = 3 width = 3 ) geo_data = NEW ycl_geo_data( cardinal_point = yif_geo_data=>c_cardinal_point-south
                                                                                                                                   sequence_list  = VALUE #( )
                                                                                                                                   starting_point = VALUE #( ) ) )->transform_direction( direction = yif_sequence=>c_direction-left ) ).
  ENDMETHOD.

  METHOD left_should_be_down4west.
    cl_abap_unit_assert=>assert_equals( exp = yif_sequence=>c_direction-down
                                        act = NEW ycl_coordinates( map_size = VALUE #( length = 3 width = 3 )  geo_data = NEW ycl_geo_data( cardinal_point = yif_geo_data=>c_cardinal_point-west
                                                                                                                                   sequence_list  = VALUE #( )
                                                                                                                                   starting_point = VALUE #( ) ) )->transform_direction( direction = yif_sequence=>c_direction-left ) ).
  ENDMETHOD.
ENDCLASS.
