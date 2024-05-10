CLASS ltcl_planet DEFINITION DEFERRED.
CLASS ycl_map DEFINITION LOCAL FRIENDS ltcl_planet.

CLASS ltd_map DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_map RETURNING VALUE(result) TYPE REF TO yif_map.
ENDCLASS.

CLASS ltd_map IMPLEMENTATION.

  METHOD create_map.
    result = NEW ycl_create_map( )->create(
                                      geo_data = NEW ycl_geo_data( cardinal_point = yif_geo_data=>c_cardinal_point-north
                                                                   starting_point = VALUE #( coordinate_x = 2 coordinate_y = 1 )
                                                                   sequence_list  = VALUE #( ( direction = yif_sequence=>c_direction-left )
                                                                                             ( direction = yif_sequence=>c_direction-left ) ) )
                                      map_size = VALUE #( length = 3 width  = 3 )
                                      spy      = NEW ycl_spy( ) ).
    result->move2neighbour( 1 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_planet DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS should_have_one_field        FOR TESTING.
    METHODS third_entry_should_be_1_3    FOR TESTING.
    METHODS should_move2neighbour        FOR TESTING.
    METHODS should_moved_back            FOR TESTING.

ENDCLASS.

CLASS ltcl_planet IMPLEMENTATION.

  METHOD should_have_one_field.
    cl_abap_unit_assert=>assert_equals( exp = 9 act = lines( ltd_map=>create_map( )->surface ) ).
  ENDMETHOD.

  METHOD third_entry_should_be_1_3.
    DATA(surface) = ltd_map=>create_map( )->surface.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 1 coordinate_y = 3 )
                                        act = surface[ 3 ]-coordinates ).
  ENDMETHOD.

  METHOD should_move2neighbour.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 3 coordinate_y = 1 )
                                        act = ltd_map=>create_map( )->move2neighbour( 2 )->read_coordinates( ) ).
  ENDMETHOD.

  METHOD should_moved_back.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_coordinates=>coordinates( coordinate_x = 2 coordinate_y = 1 )
                                        act = ltd_map=>create_map( )->move_back( index = 1 )->read_coordinates( ) ).
  ENDMETHOD.

ENDCLASS.
