
CLASS ltd_planet DEFINITION.

  PUBLIC SECTION.
    INTERFACES yif_planet.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_planet IMPLEMENTATION.

  METHOD yif_planet~create_surface.

  ENDMETHOD.

  METHOD yif_planet~find_starting_point.
    result = NEW ycl_square( coordinates  = VALUE #( coordinate_x = 1 coordinate_y = 1 )
                             has_obstacle = abap_false ).
  ENDMETHOD.

ENDCLASS.
