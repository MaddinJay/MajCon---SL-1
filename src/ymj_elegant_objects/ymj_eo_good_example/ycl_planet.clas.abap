CLASS ycl_planet DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_planet.
    ALIASES: create_surface FOR yif_planet~create_surface.
    ALIASES: find_starting_point FOR yif_planet~find_starting_point.
    ALIASES: move FOR yif_planet~move.
    ALIASES: surface FOR yif_planet~surface.
    METHODS constructor IMPORTING planet_width  TYPE int1
                                  planet_length TYPE int1.

  PRIVATE SECTION.
    DATA planet_width TYPE int1.
    DATA planet_length TYPE int1.
    METHODS create_squares.
    METHODS add_neighbours.
    METHODS move_to_neighbour
      IMPORTING
        direction     TYPE yif_sequence=>direction
        coordinates   TYPE yif_planet=>coordinates
      RETURNING
        VALUE(result) TYPE REF TO yif_square.

ENDCLASS.

CLASS ycl_planet IMPLEMENTATION.

  METHOD constructor.
    me->planet_width  = planet_width.
    me->planet_length = planet_length.
  ENDMETHOD.

  METHOD yif_planet~create_surface.
    create_squares( ).
    add_neighbours( ).
  ENDMETHOD.

  METHOD create_squares.
*    DO planet_length TIMES.
*      DATA(idx_coordinate_y) = sy-index.
*      DO planet_width TIMES.
*        APPEND NEW ycl_square( has_obstacle = abap_true
*                               coordinates  = VALUE #( coordinate_x = sy-index
*                                                       coordinate_y = idx_coordinate_y ) ) TO yif_planet=>.
*      ENDDO.
*    ENDDO.
  ENDMETHOD.

  METHOD add_neighbours.
*    DATA(index) = 1.
*    LOOP AT surface INTO DATA(square).
*      IF index <= planet_width.
*        NEW ycl_neighbour_hood( )->add_neighbour( direction = yif_neighbour_hood=>left
*                                                  neighbour = surface[ sy-index ] ).
*      ELSE.
*        index = 1.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.

  METHOD yif_planet~find_starting_point.

  ENDMETHOD.

  METHOD yif_planet~move.
    LOOP AT surface INTO DATA(surface_field).
      IF square = surface_field-square.
        result = move_to_neighbour( direction = direction
                                    coordinates = surface_field-coordinates ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD move_to_neighbour.
    DATA(new_coordinates) = SWITCH yif_planet=>coordinates( direction
                            WHEN yif_sequence=>left THEN VALUE #( coordinate_x = coordinates-coordinate_x - 1 coordinate_y = coordinates-coordinate_y ) ).
    result = surface[ coordinates = new_coordinates ]-square.
  ENDMETHOD.

ENDCLASS.
