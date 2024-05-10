CLASS ycl_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_map.

    METHODS constructor IMPORTING geo_data TYPE REF TO yif_geo_data " Decorator pattern -> create surface + calibrate
                                  map_size TYPE yif_map=>size
                                  spy      TYPE REF TO yif_spy.
  PRIVATE SECTION.
    DATA geo_data      TYPE REF TO yif_geo_data.
    DATA coordinates   TYPE REF TO yif_coordinates.
    DATA actual_coordinates TYPE yif_coordinates=>coordinates.
    DATA previous_coordinates TYPE yif_coordinates=>coordinates.
    DATA map_size TYPE yif_map=>size.
    DATA spy TYPE REF TO yif_spy.

    METHODS generate_random_obstacle RETURNING VALUE(result) TYPE abap_bool.

    METHODS remind_last_position.
    METHODS determine_coordinates_neighb IMPORTING index TYPE int1.
    METHODS return_neighbour             RETURNING VALUE(result) TYPE REF TO yif_square.

ENDCLASS.

CLASS ycl_map IMPLEMENTATION.

  METHOD constructor.
    me->geo_data = geo_data.
    me->map_size = map_size.
    me->spy      = spy.
  ENDMETHOD.

  METHOD yif_map~create_surface.
    CHECK yif_map~surface IS INITIAL. " Only create once for living object (immutable)
    DO map_size-width TIMES.
      DATA(actual_width_idx) = sy-index.
      DO map_size-length TIMES.
        APPEND VALUE yif_map=>ts_surface_field(  coordinates = VALUE #( coordinate_x = actual_width_idx
                                                                        coordinate_y = sy-index )
                                                 square      = NEW ycl_square( coordinates  = VALUE #( coordinate_x = actual_width_idx
                                                                                                       coordinate_y = sy-index )
                                                                               has_obstacle = generate_random_obstacle( )
                                                                               spy          = spy ) ) TO yif_map~surface.
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD yif_map~calibrate.
    me->coordinates = NEW ycl_coordinates( map_size = map_size geo_data = geo_data ).
  ENDMETHOD.

  METHOD yif_map~move2neighbour.
    remind_last_position( ).
    determine_coordinates_neighb( index ).
    result = return_neighbour( ).
  ENDMETHOD.

  METHOD yif_map~read_previous_coordinates.
    result = previous_coordinates.
  ENDMETHOD.

  METHOD yif_map~set_starting_point.
    actual_coordinates = geo_data->read_starting_point( ).
  ENDMETHOD.

  METHOD generate_random_obstacle.
    DATA random_number TYPE qfranint.

    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_min = 1
        ran_int_max = 100
      IMPORTING
        ran_int     = random_number.

    result = COND #( WHEN random_number MOD 2 = 0 THEN abap_false
                        ELSE abap_true ).
  ENDMETHOD.

  METHOD return_neighbour.
    result = yif_map~surface[ coordinates = actual_coordinates ]-square.
  ENDMETHOD.

  METHOD determine_coordinates_neighb.
    actual_coordinates = me->coordinates->determine_coord_neighbour(
                                      coordinates = previous_coordinates
                                      direction   = geo_data->read_direction( index ) ). ##TODO "Exception handling end of sequence
  ENDMETHOD.

  METHOD remind_last_position.
    previous_coordinates = COND #( WHEN actual_coordinates IS INITIAL THEN geo_data->read_starting_point( )
                                   ELSE actual_coordinates ).
  ENDMETHOD.

  METHOD yif_map~move_back.
    result = yif_map~surface[ coordinates = previous_coordinates ]-square.
  ENDMETHOD.

ENDCLASS.
