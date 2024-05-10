CLASS ycl_coordinates DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_coordinates.

    METHODS constructor IMPORTING map_size TYPE yif_map=>size
                                  geo_data TYPE REF TO yif_geo_data.


  PRIVATE SECTION.
    DATA map_size TYPE yif_map=>size.
    DATA geo_data TYPE REF TO yif_geo_data.

    METHODS determine_coord_left  IMPORTING coordinate    TYPE yif_coordinates=>coordinates-coordinate_x
                                  RETURNING VALUE(result) TYPE yif_coordinates=>coordinates-coordinate_x.
    METHODS determine_coord_right IMPORTING coordinate    TYPE yif_coordinates=>coordinates-coordinate_x
                                  RETURNING VALUE(result) TYPE yif_coordinates=>coordinates-coordinate_x.
    METHODS determine_coord_up    IMPORTING coordinate    TYPE yif_coordinates=>coordinates-coordinate_y
                                  RETURNING VALUE(result) TYPE yif_coordinates=>coordinates-coordinate_y.
    METHODS determine_coord_down  IMPORTING coordinate    TYPE yif_coordinates=>coordinates-coordinate_y
                                  RETURNING VALUE(result) TYPE yif_coordinates=>coordinates-coordinate_y.
    METHODS transform_direction   IMPORTING direction     TYPE yif_sequence=>direction
                                  RETURNING VALUE(result) TYPE yif_sequence=>direction.
    METHODS transform_direction4east IMPORTING direction     TYPE yif_sequence=>direction
                                     RETURNING VALUE(result) TYPE yif_sequence=>direction.
    METHODS transform_direction4south IMPORTING direction     TYPE yif_sequence=>direction
                                      RETURNING VALUE(result) TYPE yif_sequence=>direction.
    METHODS transform_direction4west  IMPORTING direction     TYPE yif_sequence=>direction
                                      RETURNING VALUE(result) TYPE yif_sequence=>direction.
ENDCLASS.

CLASS ycl_coordinates IMPLEMENTATION.

  METHOD constructor.
    me->map_size = map_size.
    me->geo_data = geo_data.
  ENDMETHOD.

  METHOD yif_coordinates~determine_coord_neighbour.
    result = SWITCH yif_coordinates=>coordinates( transform_direction( direction )
                                             WHEN yif_sequence=>c_direction-left  THEN VALUE #( coordinate_x = determine_coord_left( coordinates-coordinate_x )
                                                                                                coordinate_y = coordinates-coordinate_y )
                                             WHEN yif_sequence=>c_direction-right THEN VALUE #( coordinate_x = determine_coord_right( coordinates-coordinate_x )
                                                                                                coordinate_y = coordinates-coordinate_y  )
                                             WHEN yif_sequence=>c_direction-up    THEN VALUE #( coordinate_x = coordinates-coordinate_x
                                                                                                coordinate_y = determine_coord_up( coordinates-coordinate_y ) )
                                             WHEN yif_sequence=>c_direction-down  THEN VALUE #( coordinate_x = coordinates-coordinate_x
                                                                                                coordinate_y = determine_coord_down( coordinates-coordinate_y ) ) ).
  ENDMETHOD.

  METHOD determine_coord_left.
    result = COND #( WHEN coordinate = 1 THEN map_size-width " left border -> jump to right border
                     ELSE coordinate - 1 ).
  ENDMETHOD.

  METHOD determine_coord_right.
    result = COND #( WHEN coordinate = map_size-width THEN 1 " right border -> jump to left border
                     ELSE coordinate + 1 ).
  ENDMETHOD.

  METHOD determine_coord_up.
    result = COND #( WHEN coordinate = 1 THEN map_size-length
                     ELSE coordinate - 1 ).
  ENDMETHOD.

  METHOD determine_coord_down.
    result = COND #( WHEN coordinate = map_size-length THEN 1
                     ELSE coordinate + 1 ).
  ENDMETHOD.

  METHOD transform_direction.
    result = SWITCH #( geo_data->read_cardinal_point( )
                       WHEN yif_geo_data=>c_cardinal_point-north THEN direction
                       WHEN yif_geo_data=>c_cardinal_point-east  THEN transform_direction4east( direction )
                       WHEN yif_geo_data=>c_cardinal_point-south THEN transform_direction4south( direction )
                       WHEN yif_geo_data=>c_cardinal_point-west  THEN transform_direction4west( direction ) ).
  ENDMETHOD.

  METHOD transform_direction4east.
    result = SWITCH #( direction
                       WHEN yif_sequence=>c_direction-left  THEN yif_sequence=>c_direction-up
                       WHEN yif_sequence=>c_direction-up    THEN yif_sequence=>c_direction-right
                       WHEN yif_sequence=>c_direction-right THEN yif_sequence=>c_direction-down
                       WHEN yif_sequence=>c_direction-down  THEN yif_sequence=>c_direction-left ).
  ENDMETHOD.

  METHOD transform_direction4south.
    result = SWITCH #( direction
                       WHEN yif_sequence=>c_direction-left  THEN yif_sequence=>c_direction-right
                       WHEN yif_sequence=>c_direction-up    THEN yif_sequence=>c_direction-down
                       WHEN yif_sequence=>c_direction-right THEN yif_sequence=>c_direction-left
                       WHEN yif_sequence=>c_direction-down  THEN yif_sequence=>c_direction-up ).
  ENDMETHOD.

  METHOD transform_direction4west.
    result = SWITCH #( direction
                       WHEN yif_sequence=>c_direction-left  THEN yif_sequence=>c_direction-down
                       WHEN yif_sequence=>c_direction-up    THEN yif_sequence=>c_direction-right
                       WHEN yif_sequence=>c_direction-right THEN yif_sequence=>c_direction-down
                       WHEN yif_sequence=>c_direction-down  THEN yif_sequence=>c_direction-left ).
  ENDMETHOD.

ENDCLASS.
