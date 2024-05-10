INTERFACE yif_map
  PUBLIC .
  TYPES: width  TYPE int1,
         length TYPE int1.

  TYPES: BEGIN OF size,
           width  TYPE width,
           length TYPE length,
         END OF size.
  TYPES: BEGIN OF ts_surface_field,
           coordinates TYPE yif_coordinates=>coordinates,
           square      TYPE REF TO yif_square,
         END OF ts_surface_field.
  TYPES  tt_surface TYPE TABLE OF ts_surface_field.

  DATA surface TYPE tt_surface.

  METHODS create_surface.
  METHODS move2neighbour IMPORTING index         TYPE int1
                         RETURNING VALUE(result) TYPE REF TO yif_square.
  METHODS move_back IMPORTING index         TYPE int1
                    RETURNING VALUE(result) TYPE REF TO yif_square.
  METHODS read_previous_coordinates RETURNING VALUE(result) TYPE yif_coordinates=>coordinates.
  METHODS set_starting_point IMPORTING geo_data TYPE REF TO yif_geo_data.
  METHODS calibrate.

ENDINTERFACE.
