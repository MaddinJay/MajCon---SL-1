INTERFACE yif_planet
  PUBLIC .
  TYPES: BEGIN OF coordinates,
           coordinate_x TYPE int1,
           coordinate_y TYPE c LENGTH 1,
         END OF coordinates.
  TYPES: BEGIN OF ts_surface_field,
           coordinates TYPE coordinates,
           square      TYPE REF TO yif_square,
         END OF ts_surface_field.
  TYPES  tt_surface TYPE TABLE OF ts_surface_field.

  DATA surface TYPE tt_surface.

  METHODS create_surface.
  METHODS find_starting_point
    IMPORTING
      coordinates   TYPE coordinates
    RETURNING
      VALUE(result) TYPE REF TO yif_square.
  METHODS move IMPORTING direction     TYPE yif_sequence=>direction
                         square        TYPE REF TO yif_square
               RETURNING VALUE(result) TYPE REF TO yif_square.
ENDINTERFACE.
