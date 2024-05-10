INTERFACE yif_geo_data
  PUBLIC .

  TYPES cardinal_point TYPE string.
  CONSTANTS: BEGIN OF c_cardinal_point,
               north TYPE cardinal_point VALUE 'NORTH',
               east  TYPE cardinal_point VALUE 'EAST',
               south TYPE cardinal_point VALUE 'SOUTH',
               west  TYPE cardinal_point VALUE 'WEST',
             END OF c_cardinal_point.

  METHODS read_cardinal_point RETURNING VALUE(result) TYPE cardinal_point.
  METHODS read_starting_point RETURNING VALUE(result) TYPE yif_coordinates=>coordinates.
  METHODS read_direction IMPORTING index TYPE int1 RETURNING VALUE(result) TYPE yif_sequence=>direction.
ENDINTERFACE.
