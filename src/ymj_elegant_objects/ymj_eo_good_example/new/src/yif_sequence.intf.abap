INTERFACE yif_sequence
  PUBLIC .
  TYPES direction TYPE string.
  CONSTANTS: BEGIN OF c_direction,
               left  TYPE direction VALUE 'LEFT',
               right TYPE direction VALUE 'RIGHT',
               up    TYPE direction VALUE 'UP',
               down  TYPE direction VALUE 'DOWN',
             END OF c_direction.

  TYPES: BEGIN OF ty_list,
           direction TYPE direction,
         END OF ty_list,
         tt_list TYPE STANDARD TABLE OF ty_list WITH DEFAULT KEY.

  METHODS read_sequence RETURNING VALUE(result) TYPE tt_list.
ENDINTERFACE.
