INTERFACE yif_sequence
  PUBLIC .

  TYPES: BEGIN OF ENUM direction,
           left,
           right,
           up,
           down,
         END OF ENUM direction.

  TYPES: BEGIN OF ty_list,
           direction TYPE direction,
         END OF ty_list,
         tt_list TYPE STANDARD TABLE OF ty_list WITH DEFAULT KEY.

  METHODS read_sequence RETURNING VALUE(result) TYPE tt_list.
ENDINTERFACE.
