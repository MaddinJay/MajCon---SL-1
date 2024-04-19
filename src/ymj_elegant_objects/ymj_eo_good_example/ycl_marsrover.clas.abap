CLASS ycl_marsrover DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_marsrover.
    METHODS constructor IMPORTING cardinal_point TYPE yif_marsrover=>cardinal_point
                                  sequence_list type yif_sequence=>tt_list.

  PRIVATE SECTION.
    DATA cardinal_point TYPE yif_marsrover=>cardinal_point.
    DATA sequence_list type yif_sequence=>tt_list.

ENDCLASS.

CLASS ycl_marsrover IMPLEMENTATION.

  METHOD constructor.
    me->cardinal_point = cardinal_point.
    me->sequence_list = sequence_list.
  ENDMETHOD.

ENDCLASS.
