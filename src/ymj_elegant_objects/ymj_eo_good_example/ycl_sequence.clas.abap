CLASS ycl_sequence DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_sequence.

    METHODS constructor IMPORTING sequence_list TYPE yif_sequence=>tt_list.

  PRIVATE SECTION.
    DATA sequence_list TYPE yif_sequence=>tt_list.

ENDCLASS.

CLASS ycl_sequence IMPLEMENTATION.

  METHOD constructor.
    me->sequence_list = sequence_list.
  ENDMETHOD.

  METHOD yif_sequence~read_sequence.
    result = me->sequence_list.
  ENDMETHOD.

ENDCLASS.
