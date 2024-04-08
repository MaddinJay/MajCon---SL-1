CLASS ymj_input DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING origin TYPE int1.
    METHODS int         RETURNING VALUE(result) TYPE int1.

  PRIVATE SECTION.
    DATA raw_value TYPE int1.

ENDCLASS.

CLASS ymj_input IMPLEMENTATION.

  METHOD constructor.
    me->raw_value = origin.
  ENDMETHOD.

  METHOD int.
    result = raw_value.
  ENDMETHOD.

ENDCLASS.
