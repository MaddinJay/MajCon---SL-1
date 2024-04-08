CLASS ymj_tostringresult DEFINITION INHERITING FROM ymj_result
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING origin TYPE REF TO ymj_input.

    METHODS convert2string REDEFINITION.

  PRIVATE SECTION.
    DATA origin TYPE REF TO ymj_input.

ENDCLASS.

CLASS ymj_tostringresult IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->origin = origin.
  ENDMETHOD.

  METHOD convert2string.
    result = condense( CONV string( origin->int( ) ) ).
  ENDMETHOD.

ENDCLASS.
