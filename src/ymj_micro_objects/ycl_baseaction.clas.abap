CLASS ycl_baseaction DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ymj_if_fizzbuzzaction.
    ALIASES: act FOR ymj_if_fizzbuzzaction~act.

    METHODS constructor IMPORTING next_action TYPE REF TO ymj_if_fizzbuzzaction
                                  value       TYPE int1
                                  result      TYPE REF TO ymj_result.

  PRIVATE SECTION.
    DATA next_action TYPE REF TO ymj_if_fizzbuzzaction.
    DATA value TYPE int1.
    DATA result TYPE REF TO ymj_result.
ENDCLASS.

CLASS ycl_baseaction IMPLEMENTATION.

  METHOD constructor.
    me->next_action = next_action.
    me->value = value.
    me->result = result.
  ENDMETHOD.

  METHOD ymj_if_fizzbuzzaction~act.
    result = COND #( WHEN input->int( ) MOD value = 0 THEN result
                     ELSE next_action->act( input ) ).
  ENDMETHOD.


ENDCLASS.
