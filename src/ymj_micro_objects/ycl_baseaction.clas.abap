CLASS ycl_baseaction DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ymj_if_fizzbuzzaction.
    ALIASES: act FOR ymj_if_fizzbuzzaction~act.

    METHODS constructor IMPORTING next_action TYPE REF TO ymj_if_fizzbuzzaction
                                  value       TYPE int1
                                  result      TYPE REF TO ycl_result.

  PRIVATE SECTION.
    DATA next_action TYPE REF TO ymj_if_fizzbuzzaction.
    DATA value       TYPE int1.
    DATA result      TYPE REF TO ycl_result.

    METHODS is_dividable_by_value IMPORTING input         TYPE REF TO ymj_input
                                  RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS ycl_baseaction IMPLEMENTATION.

  METHOD constructor.
    me->next_action = next_action.
    me->value       = value.
    me->result      = result.
  ENDMETHOD.

  METHOD ymj_if_fizzbuzzaction~act.
    result = COND #( WHEN is_dividable_by_value( input ) = abap_true THEN me->result
                     ELSE next_action->act( input ) ).
  ENDMETHOD.

  METHOD is_dividable_by_value.
    result = xsdbool( input->int( ) MOD value = 0 ).
  ENDMETHOD.


ENDCLASS.
