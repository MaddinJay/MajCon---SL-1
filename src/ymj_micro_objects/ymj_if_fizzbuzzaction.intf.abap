INTERFACE ymj_if_fizzbuzzaction
  PUBLIC .
  METHODS act
    IMPORTING
      input         TYPE REF TO ymj_input
    RETURNING
      VALUE(result) TYPE REF TO ymj_result.

ENDINTERFACE.
