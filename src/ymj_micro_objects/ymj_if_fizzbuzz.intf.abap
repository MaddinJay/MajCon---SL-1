INTERFACE ymj_if_fizzbuzz
  PUBLIC .
  METHODS convert IMPORTING input         TYPE REF TO ymj_input
                  RETURNING VALUE(result) TYPE REF TO ymj_result.
ENDINTERFACE.
