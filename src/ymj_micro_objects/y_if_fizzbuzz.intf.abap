INTERFACE y_if_fizzbuzz
  PUBLIC .
  METHODS convert IMPORTING input         TYPE REF TO ycl_input
                  RETURNING VALUE(result) TYPE REF TO ycl_result.
ENDINTERFACE.
