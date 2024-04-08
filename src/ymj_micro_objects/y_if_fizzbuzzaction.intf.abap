INTERFACE y_if_fizzbuzzaction
  PUBLIC .
  METHODS act IMPORTING input         TYPE REF TO ycl_input
              RETURNING VALUE(result) TYPE REF TO ycl_result.

ENDINTERFACE.
