CLASS ycl_tostringaction DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ymj_if_fizzbuzzaction.

ENDCLASS.

CLASS ycl_tostringaction IMPLEMENTATION.

  METHOD ymj_if_fizzbuzzaction~act.
    result = NEW ycl_tostringresult( input ).
  ENDMETHOD.

ENDCLASS.
