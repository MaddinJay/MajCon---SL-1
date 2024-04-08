CLASS ycl_fizzbuzzresult DEFINITION INHERITING FROM ycl_result
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert2string REDEFINITION.

ENDCLASS.

CLASS ycl_fizzbuzzresult IMPLEMENTATION.

  METHOD convert2string.
    result = 'FizzBuzz'.
  ENDMETHOD.

ENDCLASS.
