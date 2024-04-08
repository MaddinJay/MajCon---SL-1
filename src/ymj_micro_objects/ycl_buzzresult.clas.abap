CLASS ycl_buzzresult DEFINITION INHERITING FROM ycl_result
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert2string REDEFINITION.

ENDCLASS.

CLASS ycl_buzzresult IMPLEMENTATION.

  METHOD convert2string.
    result = 'Buzz'.
  ENDMETHOD.

ENDCLASS.
