CLASS ycl_fizzresult DEFINITION INHERITING FROM ycl_result
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert2string REDEFINITION.

ENDCLASS.

CLASS ycl_fizzresult IMPLEMENTATION.

  METHOD convert2string.
    result = 'Fizz'.
  ENDMETHOD.

ENDCLASS.
