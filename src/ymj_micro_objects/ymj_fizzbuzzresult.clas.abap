CLASS ymj_fizzbuzzresult DEFINITION INHERITING FROM ymj_result
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert2string REDEFINITION.

ENDCLASS.

CLASS ymj_fizzbuzzresult IMPLEMENTATION.

  METHOD convert2string.
    result = 'FizzBuzz'.
  ENDMETHOD.

ENDCLASS.
