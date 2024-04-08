CLASS ymj_fizzresult DEFINITION INHERITING FROM ymj_result
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert2string REDEFINITION.

ENDCLASS.

CLASS ymj_fizzresult IMPLEMENTATION.

  METHOD convert2string.
    result = 'Fizz'.
  ENDMETHOD.

ENDCLASS.
