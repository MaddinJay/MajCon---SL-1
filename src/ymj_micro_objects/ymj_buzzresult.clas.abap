CLASS ymj_buzzresult DEFINITION INHERITING FROM ymj_result
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS convert2string REDEFINITION.

ENDCLASS.

CLASS ymj_buzzresult IMPLEMENTATION.

  METHOD convert2string.
    result = 'Buzz'.
  ENDMETHOD.

ENDCLASS.
