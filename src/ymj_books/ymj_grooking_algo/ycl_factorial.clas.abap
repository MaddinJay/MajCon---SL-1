CLASS ycl_factorial DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS fact IMPORTING number        TYPE int4
                 RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS ycl_factorial IMPLEMENTATION.

  METHOD fact.
    result = COND #(  WHEN number = 1 THEN 1
                      ELSE number * fact( number - 1 ) ).
  ENDMETHOD.

ENDCLASS.
