CLASS ycl_column_right DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING number TYPE  int4,

      double,

      sum RETURNING VALUE(result) TYPE int4.

  PRIVATE SECTION.
    DATA: numbers TYPE STANDARD TABLE OF int4 WITH DEFAULT KEY.

ENDCLASS.

CLASS ycl_column_right IMPLEMENTATION.

  METHOD constructor.
    APPEND number TO me->numbers.
  ENDMETHOD.

  METHOD double.
    APPEND ( me->numbers[ lines( me->numbers ) ] * 2 ) TO me->numbers.
  ENDMETHOD.

  METHOD sum.
    result = REDUCE #( INIT sum = 0  FOR <number> IN me->numbers
                       NEXT sum = sum + <number> ).
  ENDMETHOD.

ENDCLASS.
