CLASS ycl_column_left DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING number TYPE int4,
      divide.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA number TYPE int4.

ENDCLASS.

CLASS ycl_column_left IMPLEMENTATION.

  METHOD constructor.
    me->number = number.
  ENDMETHOD.

  METHOD divide.
    number = round( val = ( number / 2 ) dec = 0  mode = cl_abap_math=>round_down ).
  ENDMETHOD.

ENDCLASS.
