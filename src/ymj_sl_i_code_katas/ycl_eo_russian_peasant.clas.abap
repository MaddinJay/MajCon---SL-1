CLASS ycl_eo_russian_peasant DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: yif_eo_russian_peasant.

    METHODS constructor IMPORTING number_left TYPE int4 number_right TYPE int4.

  PRIVATE SECTION.
    DATA column_right TYPE REF TO yif_column_right.
    DATA column_left  TYPE REF TO yif_column_left.

ENDCLASS.

CLASS ycl_eo_russian_peasant IMPLEMENTATION.

  METHOD constructor.
    column_right = NEW ycl_column_right( number = number_right ).
    column_left  = NEW ycl_column_left( number = number_left column_right = column_right ).
  ENDMETHOD.

  METHOD yif_eo_russian_peasant~execute.
    DATA number_one_reached TYPE abap_bool.

    WHILE number_one_reached = abap_false.
      column_left->divide( ).
      number_one_reached = column_left->is_number_one_reached( ).
    ENDWHILE.

    result = column_right->sum_up( ).
  ENDMETHOD.


ENDCLASS.
