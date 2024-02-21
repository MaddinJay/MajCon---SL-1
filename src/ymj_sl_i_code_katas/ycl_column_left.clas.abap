CLASS ycl_column_left DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_column_left.

    METHODS:
      constructor IMPORTING number       TYPE int4
                            column_right TYPE REF TO yif_column_right.

  PRIVATE SECTION.
    DATA number TYPE int4.
    DATA column_right TYPE REF TO yif_column_right.

    METHODS is_not_dividable_by_2 IMPORTING number        TYPE int4
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS divide_by_2_and_round_down.
    METHODS check_number          IMPORTING number TYPE int4.
    METHODS set_number            IMPORTING number TYPE int4.
    METHODS set_column_right      IMPORTING column_right  TYPE REF TO yif_column_right.

ENDCLASS.

CLASS ycl_column_left IMPLEMENTATION.

  METHOD constructor.
    check_number( number ).
    set_number( number ).
    set_column_right( column_right ).
  ENDMETHOD.

  METHOD set_column_right.
    me->column_right = column_right.
  ENDMETHOD.

  METHOD set_number.
    me->number       = number.
  ENDMETHOD.

  METHOD yif_column_left~divide.
    divide_by_2_and_round_down( ).
    column_right->double( is_countable = is_not_dividable_by_2( number ) ).
  ENDMETHOD.

  METHOD yif_column_left~is_number_one_reached.
    result = xsdbool( number = 1 ).
  ENDMETHOD.

  METHOD divide_by_2_and_round_down.
    number = round( val = ( number / 2 ) dec = 0  mode = cl_abap_math=>round_down ).
  ENDMETHOD.

  METHOD is_not_dividable_by_2.
    result = xsdbool( number MOD 2 <> 0 ).
  ENDMETHOD.

  METHOD check_number.
    ##TODO " check if number is valid
  ENDMETHOD.

ENDCLASS.
