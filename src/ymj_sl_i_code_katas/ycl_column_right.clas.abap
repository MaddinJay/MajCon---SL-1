CLASS ycl_column_right DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_column_right.

    METHODS:
      constructor IMPORTING number TYPE  int4.

  PRIVATE SECTION.
    DATA: number  TYPE int4,
          numbers TYPE STANDARD TABLE OF int4 WITH DEFAULT KEY.

    METHODS double_number.

    METHODS check_input IMPORTING number TYPE int4.
    METHODS set_number  IMPORTING number TYPE int4.
    METHODS memorize_number4sum_up.

ENDCLASS.

CLASS ycl_column_right IMPLEMENTATION.

  METHOD constructor.
    check_input( number ).
    set_number( number ).
    memorize_number4sum_up( ).
  ENDMETHOD.

  METHOD yif_column_right~double.
    double_number( ).

    IF is_countable = abap_true.
      memorize_number4sum_up( ).
    ENDIF.
  ENDMETHOD.

  METHOD yif_column_right~sum_up.
    result = REDUCE #( INIT sum = 0  FOR <number> IN me->numbers
                       NEXT sum = sum + <number> ).
  ENDMETHOD.

  METHOD memorize_number4sum_up.
    APPEND me->number TO me->numbers.
  ENDMETHOD.

  METHOD set_number.
    me->number = number.
  ENDMETHOD.

  METHOD double_number.
    me->number = me->number * 2.
  ENDMETHOD.

  METHOD check_input.
    ##TODO " check if number is valid
  ENDMETHOD.

ENDCLASS.
