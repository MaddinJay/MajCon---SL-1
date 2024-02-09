CLASS ycl_multiplication DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: yif_multiplication.

    METHODS constructor IMPORTING multiplication TYPE REF TO yif_multiplication.

  PRIVATE SECTION.
    DATA multiplication TYPE REF TO yif_multiplication.

ENDCLASS.

CLASS ycl_multiplication IMPLEMENTATION.

  METHOD constructor.
    me->multiplication = multiplication.
  ENDMETHOD.

  METHOD yif_multiplication~multiplicate.
    rv_result = multiplication->multiplicate( number_one = number_one
                                             number_two = number_two ).
  ENDMETHOD.

ENDCLASS.
