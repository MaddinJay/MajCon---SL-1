CLASS ycl_newtons_method DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS square_root IMPORTING number_x      TYPE f
                        RETURNING VALUE(result) TYPE f.

  PRIVATE SECTION.
    CONSTANTS acceptance_boundary TYPE f VALUE '0.0001'.

    METHODS square_iteration IMPORTING guess         TYPE f
                                       number_x      TYPE f
                             RETURNING VALUE(result) TYPE f.
    METHODS average IMPORTING number_x      TYPE f
                              number_y      TYPE f
                    RETURNING VALUE(result) TYPE f.
    METHODS improve IMPORTING guess         TYPE f
                              number_x      TYPE f
                    RETURNING VALUE(result) TYPE f.
    METHODS is_good_enough IMPORTING guess         TYPE f
                                     number_x      TYPE f
                           RETURNING VALUE(result) TYPE abap_bool.
    METHODS square
      IMPORTING
        number_x      TYPE f
      RETURNING
        VALUE(result) TYPE f.
ENDCLASS.

CLASS ycl_newtons_method IMPLEMENTATION.

  METHOD square_root.
    result = square_iteration( guess = 1 number_x = number_x ).
  ENDMETHOD.

  METHOD average.
    result = ( number_x + number_y ) / 2.
  ENDMETHOD.

  METHOD improve.
    result = average( number_x = guess number_y = number_x / guess  ).
  ENDMETHOD.

  METHOD is_good_enough.
    result = xsdbool( abs( square( guess ) - number_x ) < acceptance_boundary ).
  ENDMETHOD.

  METHOD square_iteration.
    result = COND #( WHEN is_good_enough( guess = guess number_x = number_x ) = abap_true THEN guess
                     ELSE square_iteration( guess = improve( guess = guess number_x = number_x ) number_x = number_x ) ).
  ENDMETHOD.

  METHOD square.
    result = number_x * number_x.
  ENDMETHOD.

ENDCLASS.
