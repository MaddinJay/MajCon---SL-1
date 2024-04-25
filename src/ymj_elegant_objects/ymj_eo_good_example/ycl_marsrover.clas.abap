CLASS ycl_marsrover DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_marsrover.
    METHODS constructor IMPORTING cardinal_point TYPE yif_marsrover=>cardinal_point
                                  sequence_list  TYPE yif_sequence=>tt_list
                                  starting_point TYPE yif_planet=>coordinates
                                  planet         TYPE REF TO yif_planet.

  PRIVATE SECTION.
    DATA cardinal_point TYPE yif_marsrover=>cardinal_point.
    DATA sequence_list TYPE yif_sequence=>tt_list.
    DATA starting_point TYPE yif_planet=>coordinates.
    DATA planet         TYPE REF TO yif_planet.
    METHODS find_starting_square
      RETURNING
        VALUE(result) TYPE REF TO yif_square.

ENDCLASS.

CLASS ycl_marsrover IMPLEMENTATION.

  METHOD constructor.
    me->cardinal_point = cardinal_point.
    me->sequence_list  = sequence_list.
    me->starting_point = starting_point.
    me->planet         = planet.
  ENDMETHOD.



  METHOD yif_marsrover~scan_and_move.
    DATA(starting_square) = find_starting_square( ).

    "
  ENDMETHOD.


  METHOD find_starting_square.
    result = planet->find_starting_point( starting_point ).
  ENDMETHOD.

ENDCLASS.
