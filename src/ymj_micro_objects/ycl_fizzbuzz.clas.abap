CLASS ycl_fizzbuzz DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_fizzbuzz.
    ALIASES convert FOR yif_fizzbuzz~convert.

    METHODS constructor.

  PRIVATE SECTION.
    DATA action TYPE REF TO ymj_if_fizzbuzzaction.

ENDCLASS.

CLASS ycl_fizzbuzz IMPLEMENTATION.

  METHOD constructor.
    me->action = NEW ycl_fizzbuzzaction(
                      NEW ycl_buzzaction(
                        NEW ycl_fizzaction(
                          NEW ycl_tostringaction( ) ) ) ).
  ENDMETHOD.

  METHOD yif_fizzbuzz~convert.
    result = action->act( input ).
  ENDMETHOD.

ENDCLASS.
