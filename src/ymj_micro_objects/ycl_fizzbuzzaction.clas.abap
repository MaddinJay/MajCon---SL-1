CLASS ycl_fizzbuzzaction DEFINITION INHERITING FROM ycl_baseaction
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING next_action TYPE REF TO ymj_if_fizzbuzzaction OPTIONAL.

  PRIVATE SECTION.
    CONSTANTS fizzbuzzvalue TYPE int1 VALUE 15. " 3*5

ENDCLASS.

CLASS ycl_fizzbuzzaction IMPLEMENTATION.

  METHOD constructor.
    super->constructor( next_action = next_action value = fizzbuzzvalue result = NEW ycl_fizzbuzzresult( ) ).
  ENDMETHOD.

ENDCLASS.
