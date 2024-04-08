CLASS ycl_buzzaction DEFINITION INHERITING FROM ycl_baseaction
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING next_action TYPE REF TO ymj_if_fizzbuzzaction OPTIONAL.

  PRIVATE SECTION.
    CONSTANTS buzzvalue TYPE int1 VALUE 5.

ENDCLASS.

CLASS ycl_buzzaction IMPLEMENTATION.

  METHOD constructor.
    super->constructor( next_action = next_action value = buzzvalue result = NEW ycl_buzzresult( ) ).
  ENDMETHOD.

ENDCLASS.
