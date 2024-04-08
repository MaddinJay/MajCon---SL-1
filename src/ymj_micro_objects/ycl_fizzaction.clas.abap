CLASS ycl_fizzaction DEFINITION INHERITING FROM ycl_baseaction
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING next_action TYPE REF TO ymj_if_fizzbuzzaction.

  PRIVATE SECTION.
    CONSTANTS fizzvalue TYPE int1 VALUE 3.

ENDCLASS.

CLASS ycl_fizzaction IMPLEMENTATION.

  METHOD constructor.
    super->constructor( next_action = next_action value = fizzvalue result = NEW ymj_fizzresult( ) ).
  ENDMETHOD.

ENDCLASS.
