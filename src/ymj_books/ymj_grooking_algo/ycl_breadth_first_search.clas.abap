CLASS ycl_breadth_first_search DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_queue,
             name   TYPE string,
             friend TYPE string,
           END OF ts_queue.
    TYPES tt_queue TYPE STANDARD TABLE OF ts_queue WITH KEY name friend.

    METHODS constructor IMPORTING queue         TYPE tt_queue
                                  starting_name TYPE string.

    METHODS search RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_searched_queue,
             name TYPE string,
           END OF ts_searched_queue,
           tt_searched_queue TYPE HASHED TABLE OF ts_searched_queue WITH UNIQUE KEY name.

    DATA queue         TYPE tt_queue.
    DATA search_queue  TYPE tt_queue.
    DATA searched_queue  TYPE tt_searched_queue.
    DATA starting_name TYPE string.

    METHODS person_is_seller IMPORTING person        TYPE string
                             RETURNING VALUE(result) TYPE abap_bool.
    METHODS graph            IMPORTING person        TYPE string
                             RETURNING VALUE(result) TYPE tt_queue.
    METHODS popleft          RETURNING VALUE(result) TYPE string.
    METHODS pop_friends2search_queue IMPORTING person TYPE string.
    METHODS not_checked_yet
      IMPORTING
        person        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS pop_person2searched_queue
      IMPORTING
        person TYPE string.
ENDCLASS.

CLASS ycl_breadth_first_search IMPLEMENTATION.

  METHOD constructor.
    me->queue = queue.
    me->starting_name = starting_name.
  ENDMETHOD.

  METHOD search.
    search_queue = graph( starting_name ).

    WHILE search_queue IS NOT INITIAL.
      DATA(person) = popleft( ).

      IF not_checked_yet( person ) AND
         person_is_seller( person ).
        result = |{ person } is a mango seller!|.
        RETURN.
      ENDIF.
      pop_friends2search_queue( person ).
      pop_person2searched_queue( person ).
    ENDWHILE.
  ENDMETHOD.

  METHOD popleft.
    result  = search_queue[ 1 ]-friend.
    DELETE search_queue INDEX 1.
  ENDMETHOD.

  METHOD person_is_seller.
    DATA(strlen) = strlen( person ) - 1.
    result = xsdbool( person+strlen = 'm' ).
  ENDMETHOD.

  METHOD graph.
    result = VALUE #( FOR <line> IN queue WHERE ( name = person ):
                        ( <line> ) ).
  ENDMETHOD.

  METHOD pop_friends2search_queue.
    APPEND LINES OF graph( person ) TO search_queue.
  ENDMETHOD.

  METHOD not_checked_yet.
    TRY.
        DATA(dummy) = searched_queue[ name = person ].
      CATCH cx_sy_itab_line_not_found.
        result = abap_true.
    ENDTRY.
  ENDMETHOD.

  METHOD pop_person2searched_queue.
    INSERT VALUE #( name = person ) INTO TABLE searched_queue.
  ENDMETHOD.

ENDCLASS.
