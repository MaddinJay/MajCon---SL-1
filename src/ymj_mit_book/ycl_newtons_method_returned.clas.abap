CLASS ycl_newtons_method_returned DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING function TYPE REF TO yif_function.

    METHODS newton_method IMPORTING guess         TYPE f
                          RETURNING VALUE(result) TYPE f.

  PRIVATE SECTION.
    DATA function TYPE REF TO yif_function.


    METHODS fixed_point IMPORTING f             TYPE REF TO lif_newton_transform
                                  first_guess   TYPE f
                        RETURNING VALUE(result) TYPE f.
    METHODS try_with    IMPORTING f             TYPE REF TO lif_newton_transform
                                  guess         TYPE f
                        RETURNING
                                  VALUE(result) TYPE f.
    METHODS close_enough IMPORTING x             TYPE f
                                   y             TYPE f
                         RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS ycl_newtons_method_returned IMPLEMENTATION.

  METHOD fixed_point.
    result = try_with( f = f guess = first_guess ).
  ENDMETHOD.


  METHOD try_with.
    DATA(next) = f->execute( guess ).
    result = COND #( WHEN close_enough( x = guess y = next ) THEN next
                     ELSE try_with( f = f guess = next ) ).
  ENDMETHOD.

  METHOD constructor.
    me->function = function.
  ENDMETHOD.

  METHOD close_enough.
    DATA(abs) = COND f( WHEN x > y THEN x - y
                        ELSE y - x ).
    result = xsdbool( abs < '0.00001'  ).
  ENDMETHOD.


  METHOD newton_method.
    result = fixed_point( f           = NEW lcl_newton_transform( function )
                          first_guess = guess ).
  ENDMETHOD.
ENDCLASS.
