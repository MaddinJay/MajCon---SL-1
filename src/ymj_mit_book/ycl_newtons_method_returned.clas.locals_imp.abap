*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_cube IMPLEMENTATION.

  METHOD yif_function~g.
    result = x * x * x.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_square_root IMPLEMENTATION.

  METHOD yif_function~g.
    result = ( x * x ) - y.
  ENDMETHOD.

  METHOD constructor.
    me->y = y.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_newton_transform DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_newton_transform.
    METHODS constructor IMPORTING function TYPE REF TO yif_function.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA function TYPE REF TO yif_function.

    METHODS deriv IMPORTING function      TYPE REF TO yif_function
                            x             TYPE f
                  RETURNING VALUE(result) TYPE f.
ENDCLASS.

CLASS lcl_newton_transform IMPLEMENTATION.

  METHOD constructor.
    me->function = function.
  ENDMETHOD.

  METHOD deriv.
    CONSTANTS dx TYPE f VALUE '0.00001'.
    result = ( function->g( x + dx ) - function->g( x ) ) / dx.
  ENDMETHOD.

  METHOD lif_newton_transform~execute.
    result = x - function->g( x ) / deriv( function = function x = x ).

  ENDMETHOD.

ENDCLASS.
