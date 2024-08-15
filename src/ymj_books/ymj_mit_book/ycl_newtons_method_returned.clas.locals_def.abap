
CLASS lcl_cube DEFINITION.

  PUBLIC SECTION.
    INTERFACES yif_function.
ENDCLASS.

CLASS lcl_square_root DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_function.

    METHODS constructor IMPORTING y TYPE f.

  PRIVATE SECTION.
    DATA y TYPE f.

ENDCLASS.

INTERFACE lif_newton_transform.
  METHODS execute
    IMPORTING
      x             TYPE f
    RETURNING
      VALUE(result) TYPE f.

ENDINTERFACE.
