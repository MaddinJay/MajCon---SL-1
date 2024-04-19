CLASS ycl_planet_surface DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA surface TYPE STANDARD TABLE OF REF TO ycl_square.

    METHODS constructor IMPORTING planet_width  TYPE int1
                                  planet_length TYPE int1.
    METHODS create_surface.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA planet_width TYPE int1.
    DATA planet_length TYPE int1.
    METHODS build_coordinate_y
      IMPORTING
        index         TYPE int1
      RETURNING
        VALUE(result) TYPE char1.
ENDCLASS.



CLASS ycl_planet_surface IMPLEMENTATION.

  METHOD constructor.
    me->planet_width = planet_width.
    me->planet_length = planet_length.
  ENDMETHOD.

  METHOD create_surface.
    DO planet_width TIMES.
      DO planet_length TIMES.
        APPEND NEW ycl_square( has_obstacle = abap_true
                               coordinate_x = CONV #( sy-index )
                               coordinate_y = build_coordinate_y( CONV #( sy-index ) ) ) TO surface.
      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD build_coordinate_y.
    IF index = 1.
      result = 'A'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
