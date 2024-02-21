CLASS ltd_column_left DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING column_left TYPE REF TO yif_column_left.
    METHODS is_1_reached_after_divide RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA column_left TYPE REF TO yif_column_left.
ENDCLASS.

CLASS ltd_column_left IMPLEMENTATION.

  METHOD constructor.
    me->column_left = column_left.
  ENDMETHOD.

  METHOD is_1_reached_after_divide.
    column_left->divide( ).
    result = column_left->is_number_one_reached( ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
CLASS ltcl_column_left DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO ltd_column_left.

    METHODS:
      number_one_is_reached FOR TESTING,
      number_one_is_not_reached_yet FOR TESTING.
ENDCLASS.

CLASS ltcl_column_left IMPLEMENTATION.

  METHOD number_one_is_reached.
    cut = NEW #( NEW ycl_column_left( number       = 2
                                      column_right = NEW ycl_column_right( number = 4 ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = abap_true act = cut->is_1_reached_after_divide( ) ).
  ENDMETHOD.

  METHOD number_one_is_not_reached_yet.
    cut = NEW #( NEW ycl_column_left( number       = 4
                                      column_right = NEW ycl_column_right( number = 4 ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false act = cut->is_1_reached_after_divide( ) ).
  ENDMETHOD.

ENDCLASS.
