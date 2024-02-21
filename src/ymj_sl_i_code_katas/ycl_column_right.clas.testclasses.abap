CLASS ltd_column_right DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING column_right TYPE REF TO yif_column_right.

    METHODS double_and_sum_up IMPORTING is_countable  TYPE abap_bool
                              RETURNING VALUE(result) TYPE int4.
  PRIVATE SECTION.
    DATA column_right TYPE REF TO yif_column_right.

ENDCLASS.

CLASS ltd_column_right IMPLEMENTATION.

  METHOD constructor.
    me->column_right = column_right.
  ENDMETHOD.

  METHOD double_and_sum_up.
    column_right->double( is_countable ).
    result = column_right->sum_up( ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
CLASS ltcl_column_right DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ltd_column_right.

    METHODS:
      setup,
      sum_up_with_countable_number FOR TESTING,
      sum_up_without_countable_no  FOR TESTING.

ENDCLASS.

CLASS ltcl_column_right IMPLEMENTATION.

  METHOD setup.
    cut = NEW ltd_column_right( NEW ycl_column_right( 2 ) ).
  ENDMETHOD.

  METHOD sum_up_with_countable_number.
    cl_abap_unit_assert=>assert_equals( exp = 6 act = cut->double_and_sum_up( is_countable = abap_true ) ).
  ENDMETHOD.

  METHOD sum_up_without_countable_no.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->double_and_sum_up( is_countable = abap_false ) ).
  ENDMETHOD.

ENDCLASS.
