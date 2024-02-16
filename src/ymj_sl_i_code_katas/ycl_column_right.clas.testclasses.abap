CLASS ltcl_column_right DEFINITION DEFERRED.
CLASS ycl_column_right DEFINITION LOCAL FRIENDS ltcl_column_right.

CLASS ltcl_column_right DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_column_right.
    METHODS:
      double_number FOR TESTING RAISING cx_static_check,
      sum_up FOR TESTING RAISING cx_static_check,
      setup.
ENDCLASS.


CLASS ltcl_column_right IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_column_right( 2 ).
  ENDMETHOD.

  METHOD double_number.
    cut->double( ).

    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->numbers[ 2 ] ).
  ENDMETHOD.

  METHOD sum_up.

    cut->double( ).
    cut->sum( ).

    cl_abap_unit_assert=>assert_equals( exp = 6 act = cut->sum( ) ).
  ENDMETHOD.

ENDCLASS.
