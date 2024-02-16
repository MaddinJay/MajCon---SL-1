class ltcl_column_left DEFINITION DEFERRED.
class ycl_column_left DEFINITION LOCAL FRIENDS ltcl_column_left.

CLASS ltcl_column_left DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO ycl_column_left.

    METHODS:
      divide_by_2_and_flat FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_column_left IMPLEMENTATION.

  METHOD divide_by_2_and_flat.
    cut = NEW #( 5 ).
    cut->divide( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->number ).
  ENDMETHOD.

ENDCLASS.
