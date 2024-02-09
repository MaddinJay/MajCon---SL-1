CLASS ltcl_multiplication DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_multiplication.
    METHODS:
      test_russian_peasant FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_multiplication IMPLEMENTATION.

  METHOD test_russian_peasant.
    cut = NEW ycl_multiplication( multiplication = NEW ycl_russian_peasant( ) ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 1974
        act                  = cut->multiplicate( number_one = 47
                                                  number_two = 42 )
    ).
  ENDMETHOD.

ENDCLASS.
