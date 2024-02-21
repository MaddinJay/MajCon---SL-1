CLASS ltcl_russian_pesant DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_eo_russian_peasant.

    METHODS:
      setup,
      test_sum_up_successful FOR TESTING.

ENDCLASS.

CLASS ltcl_russian_pesant IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_eo_russian_peasant( number_left = 47 number_right = 42 ).
  ENDMETHOD.

  METHOD test_sum_up_successful.
    cl_abap_unit_assert=>assert_equals( exp = '1974' act = cut->execute( ) ).
  ENDMETHOD.

ENDCLASS.
