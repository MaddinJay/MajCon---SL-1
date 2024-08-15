CLASS ltcl_newtons_method DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_newtons_method_returned.
    METHODS: test_newton_method FOR TESTING.
ENDCLASS.


CLASS ltcl_newtons_method IMPLEMENTATION.

  METHOD test_newton_method.
    DATA act TYPE p DECIMALS 6.
    cut = NEW #( function = NEW lcl_square_root( y = 5 ) ).

    act = cut->newton_method( guess = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = '2.236068' act = act ).
  ENDMETHOD.

ENDCLASS.
