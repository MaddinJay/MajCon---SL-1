CLASS ltcl_planet_surface DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_planet_surface.
    METHODS:
      should_have_one_field FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_planet_surface IMPLEMENTATION.

  METHOD should_have_one_field.
    cut = NEW #( planet_length = 3
                 planet_width  = 3 ).
    cut->create_surface( ).
    cl_abap_unit_assert=>assert_equals( exp = 9 act = lines( cut->surface ) ).
    ##TODO " Test coordinates
  ENDMETHOD.

ENDCLASS.
