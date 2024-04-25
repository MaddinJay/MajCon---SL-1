CLASS ltcl_planet DEFINITION DEFERRED.
CLASS ycl_planet DEFINITION LOCAL FRIENDS ltcl_planet.

CLASS ltcl_planet DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_planet.
    METHODS: should_have_one_field FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_planet IMPLEMENTATION.

  METHOD should_have_one_field.
    cut = NEW #( planet_length = 3
                 planet_width  = 3 ).
    cut->create_surface( ).
    cl_abap_unit_assert=>assert_equals( exp = 9 act = lines( cut->surface ) ).
    ##TODO " Test coordinates
  ENDMETHOD.

ENDCLASS.
