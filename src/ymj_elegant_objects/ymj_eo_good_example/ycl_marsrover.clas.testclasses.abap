CLASS ltcl_marsrover DEFINITION DEFERRED.
CLASS ycl_marsrover DEFINITION LOCAL FRIENDS ltcl_marsrover.


CLASS ltcl_marsrover DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_marsrover.

    METHODS:
      setup,
      should_have_cardinal_poin FOR TESTING RAISING cx_static_check,
      should_have_sequence_list FOR TESTING RAISING cx_static_check,
      should_have_starting_point FOR TESTING,
      should_find_starting_point FOR TESTING.
ENDCLASS.


CLASS ltcl_marsrover IMPLEMENTATION.

  METHOD setup.
    DATA(planet) = NEW ltd_planet( ).
    cut = NEW #( cardinal_point = yif_marsrover=>north
                 sequence_list  = VALUE yif_sequence=>tt_list( ( direction = yif_sequence=>left ) )
                 starting_point = VALUE yif_planet=>coordinates( coordinate_x = 2 coordinate_y = 1 )
                 planet         = planet  ).
  ENDMETHOD.

  METHOD should_have_cardinal_poin.
    cl_abap_unit_assert=>assert_equals( exp = yif_marsrover=>north act = cut->cardinal_point ).
  ENDMETHOD.

  METHOD should_have_sequence_list.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_sequence=>tt_list( ( direction = yif_sequence=>left ) )
                                        act = cut->sequence_list ).
  ENDMETHOD.


  METHOD should_have_starting_point.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_planet=>coordinates( coordinate_x = 2 coordinate_y = 1 )
                                        act = cut->starting_point ).
  ENDMETHOD.

  METHOD should_find_starting_point.
    cl_abap_unit_assert=>assert_true( xsdbool( cut->find_starting_square( ) IS INSTANCE OF ycl_square ) ).
  ENDMETHOD.

ENDCLASS.
