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
      should_have_sequence_list FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_marsrover IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( cardinal_point = yif_marsrover=>north
                 sequence_list  = VALUE yif_sequence=>tt_list( ( direction = yif_sequence=>left ) ) ).
  ENDMETHOD.

  METHOD should_have_cardinal_poin.
    cl_abap_unit_assert=>assert_equals( exp = yif_marsrover=>north act = cut->cardinal_point ).
  ENDMETHOD.

  METHOD should_have_sequence_list.
    cl_abap_unit_assert=>assert_equals( exp = VALUE yif_sequence=>tt_list( ( direction = yif_sequence=>left ) )
                                        act = cut->sequence_list ).
  ENDMETHOD.


ENDCLASS.
