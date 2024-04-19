CLASS ltcl_sequence DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO yif_sequence.
    METHODS:
      setup,
      build_sequence_list4test RETURNING VALUE(result) TYPE yif_sequence=>tt_list,
      should_have_sequence_list FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_sequence IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_sequence( sequence_list = build_sequence_list4test( ) ).
  ENDMETHOD.

  METHOD should_have_sequence_list.
    cl_abap_unit_assert=>assert_equals( exp = build_sequence_list4test( )
                                        act = cut->read_sequence( ) ).
  ENDMETHOD.

  METHOD build_sequence_list4test.
    result = VALUE yif_sequence=>tt_list( ( direction = yif_sequence=>down )
                                          ( direction = yif_sequence=>up )
                                          ( direction = yif_sequence=>left )
                                          ( direction = yif_sequence=>right ) ).
  ENDMETHOD.

ENDCLASS.
