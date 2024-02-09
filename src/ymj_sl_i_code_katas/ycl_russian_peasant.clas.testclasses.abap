CLASS ltcl_russian_multiplication DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO yif_multiplication.

    METHODS: setup,
      check_multiplicate_47_42       FOR TESTING,
      check_input_left_invalid       FOR TESTING,
      check_input_right_invalid      FOR TESTING,
      check_input_overload           FOR TESTING,
      check_input_left_max_number    FOR TESTING.
ENDCLASS.

CLASS ltcl_russian_multiplication IMPLEMENTATION.

  METHOD setup.
    cut = NEW ycl_russian_peasant( ).
  ENDMETHOD.

  METHOD check_multiplicate_47_42.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 1974
        act                  = cut->multiplicate( number_one = 47
                                                  number_two = 42 )
    ).
  ENDMETHOD.

  METHOD check_input_left_invalid.
    TRY.
        cut->multiplicate( number_one = 0
                           number_two = 30 ).
      CATCH ycx_russian_peasant_multipl INTO DATA(lo_exc).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            exp                  = 'Zahl Links ist ungültig. Bitte Zahlen grösser 0 eingeben.'
            act                  = lo_exc->get_message( )
        ).
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>fail( msg = 'Test sollte hier nicht landen.' ).
  ENDMETHOD.

  METHOD check_input_right_invalid.
    TRY.
        cut->multiplicate( number_one = 1
                           number_two = 0 ).
      CATCH ycx_russian_peasant_multipl INTO DATA(exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            exp                  = 'Zahl Rechts ist ungültig. Bitte Zahlen grösser 0 eingeben.'
            act                  = exception->get_message(  ) ).
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>fail( msg = 'Test sollte hier nicht landen.' ).
  ENDMETHOD.

  METHOD check_input_overload.
    TRY.
        cut->multiplicate( number_one = 2
                           number_two = 2147483647 ). " 2147483647 grösste Zahl in INT4
      CATCH ycx_russian_peasant_multipl INTO DATA(exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            exp                  = 'Overflow bei Berechnung Rechte Zahl in Numbers-Table.'
            act                  = exception->get_message( )
        ).
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>fail( msg = 'Test sollte hier nicht landen.' ).

  ENDMETHOD.

  METHOD check_input_left_max_number.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 2147483647
        act                  = cut->multiplicate( number_one = 2147483647
                                                  number_two = 1 )
    ).
  ENDMETHOD.
ENDCLASS.
