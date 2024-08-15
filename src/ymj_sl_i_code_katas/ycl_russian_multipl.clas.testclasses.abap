CLASS ltcl_russian_multiplication DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    CONSTANTS:
      cv_class_name TYPE string VALUE 'YCL_RUSSIAN_MULTIPL'.
    DATA:
      go_cut TYPE REF TO yif_russian_peasant_multipl.

    METHODS:
      setup                          FOR TESTING,
      check_multiplicate_1_30        FOR TESTING,
      check_multiplicate_2_30        FOR TESTING,
      check_multiplicate_47_42       FOR TESTING,
      check_input_left_invalid       FOR TESTING,
      check_input_right_invalid      FOR TESTING,
      check_input_overload           FOR TESTING,
      check_input_left_max_number    FOR TESTING,
      check_multiplicate_3456_7898   FOR TESTING.
ENDCLASS.

CLASS ltcl_russian_multiplication IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT go_cut TYPE (cv_class_name).
  ENDMETHOD.

  METHOD check_multiplicate_1_30.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 30
        act                  = go_cut->multiplicate( iv_number_one = 1
                                                     iv_number_two = 30 )
    ).

  ENDMETHOD.

  METHOD check_multiplicate_2_30.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 60
        act                  = go_cut->multiplicate( iv_number_one = 2
                                                     iv_number_two = 30 )
    ).

  ENDMETHOD.

  METHOD check_multiplicate_47_42.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 1974
        act                  = go_cut->multiplicate( iv_number_one = 47
                                                     iv_number_two = 42 )
    ).

  ENDMETHOD.


  METHOD check_input_left_invalid.
    DATA:
      lo_exc     TYPE REF TO ycx_russian_peasant_multipl,
      lv_message TYPE string.
    TRY.
        go_cut->multiplicate( iv_number_one = 0
                              iv_number_two = 30 ).
      CATCH ycx_russian_peasant_multipl INTO lo_exc.
        lv_message = lo_exc->get_message( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Zahl Links ist ungültig. Bitte Zahlen grösser 0 eingeben.'
        act                  = lv_message
    ).
  ENDMETHOD.

  METHOD check_input_right_invalid.
    DATA:
      lo_exc     TYPE REF TO ycx_russian_peasant_multipl,
      lv_message TYPE string.
    TRY.
        go_cut->multiplicate( iv_number_one = 1
                              iv_number_two = 0 ).
      CATCH ycx_russian_peasant_multipl INTO lo_exc.
        lv_message = lo_exc->get_message( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Zahl Rechts ist ungültig. Bitte Zahlen grösser 0 eingeben.'
        act                  = lv_message
    ).
  ENDMETHOD.

  METHOD check_input_overload.
    " 2147483647 grösste Zahl in INT4
    DATA:
      lo_exc     TYPE REF TO ycx_russian_peasant_multipl,
      lv_message TYPE string.
    TRY.
        go_cut->multiplicate( iv_number_one = 2
                              iv_number_two = 2147483647 ).
      CATCH ycx_russian_peasant_multipl INTO lo_exc.
        lv_message = lo_exc->get_message( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 'Overflow bei Berechnung Rechte Zahl in Numbers-Table.'
        act                  = lv_message
    ).
  ENDMETHOD.

  METHOD check_multiplicate_3456_7898.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 27295488
        act                  = go_cut->multiplicate( iv_number_one = 3456
                                                     iv_number_two = 7898 )
    ).
  ENDMETHOD.

  METHOD check_input_left_max_number.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        exp                  = 2147483647
        act                  = go_cut->multiplicate( iv_number_one = 2147483647
                                                     iv_number_two = 1 )
    ).
  ENDMETHOD.
ENDCLASS.
