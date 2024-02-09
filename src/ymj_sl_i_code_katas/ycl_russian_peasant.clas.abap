CLASS ycl_russian_peasant DEFINITION
  PUBLIC
  CREATE PUBLIC .


  PUBLIC SECTION.
    INTERFACES yif_multiplication.
    ALIASES multiplicate FOR yif_multiplication~multiplicate.

  PROTECTED SECTION.
    TYPES: BEGIN OF ts_numbers,
             column_left  TYPE int4,
             column_right TYPE int4,
           END OF ts_numbers.
    TYPES: tt_numbers TYPE STANDARD TABLE OF ts_numbers WITH NON-UNIQUE KEY column_left.

    METHODS delete_not_relevant_numbers RETURNING VALUE(result) TYPE tt_numbers.

    METHODS is_left_number_dividable_by_2 IMPORTING iv_number        TYPE int4
                                          RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS calculate_result              IMPORTING it_numbers       TYPE tt_numbers
                                          RETURNING VALUE(rv_result) TYPE int4.

    METHODS calculate_left_number         IMPORTING iv_number        TYPE int4
                                          RETURNING VALUE(rv_result) TYPE int4.

    METHODS round_off_number              IMPORTING iv_number        TYPE decfloat16
                                          RETURNING VALUE(rv_result) TYPE int4.

    METHODS add_numbers_table             RAISING ycx_russian_peasant_multipl.

    METHODS continue_algorithm            IMPORTING iv_number        TYPE int4
                                          RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS create_1th_line_numbers_table IMPORTING iv_number_one TYPE int4
                                                    iv_number_two TYPE int4.

    METHODS check_input_numbers_are_valid IMPORTING iv_number_one TYPE int4
                                                    iv_number_two TYPE int4
                                          RAISING   ycx_russian_peasant_multipl.

  PRIVATE SECTION.
    DATA: numbers TYPE tt_numbers.

    METHODS calculate_right_number        IMPORTING iv_number        TYPE int4
                                          RETURNING VALUE(rv_result) TYPE int4
                                          RAISING
                                                    ycx_russian_peasant_multipl.

ENDCLASS.

CLASS ycl_russian_peasant IMPLEMENTATION.

  METHOD multiplicate.
    check_input_numbers_are_valid(
      iv_number_one = number_one
      iv_number_two = number_two ).

    create_1th_line_numbers_table( iv_number_one = number_one
                                   iv_number_two = number_two ).

    add_numbers_table( ).

    rv_result = calculate_result( delete_not_relevant_numbers( ) ).

  ENDMETHOD.

  METHOD delete_not_relevant_numbers.
    DATA:
      lv_tabix   TYPE sy-tabix.

    result = numbers.
    LOOP AT result INTO DATA(ls_numbers).
      lv_tabix = sy-tabix.
      CHECK is_left_number_dividable_by_2( ls_numbers-column_left ) = abap_true.
      DELETE result INDEX lv_tabix.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_left_number_dividable_by_2.
    IF iv_number MOD 2 = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_result.
    DATA:
      ls_numbers TYPE ts_numbers.

    LOOP AT it_numbers INTO ls_numbers.
      rv_result = rv_result + ls_numbers-column_right.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_left_number.
    rv_result          = round_off_number( ( iv_number / 2 ) ).
  ENDMETHOD.

  METHOD round_off_number.
    rv_result = floor( iv_number ).
  ENDMETHOD.

  METHOD add_numbers_table.
    LOOP AT numbers INTO DATA(ls_numbers).
      CHECK continue_algorithm( ls_numbers-column_left ) = abap_true.
      APPEND VALUE ts_numbers( column_left  = calculate_left_number( ls_numbers-column_left )
                               column_right = calculate_right_number( ls_numbers-column_right ) ) TO numbers.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_right_number.
    DATA:
      lv_msg TYPE string.
    TRY.
        rv_result = iv_number * 2.
      CATCH cx_root.
        MESSAGE i007(z_coaching_mj) INTO lv_msg.
        RAISE EXCEPTION TYPE ycx_russian_peasant_multipl
          EXPORTING
            message = lv_msg.
    ENDTRY.
  ENDMETHOD.

  METHOD continue_algorithm.
    IF iv_number > 1.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD create_1th_line_numbers_table.
    APPEND VALUE ts_numbers( column_left  = iv_number_one
                             column_right = iv_number_two ) TO numbers.
  ENDMETHOD.

  METHOD check_input_numbers_are_valid.

    DATA:
      lv_message TYPE string.

    IF iv_number_one <= 0.
      MESSAGE i006(ymj_coaching_msg) INTO lv_message WITH 'Links'.
      RAISE EXCEPTION TYPE ycx_russian_peasant_multipl
        EXPORTING
          message = lv_message.
    ENDIF.

    IF iv_number_two <= 0.
      MESSAGE i006(ymj_coaching_msg) INTO lv_message WITH 'Rechts'.
      RAISE EXCEPTION TYPE ycx_russian_peasant_multipl
        EXPORTING
          message = lv_message.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
