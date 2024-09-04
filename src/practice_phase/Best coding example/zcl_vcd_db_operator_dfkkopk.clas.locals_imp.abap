CLASS ltd_db_operator DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_vcd_db_operator.

  PRIVATE SECTION.
    DATA call_already_done TYPE abap_bool.

ENDCLASS.

CLASS ltd_db_operator IMPLEMENTATION.

  METHOD zif_vcd_db_operator~fetch_next_package.
    " Only for first round -> Simulate DB value selection of all relevant fields for scramble process
    IF call_already_done = abap_false.
      CREATE DATA result TYPE zvcd_tt_sel_dfkkopk.
      ASSIGN result->* TO FIELD-SYMBOL(<values>).
      <values> = VALUE zvcd_tt_sel_dfkkopk( ( opbel = '1'
                                              opupk = '1'
                                              zzv_name_firs_ko = 'Martin' "#EC NOTEXT
                                              zzv_name_last_ko  = 'Jonen'  ) ). "#EC NOTEXT
      call_already_done = abap_true.
      RETURN.
    ENDIF.

    " Second round -> Return empty list like DB select would do
    CREATE DATA result TYPE zvcd_tt_sel_dfkkopk.
    ASSIGN result->* TO <values>.
    <values> = VALUE zvcd_tt_sel_dfkkopk(  ).
  ENDMETHOD.

  METHOD zif_vcd_db_operator~move_correspond_source2target.
    " Execute call like in productive workflow
    result = NEW zcl_vcd_db_operator_dfkkopk( )->zif_vcd_db_operator~move_correspond_source2target(
                                               source_list        = source_list
                                               target_list        = target_list
                                               fields_range_table = fields_range_table
                                             ).
  ENDMETHOD.

  METHOD zif_vcd_db_operator~open_cursor.
    " Nothing to do
  ENDMETHOD.

  METHOD zif_vcd_db_operator~update_db.
    " Check if values were scrambled correctly. This simulates the DB update
    FIELD-SYMBOLS <value_list> TYPE zvcd_tt_sel_dfkkopk.
    ASSIGN value_list->* TO <value_list>.

    " Result for test method all_fields_are_scrambled
    IF <value_list> <> VALUE zvcd_tt_sel_dfkkopk( ( opbel = '1'
                                                    opupk = '1'
                                                    zzv_name_firs_ko = 'accéfe'
                                                    zzv_name_last_ko = 'beddc'
                                                    ) )
    " Result for test method one_fields_are_scrambled
    AND <value_list> <> VALUE zvcd_tt_sel_dfkkopk( ( opbel = '1'
                                                     opupk = '1'
                                                     zzv_name_firs_ko = 'accéfe'
                                                     zzv_name_last_ko  = 'Jonen'
                                                    ) ).
      RAISE EXCEPTION TYPE zcx_vcd_appl_error.
    ENDIF.
  ENDMETHOD.

  METHOD zif_vcd_db_operator~generate_spec_rule_list.
    " Execute call like in productive workflow
    result = NEW zcl_vcd_db_operator_dfkkop( )->zif_vcd_db_operator~generate_spec_rule_list( ).
  ENDMETHOD.

  METHOD zif_vcd_db_operator~get_counter_selected.
    " Nothing to do
  ENDMETHOD.

  METHOD zif_vcd_db_operator~get_counter_updated.
    " Nothing to do
  ENDMETHOD.

ENDCLASS.
