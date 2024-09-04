INTERFACE zif_vcd_db_operator
  PUBLIC .
  TYPES: tt_fields_range_table TYPE RANGE OF name_feld.

  METHODS open_cursor IMPORTING opbel_range_table TYPE zvcd_rt_opbel
                                budat_range_table TYPE zvcd_rt_budat
                      RAISING   zcx_vcd_appl_error.
  METHODS fetch_next_package IMPORTING package_size  TYPE i
                             RETURNING VALUE(result) TYPE REF TO data.
  METHODS update_db   IMPORTING value_list TYPE REF TO data
                                testrun    TYPE flag
                      RAISING   zcx_vcd_appl_error.
  METHODS move_correspond_source2target IMPORTING source_list        TYPE zvcd_scramble_fields_t
                                                  target_list        TYPE REF TO data
                                                  fields_range_table TYPE tt_fields_range_table
                                        RETURNING VALUE(result)      TYPE REF TO data
                                        RAISING   zcx_vcd_appl_error.
  METHODS generate_spec_rule_list       RETURNING VALUE(result) TYPE zcl_vcd_tools=>tt_spec_rule.
  METHODS get_counter_selected          RETURNING VALUE(result) TYPE int4.
  METHODS get_counter_updated           RETURNING VALUE(result) TYPE int4.
ENDINTERFACE.
