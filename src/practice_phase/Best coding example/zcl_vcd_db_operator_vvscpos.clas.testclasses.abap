CLASS ltcl_scramble_book_vvscpos DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_vcd_scramble_book_process.

    METHODS:
      create_cut4all_fields_scram RETURNING VALUE(result) TYPE REF TO zcl_vcd_scramble_book_process
                                  RAISING   zcx_vcd_appl_error,
      create_cut4one_field_scram  RETURNING VALUE(result) TYPE REF TO zcl_vcd_scramble_book_process
                                  RAISING   zcx_vcd_appl_error,

      " GIVEN: DB entry with values in all relevant fields WHEN: Process is executed THEN: ...
      all_fields_are_scrambled FOR TESTING,
      " GIVEN:  DB entry with values in all relevant fields and ...
      "        ... restricted field NAME1 change
      " WHEN: Process is executed
      " THEN: ...
      name1_is_scrambled FOR TESTING.
ENDCLASS.

CLASS ltcl_scramble_book_vvscpos IMPLEMENTATION.

  METHOD all_fields_are_scrambled.
    " Implicit Check in DB operator method UPDATE_DB
    " Relevant fields:
    " - zzv_name_first, zzv_name_last, zzv_txtvw1-4
    TRY.
        create_cut4all_fields_scram( )->process( ).
      CATCH zcx_vcd_appl_error.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD name1_is_scrambled.
    " Implicit Check in DB operator method UPDATE_DB
    TRY.
        create_cut4one_field_scram( )->process( ).
      CATCH zcx_vcd_appl_error.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_cut4all_fields_scram.
    result = zcl_vcd_scramble_book_process=>create(
          hash_algo_values = VALUE #( hashsalt           = 'zUiFlkWj'
                                      hashalgo           = 'SHA512'
                                      codepage           = '1252'
                                      structure_name     = 'VVSCPOS'
                                      fields_range_table = VALUE #( )
                                      package_size       = 200
                                      opbel_range_table  = VALUE #( )
                                      budat_range_table  = VALUE #( ) )
          appl_log         = NEW zvcd_cl_anwendungs_log( i_object        = 'ZVCD_REPORTS'
                                                         i_subobject     = 'ZVCD_MIG'
                                                         i_extnumber     = ''
                                                         i_prog          = 'ZVCD_SCRAMBLE_BOOKINGS' )
          db_operator      = NEW ltd_db_operator( )
        ).
  ENDMETHOD.

  METHOD create_cut4one_field_scram.
    result = zcl_vcd_scramble_book_process=>create(
          hash_algo_values = VALUE #( hashsalt           = 'zUiFlkWj'
                                      hashalgo           = 'SHA512'
                                      codepage           = '1252'
                                      structure_name     = 'VVSCPOS'
                                      fields_range_table = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZZV_NAME_FIRST' ) )
                                      package_size       = 200
                                      opbel_range_table  = VALUE #( )
                                      budat_range_table  = VALUE #( ) )
          appl_log         = NEW zvcd_cl_anwendungs_log( i_object        = 'ZVCD_REPORTS'
                                                         i_subobject     = 'ZVCD_MIG'
                                                         i_extnumber     = ''
                                                         i_prog          = 'ZVCD_SCRAMBLE_BOOKINGS' )
          db_operator      = NEW ltd_db_operator( )
        ).
  ENDMETHOD.

ENDCLASS.
