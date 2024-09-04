CLASS zcl_vcd_scramble_bookings DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_fields_range_table TYPE RANGE OF name_feld.
    TYPES: BEGIN OF ts_hash_algo_values,
             hashsalt              TYPE string,
             hashalgo              TYPE string,
             codepage              TYPE cpcodepage,
             fields_range_table    TYPE tt_fields_range_table,
             package_size          TYPE i,
             structure_name        TYPE domvalue,
             opbel_range_table     TYPE zvcd_rt_opbel,
             budat_range_table     TYPE zvcd_rt_budat,
             table_list            TYPE string_t,
             show_alv              TYPE abap_bool,
             layout_variant        TYPE slis_vari,
             testrun               TYPE flag,
             reported_package_size TYPE i,
           END OF ts_hash_algo_values.
    TYPES tt_scramble_book_single TYPE STANDARD TABLE OF REF TO zcl_vcd_scramble_book_process WITH DEFAULT KEY.

    METHODS constructor IMPORTING hash_algo_values TYPE ts_hash_algo_values
                                  appl_log         TYPE REF TO zvcd_if_anwendungs_log.

    "! Hauptmethode für Anonymisierung Buchungsdaten
    "! @raising zcx_vcd_appl_error |
    METHODS process RAISING zcx_vcd_appl_error.
    "! Zeige aufbereitete Daten in ALV Liste
    METHODS show_alv RAISING zcx_vcd_appl_error.

  PRIVATE SECTION.
    CONSTANTS process_class_prefix TYPE classname VALUE 'ZCL_VCD_SCRAMBLE_BOOK_'.

    DATA appl_log         TYPE REF TO zvcd_if_anwendungs_log.
    DATA hash_algo_values TYPE ts_hash_algo_values.
    DATA booking_single_list TYPE zcl_vcd_scramble_bookings=>tt_scramble_book_single.

    METHODS create_process_class_list RETURNING VALUE(result) TYPE tt_scramble_book_single
                                      RAISING   zcx_vcd_appl_error.
    METHODS show_alv_list             IMPORTING alv_list TYPE zvcd_scrambl_fields_book_alv_t
                                      RAISING   zcx_vcd_appl_error.
    METHODS aggregate_alv_list        RETURNING VALUE(result) TYPE zvcd_scrambl_fields_book_alv_t.
    METHODS activate_all_functions    IMPORTING alv TYPE REF TO cl_salv_table.
    METHODS set_optimize_column_width IMPORTING alv TYPE REF TO cl_salv_table.
    METHODS set_color_column          IMPORTING alv TYPE REF TO cl_salv_table
                                      RAISING   cx_salv_data_error.
    METHODS enable_layout_functionality IMPORTING alv TYPE REF TO cl_salv_table.
    METHODS set_technical_column_names  IMPORTING alv TYPE REF TO cl_salv_table.

ENDCLASS.



CLASS zcl_vcd_scramble_bookings IMPLEMENTATION.


  METHOD activate_all_functions.
    DATA(lo_functions) = alv->get_functions( ).
    lo_functions->set_all( abap_true ).
  ENDMETHOD.


  METHOD aggregate_alv_list.
    " Liest die aufbereiteten Daten aus den erzeugten Instanzen für den DB Update
    " und hängt diese für die Anzeige in der ALV Liste an
    LOOP AT booking_single_list INTO DATA(object).
      APPEND LINES OF object->get_alv_values( ) TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    me->hash_algo_values = hash_algo_values.
    me->appl_log         = appl_log.
  ENDMETHOD.


  METHOD create_process_class_list.
    " Dynamischer Aufruf der Create Instance Methoden der DB Update Klassen
    " z.B. ZCL_VCD_SCRAMBLE_BOOK_DFKKOP
    LOOP AT hash_algo_values-table_list INTO DATA(table).
      TRY.
          DATA(object) = zcl_vcd_scramble_book_process=>create( hash_algo_values = VALUE #( BASE hash_algo_values
                                                                                            structure_name = table )
                                                                appl_log         = appl_log ).
          APPEND object TO result.
        CATCH cx_sy_dyn_call_illegal_class.
          RAISE EXCEPTION TYPE zcx_vcd_appl_error
            MESSAGE e189(zvcd2) WITH condense( table ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD enable_layout_functionality.
    DATA(layout) = alv->get_layout( ).
    layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ). " Announce report
    layout->set_save_restriction( if_salv_c_layout=>restrict_none ). " No restriction in saving layouts
    layout->set_initial_layout( hash_algo_values-layout_variant ).   " Set initial layout by user input
  ENDMETHOD.


  METHOD process.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : PROCESS
* Klasse          : ZCL_VCD_SCRAMBLE_BOOKINGS
* Spezifikation   : Bug 659905 - Anonymisierung Buchungsdaten
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Hauptroutine für den Anonymisierungsprozess
*   - Erzeugt die Objekte für die einzelnen DB Tabellen Updates, die über
*     den Parameter hash_algo_values-table_list durch den User vorgegeben
*     wurden
*   - Führt die Process Methoden dieses erzeugten Klasseninstanzen aus
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    " Erzeuge Liste der relevanten DB Updateobjekte
    booking_single_list = create_process_class_list( ).

    " Für alle erzeugten DB Updateobjekte für den Prozess via Methode
    " Process aus (Command Pattern)
    LOOP AT booking_single_list INTO DATA(object).
      object->process( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD set_color_column.
    alv->get_columns( )->set_color_column( 'COLOR' ).
  ENDMETHOD.


  METHOD set_optimize_column_width.
    DATA(columns) = alv->get_columns( ).
    columns->set_optimize( 'X' ).
  ENDMETHOD.


  METHOD set_technical_column_names.
    " Setze die technischen Feldnamen als Überschrift in Tabellen Header
    LOOP AT alv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<column>).
      <column>-r_column->set_short_text( |{ <column>-r_column->get_columnname( ) }| ).
      <column>-r_column->set_medium_text( |{ <column>-r_column->get_columnname( ) }| ).
      <column>-r_column->set_long_text( |{ <column>-r_column->get_columnname( ) }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD show_alv.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : SHOW_ALV
* Klasse          : ZCL_VCD_SCRAMBLE_BOOKINGS
* Spezifikation   : Bug 659905 - Anonymisierung Buchungsdaten
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Zeigt die aufbereiteten Daten in einer ALV Liste an.
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    IF hash_algo_values-show_alv = abap_true.
      show_alv_list( aggregate_alv_list( ) ).
    ENDIF.
  ENDMETHOD.


  METHOD show_alv_list.
    DATA alv TYPE REF TO cl_salv_table.

    DATA(wa_alv_list) = alv_list.
    TRY.
        " Erzeuge ALV Instanz
        cl_salv_table=>factory( IMPORTING r_salv_table = alv
                                CHANGING  t_table      = wa_alv_list ).

        activate_all_functions( alv ).
        set_optimize_column_width( alv ).
        set_color_column( alv ).
        enable_layout_functionality( alv ).
        set_technical_column_names( alv ).

        alv->display( ).
      CATCH cx_salv_msg cx_salv_data_error.
        RAISE EXCEPTION TYPE zcx_vcd_appl_error
          MESSAGE ID 'ZVCD2' TYPE 'E' NUMBER 182.        "#EC NUMBER_OK
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
