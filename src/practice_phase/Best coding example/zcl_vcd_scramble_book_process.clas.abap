CLASS zcl_vcd_scramble_book_process DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING hash_algo_values TYPE zcl_vcd_scramble_bookings=>ts_hash_algo_values
                                   appl_log         TYPE REF TO zvcd_if_anwendungs_log
                                   db_operator      TYPE REF TO zif_vcd_db_operator OPTIONAL
                         RETURNING VALUE(result)    TYPE REF TO zcl_vcd_scramble_book_process
                         RAISING   zcx_vcd_appl_error.

    METHODS constructor IMPORTING hash_algo_values TYPE zcl_vcd_scramble_bookings=>ts_hash_algo_values
                                  appl_log         TYPE REF TO zvcd_if_anwendungs_log
                                  db_operator      TYPE REF TO zif_vcd_db_operator.

    "! Führt den Anonymisierungsprozess abhängig vom injizierten DB Operator aus
    "! @raising zcx_vcd_appl_error | Exception Class
    METHODS process RAISING   zcx_vcd_appl_error.

    "! Liefert die aufbereitete ALV Liste mit den DB selektierten und anonymisierten Daten
    "! @parameter result | ALV Liste
    METHODS get_alv_values RETURNING VALUE(result) TYPE zvcd_scrambl_fields_book_alv_t.

  PRIVATE SECTION.
    CONSTANTS db_operator_name_prefix       TYPE classname VALUE 'ZCL_VCD_DB_OPERATOR_'.
    CONSTANTS initial_reported_package_size TYPE i VALUE 100000.

    DATA db_operator      TYPE REF TO zif_vcd_db_operator.
    DATA appl_log         TYPE REF TO zvcd_if_anwendungs_log.

    DATA hash_algo_values           TYPE zcl_vcd_scramble_bookings=>ts_hash_algo_values.
    DATA counter_selected_positions TYPE i.
    DATA count_update_done          TYPE i.
    DATA alv_list                   TYPE zvcd_scrambl_fields_book_alv_t .
    DATA reported_package_size TYPE zcl_vcd_scramble_bookings=>ts_hash_algo_values-reported_package_size.

    CLASS-METHODS create_new_instance IMPORTING structure_name TYPE domvalue
                                      RETURNING VALUE(result)  TYPE REF TO zif_vcd_db_operator
                                      RAISING   zcx_vcd_appl_error.

    METHODS map_corresponding IMPORTING value_list    TYPE zvcd_scramble_fields_t
                                        db_value_list TYPE REF TO data
                              RETURNING VALUE(result) TYPE REF TO data
                              RAISING
                                        zcx_vcd_appl_error.
    METHODS create_field_list IMPORTING db_value_list      TYPE table
                              RETURNING VALUE(rt_names_gp) TYPE zvcd_scramble_fields_t.
    METHODS scramble_fields   IMPORTING field_list    TYPE zvcd_scramble_fields_t
                              RETURNING VALUE(result) TYPE zvcd_scramble_fields_t
                              RAISING   zcx_vcd_appl_error.
    METHODS open_cursor_db    IMPORTING opbel_range_table TYPE zvcd_rt_opbel
                                        budat_range_table TYPE zvcd_rt_budat
                              RAISING
                                        zcx_vcd_appl_error.
    METHODS fetch_next_package RETURNING VALUE(result) TYPE REF TO data.
    METHODS update_db          IMPORTING scrambled_value_list TYPE REF TO data
                               RAISING
                                         zcx_vcd_appl_error.
    METHODS create_line_names_gp IMPORTING line          TYPE any
                                 RETURNING VALUE(result) TYPE zvcd_scramble_fields.
    METHODS scramble_values IMPORTING db_value_list TYPE REF TO data
                            RETURNING VALUE(result) TYPE REF TO data
                            RAISING   zcx_vcd_appl_error.
    METHODS generate_spec_rule_list RETURNING VALUE(result) TYPE zcl_vcd_tools=>tt_spec_rule.
    METHODS create_sql_set_for_scramble RAISING zcx_vcd_appl_error.
    METHODS count_selected_positions IMPORTING value_list TYPE REF TO data.
    METHODS write_appl_log RAISING zcx_vcd_appl_error.
    METHODS is_db_response_initial IMPORTING db_value_list TYPE REF TO data
                                   RETURNING VALUE(result) TYPE abap_bool.
    METHODS add2alv_list           IMPORTING value_list TYPE REF TO data
                                             linecolor  TYPE lvc_col.
    METHODS write_package_info2job_log.
    METHODS commit_work
      RAISING
        zcx_vcd_appl_error.
    METHODS write_table_info2job_log.
    METHODS map_key_values
      IMPORTING
        line          TYPE any
      RETURNING
        VALUE(result) TYPE zvcd_scramble_fields-key.
    METHODS determine_table_key_fields
      RETURNING
        VALUE(rt_key_field) TYPE ddfields.

ENDCLASS.



CLASS zcl_vcd_scramble_book_process IMPLEMENTATION.


  METHOD add2alv_list.
    FIELD-SYMBOLS <value_list> TYPE table.

    CHECK hash_algo_values-show_alv = abap_true. " Nur merken, wenn ALV Anzeige gewünscht.
    ASSIGN value_list->* TO <value_list>.

    " Mappe Werte für die ALV Anzeige
    LOOP AT <value_list> ASSIGNING FIELD-SYMBOL(<line>).
      APPEND INITIAL LINE TO alv_list ASSIGNING FIELD-SYMBOL(<alv_line>).
      <alv_line> = CORRESPONDING #( <line> ).
      <alv_line>-strukt = hash_algo_values-structure_name.
      <alv_line>-color = VALUE #( ( color = VALUE #( col = linecolor int = 0 inv = 0 ) ) ). " Setze Farbe unterschiedlich zwischen DB und Scrambled Values
    ENDLOOP.
  ENDMETHOD.


  METHOD commit_work.
    TRY.
        IF hash_algo_values-testrun = abap_false.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
      CATCH cx_sql_exception INTO DATA(exception).
        RAISE EXCEPTION TYPE zcx_vcd_appl_error
          EXPORTING
            previous = exception.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    me->hash_algo_values      = hash_algo_values.
    me->appl_log              = appl_log.
    me->db_operator           = db_operator.
    me->reported_package_size = hash_algo_values-reported_package_size.
  ENDMETHOD.

  METHOD create.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : CREATE
* Klasse          : ZCL_VCD_SCRAMBLE_BOOK_PROCESS
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Erzeugt eine Instanz der Klasse ZCL_VCD_SCRAMBLE_BOOK_PROCESS für den
* Prozess des DB Updates für die über den Parameter hash_algo_values-
* structure_name importierte DB Tabelle
* Durch Übergabe der jeweiligen DB Operator Klasse wird
* die Prozesslogik für die DB Tabelle ausgeführt.
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    result = NEW zcl_vcd_scramble_book_process(
                    hash_algo_values = VALUE #( BASE hash_algo_values
                                                structure_name = hash_algo_values-structure_name )
                    appl_log         = appl_log
                    db_operator      = COND #( WHEN db_operator IS BOUND THEN db_operator
                                               ELSE create_new_instance( hash_algo_values-structure_name ) ) ).
  ENDMETHOD.

  METHOD process.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : PROCESS
* Klasse          : ZCL_VCD_SCRAMBLE_BOOK_PROCESS
* Spezifikation   : Bug 65990
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Prozessiert die Anonymisiserung in Abhängigkeit vom injizierten DB
* Ooperator für die einzelnen DB Tabellen
* Z.B. Wird der DB Operator für DFKKOP (ZCL_VCD_DB_OPERATOR_DFKKOP) über
* den Constructor injiziert, wird die Logik mit den Daten der DFKKOP
* durchlaufen und so die Anonymisierung der Felder der DFKKOP durchgeführt
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    " Info ins Job_log schreiben, welche Tabelleaktualisiert wird
    write_table_info2job_log( ).
    " Erzeuge die Selection und Update Expressions für den spezifischen
    " Select und Update Zugriff auf die DB Tabelle
    create_sql_set_for_scramble( ).

    " Eröffne den Cursor via DB Select
    open_cursor_db( opbel_range_table = hash_algo_values-opbel_range_table
                    budat_range_table = hash_algo_values-budat_range_table ).

    DO.
      " Solange wir Daten erhalten (FETCH DB), läuft der Anonymisierungsprozess
      DATA(db_value_list) = fetch_next_package( ).
      IF is_db_response_initial( db_value_list ).
        EXIT.
      ENDIF.
      " Merken der DB Selektion für spätere eventuelle ALV Anzeige
      add2alv_list( value_list = db_value_list
                    linecolor  = 1 ). " Standard Farbe
      " Führe die Anonymisierung durch
      DATA(scrambled_value_list) = scramble_values( db_value_list ).
      " DB Update (auch im Testfall; später ROLLBACK)
      update_db( scrambled_value_list ).

      " Merken der Anonymisierten Werte für spätere eventuelle ALV Anzeige
      add2alv_list( value_list = scrambled_value_list
                    linecolor  = 5 ). " Farbe Grün

      write_package_info2job_log( ).
    ENDDO.
    " Schreibe Protokoll-Einträge zu DB Update-Prozess
    write_appl_log( ).
  ENDMETHOD.

  METHOD count_selected_positions.
    " Zähle Anzahl der Selektierten Werte für Protokoll
    FIELD-SYMBOLS <value_list> TYPE table.
    ASSIGN value_list->* TO <value_list>.
    IF <value_list> IS ASSIGNED.
      counter_selected_positions = counter_selected_positions + lines( <value_list> ).
    ENDIF.
  ENDMETHOD.

  METHOD create_field_list.
    " Erzeuge Liste der Werte für den Aufruf der Scramble Routine
    rt_names_gp = VALUE #( FOR <line> IN db_value_list:
                             ( create_line_names_gp( <line> ) ) ).
  ENDMETHOD.


  METHOD create_line_names_gp.
    result         = CORRESPONDING zvcd_scramble_fields( line ).
    result-key     = map_key_values( line ).
    result-strukt  = hash_algo_values-structure_name.
    result-fields  = CORRESPONDING #( line ).
  ENDMETHOD.


  METHOD create_new_instance.
    TRY.
        DATA(db_operator_name) = |{ db_operator_name_prefix }{ condense( COND #( WHEN structure_name = 'ZVCD_SVVSCPOS' THEN 'ZVVSCPOS'
                                                                                 ELSE structure_name ) ) }|.
        CREATE OBJECT result TYPE (db_operator_name).
      CATCH cx_sy_dyn_call_illegal_class.
        RAISE EXCEPTION TYPE zcx_vcd_appl_error
          MESSAGE e189(zvcd2) WITH condense( structure_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_sql_set_for_scramble.
    zcl_vcd_tools=>create_sql_sets_for_scramble( ).
  ENDMETHOD.


  METHOD fetch_next_package.
    result = db_operator->fetch_next_package( hash_algo_values-package_size ).
    count_selected_positions( result ).
  ENDMETHOD.


  METHOD generate_spec_rule_list.
    " Erzeuge, falls nötig eine Liste der Felder welche von Prozess ausgeschlossen  oder
    " gelöscht werden sollen
    result = db_operator->generate_spec_rule_list( ).
  ENDMETHOD.


  METHOD get_alv_values.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : GET_ALV_VALUES
* Klasse          : ZCL_VCD_SCRAMBLE_BOOK_PROCESS
* Spezifikation   : Bug 65990
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Liefert die erzeugte ALV Liste mit den DB selektierten und anonymisierten
* Werten zurück
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    result = alv_list.
  ENDMETHOD.


  METHOD is_db_response_initial.
    FIELD-SYMBOLS <value_list> TYPE table.
    ASSIGN db_value_list->* TO <value_list>.

    result = xsdbool( <value_list> IS INITIAL ).
  ENDMETHOD.


  METHOD map_corresponding.
    " Mappe Werte zurück in DB Selektion für späteren DB Update und zählen der
    " Anzahl der upgedateten Einträge
    result = db_operator->move_correspond_source2target( source_list        = value_list
                                                         target_list        = db_value_list
                                                         fields_range_table = hash_algo_values-fields_range_table ). " Ggf. durch User eingeschränkte Feldliste beachten

  ENDMETHOD.

  METHOD map_key_values.
    LOOP AT determine_table_key_fields( ) INTO DATA(field).
      ASSIGN COMPONENT field-fieldname OF STRUCTURE line TO FIELD-SYMBOL(<value>).
      " Mappe Key Values als String für eventuell Fehlermeldung beim Scramble Prozess
      result = COND #( WHEN result IS INITIAL THEN |{ <value> }|
                       ELSE condense( |{ result } / { <value> }| ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_table_key_fields.
    DATA(rtti_struct) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( hash_algo_values-structure_name ) ).

    rt_key_field  = VALUE ddfields( FOR <field> IN rtti_struct->get_ddic_field_list( ) WHERE ( keyflag = 'X ' AND fieldname <> 'MANDT' AND fieldname <> 'CLNT' )
                                     ( fieldname = <field>-fieldname ) ).
  ENDMETHOD.

  METHOD open_cursor_db.
    db_operator->open_cursor( opbel_range_table = opbel_range_table
                              budat_range_table = budat_range_table ).

  ENDMETHOD.

  METHOD scramble_fields.
    " Ausführen des Scramble Process
    result = zcl_vcd_tools=>scramble_gp_fields( it_daten     = field_list
                                                iv_salt      = hash_algo_values-hashsalt
                                                iv_algo      = hash_algo_values-hashalgo
                                                it_spec_rule = generate_spec_rule_list( ) " Spezifisch: ggf. Felder ausschliessen/löschen
                                                iv_codepage  = hash_algo_values-codepage
                                                ir_appl_log  = CAST #( appl_log ) ).
  ENDMETHOD.


  METHOD scramble_values.
    " Dynamische Value Übergabe
    ASSIGN db_value_list->* TO FIELD-SYMBOL(<db_value_list>).

    DATA(scramble_field_list) = create_field_list( <db_value_list> ).
    DATA(scrambled_values)    = scramble_fields( scramble_field_list ).
    result                    = map_corresponding( value_list    = scrambled_values
                                                   db_value_list = db_value_list ).
  ENDMETHOD.


  METHOD update_db.
    db_operator->update_db( value_list = scrambled_value_list
                            testrun    = hash_algo_values-testrun ).
    commit_work( ).
  ENDMETHOD.


  METHOD write_appl_log.
    " Schreibe Protokoll
    " Trenner |------------------------------------------------------------------------
    MESSAGE i997(>2) INTO DATA(lv_dummy).
    appl_log->add_message( zif_vcd_constants=>problem_class-high ).

    " Anzahl Belegpositionen (&1) selektiert: &2
    MESSAGE s187(zvcd2) INTO lv_dummy WITH hash_algo_values-structure_name condense( CONV string( db_operator->get_counter_selected( ) ) ).
    appl_log->add_message( zif_vcd_constants=>problem_class-high ).

    " Anzahl Belegpositionen (&1) aktualisiert: &2
    MESSAGE s188(zvcd2) INTO lv_dummy WITH hash_algo_values-structure_name condense( CONV string( db_operator->get_counter_updated( ) ) ).
    appl_log->add_message( zif_vcd_constants=>problem_class-high ).
  ENDMETHOD.


  METHOD write_package_info2job_log.
    CHECK sy-batch = abap_true.
    IF me->counter_selected_positions >= me->reported_package_size. " Package finished?
      " Nachricht: 'Verarbeitung abgeschlossen: &1 Records'
      MESSAGE i191(zvcd2) WITH me->reported_package_size.
      me->reported_package_size = me->reported_package_size + me->hash_algo_values-reported_package_size. " Next Package border
    ENDIF.
  ENDMETHOD.


  METHOD write_table_info2job_log.
    CHECK sy-batch = abap_true.
    MESSAGE i192(zvcd2) WITH me->hash_algo_values-structure_name.
  ENDMETHOD.
ENDCLASS.
