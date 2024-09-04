CLASS zcl_vcd_db_operator_dfkkesr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_vcd_db_operator.

  PRIVATE SECTION.
    TYPES tt_sel_table TYPE zvcd_tt_sel_dfkkesr.

    CONSTANTS structure_name TYPE domvalue VALUE 'DFKKESR'.

    DATA db_cursor           TYPE cursor.
    DATA counter_db_updated  TYPE int4.
    DATA counter_db_selected TYPE int4.


ENDCLASS.

CLASS zcl_vcd_db_operator_dfkkesr IMPLEMENTATION.

  METHOD zif_vcd_db_operator~open_cursor.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : OPEN_CURSOR
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Setze den Cursor für den DB Select auf die Tabelle DFKKESR
*
* Testcase: Über OPBEL der DFKKOP werden die RefNr für DFKKESR erstellt
*           d.h. in Reportselektion die DFKKOP-OPBEL angeben
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    TYPES: BEGIN OF ts_refnr,
             refnr TYPE refnr_kk,
           END OF ts_refnr.
    DATA refnr_list TYPE STANDARD TABLE OF ts_refnr.

    " Einschränkung für Test über OPBEL möglich
    IF opbel_range_table IS NOT INITIAL OR
       budat_range_table IS NOT INITIAL.
      " Selektiere DFKKOP, um die RefNummern für den DB Select auf DFKKESR zu erstellen
      SELECT opbel, opupk, opupw, opupz FROM dfkkop INTO TABLE @DATA(dfkkop_list) WHERE opbel IN @opbel_range_table
                                                                                  AND   budat IN @budat_range_table.
      refnr_list = VALUE #( FOR <line> IN dfkkop_list:
                             ( refnr = |{ <line>-opbel }{ <line>-opupw }{ <line>-opupk }{ <line>-opupz }| ) ).

      OPEN CURSOR WITH HOLD @db_cursor
        FOR SELECT (zcl_vcd_tools=>ov_sel_dfkkesr)
              FROM dfkkesr
              FOR ALL ENTRIES IN @refnr_list
              WHERE refnr = @refnr_list-refnr.     "#EC "#EC CI_NOFIRST
      " Full DB Select
    ELSE.
      OPEN CURSOR WITH HOLD @db_cursor
        FOR SELECT (zcl_vcd_tools=>ov_sel_dfkkesr)
              FROM dfkkesr.        "#EC "#EC CI_NOFIRST "#EC CI_NOWHERE
    ENDIF.
  ENDMETHOD.

  METHOD zif_vcd_db_operator~update_db.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : UPDATE_DB
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Update der DB mit eingeschränkten Update Parametern
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    FIELD-SYMBOLS <values> TYPE tt_sel_table.

    ASSIGN value_list->* TO <values>.

    LOOP AT <values> INTO DATA(ls_dfkkesr).
      UPDATE dfkkesr SET (zcl_vcd_tools=>ot_upd_dfkkesr)
        WHERE reftp = @ls_dfkkesr-reftp
        AND   refnr = @ls_dfkkesr-refnr.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH structure_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_vcd_db_operator~move_correspond_source2target.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : MOVE_CORRESPOND_SOURCE2TARGET
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Mappt die anonymisierten Werte zurück in die DB Selektionsliste
* Dabei wird die eventuelle Einschränkung auf bestimmte Felder durch den
* User berücksichtigt
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    FIELD-SYMBOLS <values> TYPE tt_sel_table.

    result = target_list.
    ASSIGN result->* TO <values>.

    DATA(wa_source_list) = source_list.
    SORT wa_source_list BY strukt reftp refnr.

    LOOP AT <values> ASSIGNING FIELD-SYMBOL(<line>).
      READ TABLE wa_source_list ASSIGNING FIELD-SYMBOL(<names_gp>) WITH KEY strukt = structure_name
                                                                            reftp = <line>-reftp
                                                                            refnr = <line>-refnr BINARY SEARCH.
      IF sy-subrc = 0.
        zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src   = <names_gp>-fields
                                                              ir_selop_range = fields_range_table " Ggf. Einschränkung durch User beachten
                                                              iv_not_inital  = abap_false
                                                    CHANGING  cr_struc_dst   = <line> ).
        counter_db_updated = counter_db_updated + 1. " Merke Anzahl für Protokoll
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_vcd_db_operator~fetch_next_package.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : FETCH_NEXT_PACKAGE
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Liefert für den gesetzten Cursor das nächste Data Package
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    FIELD-SYMBOLS: <values> TYPE table.

    CREATE DATA result TYPE tt_sel_table.
    ASSIGN result->* TO <values>.

    FETCH NEXT CURSOR @db_cursor INTO TABLE @<values> PACKAGE SIZE @package_size.
    " Merke Anzahl für Protokoll
    counter_db_selected = counter_db_selected + lines( <values> ).
  ENDMETHOD.

  METHOD zif_vcd_db_operator~generate_spec_rule_list.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : GENERATE_SPEC_RULE_LIST
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Generiere Liste von Feldern, die von Anonymisierung ausgeschlossen
* oder gelöscht werden sollen
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    " Post Code nicht anonymisieren
    result = VALUE #( ( fieldname = 'POST_CODE1' rule = 'Y' ) ).
  ENDMETHOD.

  METHOD zif_vcd_db_operator~get_counter_selected.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : GET_COUNTER_SELECTED
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Liefert die Anzahl der selektierten DB Einträge zurück
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    result = counter_db_selected.
  ENDMETHOD.

  METHOD zif_vcd_db_operator~get_counter_updated.
***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Methodenname    : GET_COUNTER_UPDATED
* Klasse          : ZCL_VCD_DB_OPERATOR_DFKKESR
* Spezifikation   : Bug 659905
* Programmtyp     : Methode
* Autor           : Martin Jonen / MJON
* Erstellungsdatum: 30.07.2024
************************************************************************
* Kurze Funktionsbeschreibung
*
* Liefert die Anzahl der anonymisierten und upgedateten DB Einträge zurück
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
*
************************************************************************
    result = counter_db_updated.
  ENDMETHOD.

ENDCLASS.
