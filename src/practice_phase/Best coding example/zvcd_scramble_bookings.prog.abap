***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Programmname    : ZVCD_SCRAMBLE_BOOKINGS
* Spezifikation   : Anonymisierung der GP Daten
* Programmtyp     : Report
* Autor           : Ibrahim Türkyilmaz
* Erstellungsdatum: 23.04.2024
************************************************************************
* Kurze Funktionsbeschreibung
* ----------------
* Programm dient zur Anonymisierung von Buchungsbelegen und basiert
* auf dem Programm ZVCD_SCRAMBLE_GPART
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum       Name  Änderung
* --   30.07.2024  MJON  BUG 659905: Trennung von Geschäftspartnern und
*                        Buchungsbelegen, da der Job immer abbricht
************************************************************************

REPORT  zvcd_scramble_bookings.

INCLUDE zvcd_constants ##INCL_OK.

TABLES:
  dd03d,
  thxet_item_bp,
  dfkkko.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_budat FOR dfkkko-budat.
SELECT-OPTIONS: s_opbel FOR dfkkko-opbel.
SELECT-OPTIONS: s_table FOR dd03d-domname NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-002.
PARAMETERS p_rows TYPE n LENGTH 4 DEFAULT '200' OBLIGATORY.
PARAMETERS p_repsiz TYPE n LENGTH 6 DEFAULT '100000' OBLIGATORY.
PARAMETERS p_test TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_alv  TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_var  TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-004.
PARAMETERS p_salt  TYPE string DEFAULT 'zUiFlkWj' LOWER CASE. "Salt für Hash
PARAMETERS p_algo  TYPE string DEFAULT 'SHA512' OBLIGATORY.   "Hash Algorithmus SHA512
PARAMETERS p_codep TYPE tcp00-cpcodepage DEFAULT '1252'.      "Codepage
SELECT-OPTIONS s_field FOR dd03d-fieldname.
SELECTION-SCREEN END OF BLOCK b5.

* Selektionsblock Protokoll
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-bpr.
PARAMETERS p_extp TYPE emma_extrunid. "Externe ID <= 50
PARAMETERS p_prcl TYPE balprobcl AS LISTBOX VISIBLE LENGTH 21 DEFAULT '2'.
PARAMETERS p_vdat TYPE aldate_del.
PARAMETERS p_dbef TYPE del_before DEFAULT c_true.
PARAMETERS p_lgds TYPE c AS CHECKBOX DEFAULT c_true.
PARAMETERS p_lgsa TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b6.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*

  p_vdat = sy-datum + 180 ##NUMBER_OK.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM f4_layout CHANGING p_var.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  TRY.
      " Erzeuge Appl Log Object
      DATA(go_app_log) = NEW zvcd_cl_anwendungs_log( i_object        = c_object_rept
                          i_subobject     = c_subobj_mig
                          i_extnumber     = CONV #( p_extp )
                          i_prog          = sy-cprog
                          i_desired_class = p_prcl
                          i_aldate_del    = p_vdat
                          i_del_before    = p_dbef ).       "#EC NEEDED

      go_app_log->add_header( p_test ).

      " Verarbeitungsart "Standard mit eigener Selektion" ausgeben
      MESSAGE s000(zvcd) WITH TEXT-006 INTO DATA(lv_dummy). "#EC NEEDED
      go_app_log->add_message( i_probclass = c_probclass_high ).

      " Programm darf mit dieser Verarbeitungsart nicht auf PK1 ausgeführt werden
      IF sy-sysid(1) EQ 'P' AND p_test IS INITIAL.
        MESSAGE e086(zvcd2) WITH sy-repid TEXT-029 INTO lv_dummy.
        "Programm &1 mit Verarbeitungsart &2 darf auf PK1 nicht ausgeführt werden!
        zcx_vcd_appl_error=>raise_ex( ).
      ENDIF.

      " Prozessiere Anonymisierung
      DATA(scramble_bookings) = NEW zcl_vcd_scramble_bookings(
                                     hash_algo_values = VALUE #( hashalgo = p_algo
                                                                 hashsalt = p_salt
                                                                 codepage = p_codep
                                                                 fields_range_table = s_field[]
                                                                 package_size       = p_rows
                                                                 opbel_range_table  = s_opbel[]
                                                                 budat_range_table  = s_budat[]
                                                                 show_alv           = p_alv
                                                                 layout_variant     = p_var
                                                                 testrun            = p_test
                                                                 reported_package_size = p_repsiz
                                                                 table_list         = VALUE #( FOR <line> IN s_table[]:
                                                                                                 ( CONV #( <line>-low ) ) ) )
                                     appl_log         = go_app_log ). "#EC NEEDED
      scramble_bookings->process( ).

      " Testrun -> Rollback changes
      IF p_test = abap_true.
        CALL FUNCTION 'DB_ROLLBACK'.
      ENDIF.

      " ALV ausgeben
      IF p_alv IS NOT INITIAL.
        scramble_bookings->show_alv( ).
      ENDIF.

    CATCH zcx_vcd_appl_error INTO DATA(gx_error).           "#EC NEEDED
      go_app_log->add_message( i_probclass = c_probclass_high i_msg = gx_error->get_msg( ) ).
      CALL FUNCTION 'DB_ROLLBACK'.                              "#EC "#EC CI_ROLLBACK
  ENDTRY.

  " Programmende: Meldung ins Appl Log schreiben
  MESSAGE i110(zvcd) INTO lv_dummy.
  go_app_log->add_message( zif_vcd_constants=>problem_class-high ).

  " Speichere Appl Log
  IF p_lgsa IS NOT INITIAL.
    go_app_log->save_log( ).
    COMMIT WORK.
  ENDIF.

  " Show Appl Log
  IF p_lgds IS NOT INITIAL.
    go_app_log->display_log( ).
  ENDIF.

**********************************************************************
*** FORM-ROUTINES
**********************************************************************
FORM f4_layout CHANGING cv_vari TYPE slis_vari.
  CONSTANTS gc_alv_report_name TYPE repid VALUE 'ZCL_VCD_SCRAMBLE_BOOKINGS=====CP'.

  DATA(ls_layout_info) = cl_salv_layout_service=>f4_layouts( VALUE #( report = gc_alv_report_name ) ).
  cv_vari              = ls_layout_info-layout.
ENDFORM.
