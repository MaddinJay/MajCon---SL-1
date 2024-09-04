***********************************************************************
* Identifikation
* ----------------------------------------------------------------------
* Programmname    : ZVCD_SCRAMBLE_GPART
* Spezifikation   : Anonymisierung der GP Daten
* Programmtyp     : Report
* Autor           : 14.07.2011
* Erstellungsdatum:
************************************************************************
* Kurze Funktionsbeschreibung
* ----------------
*
************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum       Name  Änderung
* --   17.04.2020  POEA  PBI 217528: Komplette Überarbeitung aufgrund
*                        Neuanforderungen Projekt DAvID
* 001  14.10.2020  BST8  PBI 283873 E-Mail Adresse zusätzlich anonymisieren
* 002  24.11.2021  BST8  PBI 394548 Anforderungen von DAvID implementiert
* 003  09.03.2023  WEOL  Bug 560188 Optimierung der Performance
* 004  06.04.2023  WEOL  PBI 569159 Bereits anonymisierte Objekt nochmals
*                                   melden
* 005  13.02.2024  WEOL  Bug 659905 Commit pro Datenpaket setzen
* 006  29.07.2024  MJON  Bug 659905 Einführung @-Escaping für DB Selects
*                                   und Ergänzen Table Key Values for
*                                   Error handling
************************************************************************

REPORT  zvcd_scramble_gpart.

INCLUDE zvcd_constants ##INCL_OK.

CONSTANTS gc_rb1 TYPE syucomm VALUE 'RB1' ##NEEDED.

TABLES:
  but000,
  dd03d.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-005.
PARAMETERS:
  rb_stand RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND gc_rb1,
  rb_trigg RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS s_part FOR but000-partner.
SELECT-OPTIONS s_kind FOR but000-bpkind.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS p_contsb TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_but000 TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_but0id TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_but0bk TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_tiban  TYPE c AS CHECKBOX.
PARAMETERS p_adrc   TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_adr2   TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_adr3   TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_adr6   TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_belege TYPE c AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS s_field FOR dd03d-fieldname.
PARAMETERS p_layout TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-002.
PARAMETERS p_rows TYPE n LENGTH 4 DEFAULT '200' OBLIGATORY.
PARAMETERS p_test TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_alv  TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-004.
PARAMETERS p_salt  TYPE string DEFAULT 'zUiFlkWj' LOWER CASE. "Salt für Hash
PARAMETERS p_algo  TYPE string DEFAULT 'SHA512' OBLIGATORY.   "Hash Algorithmus SHA512
PARAMETERS p_codep TYPE tcp00-cpcodepage DEFAULT '1252'.      "Codepage
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

TYPES: BEGIN OF ts_tiban.
         INCLUDE TYPE tiban.
         TYPES: gpart TYPE gpart_kk,
         bkvid TYPE bu_bkvid.
TYPES: END OF ts_tiban.

TYPES: BEGIN OF ts_dfkkopk.
         INCLUDE TYPE zvcd_sel_dfkkopk.
         TYPES gpart TYPE gpart_kk.
TYPES: END OF ts_dfkkopk.

TYPES: BEGIN OF ts_dfkkesr.
         INCLUDE TYPE dfkkesr.
         TYPES gpart TYPE gpart_kk.
TYPES: END OF ts_dfkkesr.

TYPES: BEGIN OF ts_dfkkzp.
         INCLUDE TYPE dfkkzp.
         TYPES gpart TYPE gpart_kk.
TYPES: END OF ts_dfkkzp.

TYPES: BEGIN OF ts_dpayh.
         INCLUDE TYPE dpayh.
         TYPES gpart TYPE gpart_kk.
TYPES: END OF ts_dpayh.

TYPES tt_dsg_trigger TYPE STANDARD TABLE OF zvcd_dsg_trigger.

DATA ##NEEDED:
  lt_contr_sachb TYPE TABLE OF zvcd_contr_sachb,
  lt_but000      TYPE TABLE OF but000,
  lt_but0id      TYPE TABLE OF but0id,
  lt_but020      TYPE TABLE OF but020,
  lt_but0bk      TYPE TABLE OF but0bk,
  lt_tiban       TYPE TABLE OF ts_tiban,
  lt_adrc        TYPE TABLE OF adrc,
  lt_adrp        TYPE TABLE OF adrp,
  lt_adr2        TYPE TABLE OF adr2,
  lt_adr3        TYPE TABLE OF adr3,
  lt_adr6        TYPE TABLE OF adr6,
  lt_dfkkop      TYPE TABLE OF zvcd_sel_dfkkop,
  ls_dfkkopk     TYPE          ts_dfkkopk,
  lt_dfkkopk     TYPE TABLE OF ts_dfkkopk,
  lt_dfkkesr     TYPE TABLE OF ts_dfkkesr,
  lt_vvscpos     TYPE TABLE OF zvcd_sel_vvscpos,
  ls_vvscpos     TYPE          zvcd_sel_vvscpos,
  lt_svvscpos    TYPE TABLE OF zvcd_svvscpos,
  lt_dpayh       TYPE TABLE OF ts_dpayh,
  lt_dfkkzp      TYPE TABLE OF ts_dfkkzp.

DATA ##NEEDED:
  lt_names_gp            TYPE zvcd_scramble_fields_t,
  lt_scrambled_gp        TYPE zvcd_scramble_fields_t,
  lt_scramble_fields_alv TYPE TABLE OF zvcd_scramble_fields_alv,
  lt_spec_rule           TYPE zcl_vcd_tools=>tt_spec_rule,
  ls_spec_rule           LIKE LINE OF lt_spec_rule.

DATA lc_dbcursor TYPE cursor ##NEEDED.

DATA ##NEEDED:
  lv_cnt_contr_sachb TYPE i,
  lv_cnt_but000      TYPE i,
  lv_cnt_but0id      TYPE i,
  lv_cnt_tiban       TYPE i,
  lv_cnt_adrp        TYPE i,
  lv_cnt_adrc        TYPE i,
  lv_cnt_adr2        TYPE i,
  lv_cnt_adr3        TYPE i,
  lv_cnt_adr6        TYPE i,
  lv_cnt_but0bk      TYPE i,
  lv_cnt_dfkkop      TYPE i,
  lv_cnt_dfkkopk     TYPE i,
  lv_cnt_dfkkesr     TYPE i,
  lv_cnt_vvscpos     TYPE i,
  lv_cnt_svvscpos    TYPE i,
  lv_cnt_dpayh       TYPE i,
  lv_cnt_dfkkzp      TYPE i,
  ls_variant         TYPE disvariant,
  ls_layout          TYPE slis_layout_alv,
  lx_error           TYPE REF TO zcx_vcd_appl_error,
  ls_names_gp        TYPE zvcd_scramble_fields,
  lv_tabkey          TYPE iban_orkey,
  ls_tiban           TYPE ts_tiban,
  lv_dummy           TYPE c,
  grf_app_log        TYPE REF TO zvcd_cl_anwendungs_log,
  ls_msg             TYPE bal_s_msg,
  lv_cnt_done        TYPE i,
  lv_cnt_error       TYPE i,
  lv_cnt_all         TYPE i,
  lv_extp            TYPE balnrext,
  lt_dsg_trigger     TYPE TABLE OF zvcd_dsg_trigger,
  lt_dsg_trigger_w   TYPE TABLE OF zvcd_dsg_trigger,
  ls_dfkkesr         TYPE ts_dfkkesr,
  ls_dpayh           TYPE ts_dpayh,
  ls_dfkkzp          TYPE ts_dfkkzp,
  lv_opbel           TYPE opbel_kk,
  lv_refnr           TYPE refnr_kk,
  lv_augbl           TYPE augbl_kk,
  lv_switch          TYPE zvcd_ft_switch.
* Beginn @003
DATA ##NEEDED:
  BEGIN              OF            ls_dimaiobpar,
    insobject TYPE          insobject_md,
    partner   TYPE          bu_partner,
  END   OF ls_dimaiobpar,
  lt_dimaiobpar LIKE TABLE OF ls_dimaiobpar,
  ls_dfkkop     TYPE          zvcd_sel_dfkkop,
  lv_sel_list   TYPE          char1024,
  lt_upd_list   TYPE          fkk_c_table.
* Ende   @003
FIELD-SYMBOLS ##NEEDED:
  <contr_sachb>     TYPE zvcd_contr_sachb,
  <but000>          TYPE but000,
  <but020>          TYPE but020,
  <but0id>          TYPE but0id,
  <adrp>            TYPE adrp,
  <tiban>           TYPE ts_tiban,
  <adrc>            TYPE adrc,
  <but0bk>          TYPE but0bk,
  <adr2>            TYPE adr2,
  <adr3>            TYPE adr3,
  <adr6>            TYPE adr6,
  <dfkkop>          TYPE zvcd_sel_dfkkop,
  <dfkkopk>         TYPE ts_dfkkopk,
  <dfkkesr>         TYPE ts_dfkkesr,
  <vvscpos>         TYPE zvcd_sel_vvscpos,
  <svvscpos>        TYPE zvcd_svvscpos,
  <dpayh>           TYPE ts_dpayh,
  <dfkkzp>          TYPE ts_dfkkzp,
  <dsg_trigger>     TYPE zvcd_dsg_trigger,
  <scramble_gp>     TYPE zvcd_scramble_fields,
  <names_gp>        TYPE zvcd_scramble_fields,
  <scramble_gp_alv> TYPE zvcd_scramble_fields_alv.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*

  p_vdat = sy-datum + 180 ##NUMBER_OK.

* Lesen der noch nicht verarbeiteten Triggereinträge
  SELECT *
    INTO TABLE lt_dsg_trigger
    FROM zvcd_dsg_trigger
    WHERE status EQ 'G'. "#EC "#EC CI_NOFIELD "Wurde von BUTOS geholt (GET)

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR p_belege.
*&---------------------------------------------------------------------*

  MESSAGE i000(zvcd) WITH TEXT-008 TEXT-009.
  "In Belege enthaltene Tabellen

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
*&---------------------------------------------------------------------*

  TRY.
      CALL METHOD zcl_vcd_alv_services=>variant_f4
        EXPORTING
          iv_repid   = sy-repid
          iv_name    = TEXT-001
        IMPORTING
          es_variant = ls_variant
        CHANGING
          cv_variant = p_layout.
    CATCH zcx_vcd_appl_error INTO lx_error.
      lx_error->create_msg( iv_into = abap_false ).
  ENDTRY.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*

  LOOP AT SCREEN.
    IF rb_stand EQ 'X'.
      IF screen-name CS 'P_CONTSB'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF rb_trigg EQ 'X'.
      IF screen-name CS 'S_PART' OR screen-name CS 'S_KIND'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  TRY.
* Protokoll
      MOVE p_extp TO lv_extp.

      CREATE OBJECT grf_app_log
        EXPORTING
          i_object        = c_object_rept
          i_subobject     = c_subobj_mig
          i_extnumber     = lv_extp
          i_prog          = sy-cprog
          i_desired_class = p_prcl
          i_aldate_del    = p_vdat
          i_del_before    = p_dbef.

      grf_app_log->add_header( p_test ).

*     Alle Sets für DB-Zugriff wie SELECT und UPDATE aufbereiten
      zcl_vcd_tools=>create_sql_sets_for_scramble( ).

      CASE abap_true.
        WHEN rb_stand.
* Verarbeitungsart "Standard mit eigener Selektion" ausgeben
          MESSAGE s000(zvcd) WITH TEXT-006 INTO lv_dummy.
          grf_app_log->add_message( i_probclass = c_probclass_high ).
* Programm darf mit dieser Verarbeitungsart nicht auf PK1 ausgeführt werden
          IF sy-sysid(1) EQ 'P' AND p_test IS INITIAL.
            MESSAGE e086(zvcd2) WITH sy-repid TEXT-029 INTO lv_dummy.
            "Programm &1 mit Verarbeitungsart &2 darf auf PK1 nicht ausgeführt werden!
            zcx_vcd_appl_error=>raise_ex( ).
          ENDIF.
        WHEN rb_trigg.
* Verarbeitungsart "Verarbeitung Triggertabelle" ausgeben
          MESSAGE s000(zvcd) WITH TEXT-007 INTO lv_dummy.
          grf_app_log->add_message( i_probclass = c_probclass_high ).
* Prüfen Feature Toggle DSG Anonymisierung inaktiv
          zcl_vcd_system_services=>get_feature_toggle(
                                     EXPORTING iv_name   = 'ZVCD_DSG_INACTIVE'
                                     IMPORTING ev_switch = lv_switch ).
          IF lv_switch EQ abap_true AND p_test IS INITIAL.
            MESSAGE e107(zvcd2) WITH 'ZVCD_DSG_INACTIVE' INTO lv_dummy.
            "DSG Anonymisierung ist mittels Feature Toggle &1 deaktiviert!
            zcx_vcd_appl_error=>raise_ex( ).
          ENDIF.
* Partner-Nr. und Verträge anhand der Einträge der Triggertabelle ermitteln
          CLEAR: s_part[], s_part, s_kind[], s_kind, lt_contr_sachb.
          PERFORM get_gpart_from_trigger USING lt_dsg_trigger
                                         CHANGING s_part[] lt_contr_sachb.
      ENDCASE.

*&---------------------------------------------------------------------*
* Löschen Sachbearbeiterzuordnung zum Vertrag
*&---------------------------------------------------------------------*

      IF p_test IS INITIAL.
* Sachbearbeiterzuordnung am Vertrag löschen
        IF p_contsb IS NOT INITIAL.
          lv_cnt_contr_sachb =  lines( lt_contr_sachb ).
          LOOP AT lt_contr_sachb ASSIGNING <contr_sachb>.
            CLEAR <contr_sachb>-partner.
          ENDLOOP.
          UPDATE zvcd_contr_sachb FROM TABLE lt_contr_sachb.
          IF sy-subrc = 0.
            lv_cnt_done = lv_cnt_done + sy-dbcnt.
          ELSE.
            RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ZVCD_CONTR_SACHB'.
          ENDIF.
        ENDIF.
      ELSE.
* Zähler setzen
        lv_cnt_contr_sachb = lines( lt_contr_sachb ).
        lv_cnt_all = lv_cnt_contr_sachb.
      ENDIF.

*&---------------------------------------------------------------------*
* Anonymisierung Personendaten
*&---------------------------------------------------------------------*

      OPEN CURSOR WITH HOLD
        lc_dbcursor FOR SELECT * FROM but000
                         WHERE partner IN s_part
                           AND bpkind IN s_kind.

* Paketweise lesen und bearbeiten
      DO.
        FETCH NEXT CURSOR lc_dbcursor
        INTO TABLE lt_but000 PACKAGE SIZE p_rows.
        IF sy-subrc = 0.
          lv_cnt_all = lv_cnt_all + lines( lt_but000 ).
        ELSE.
          EXIT.
        ENDIF.

* Identifikationsnummern lesen
        IF p_but0id IS NOT INITIAL.
          SELECT * FROM but0id INTO TABLE lt_but0id
            FOR ALL ENTRIES IN lt_but000
            WHERE partner = lt_but000-partner
            AND ( type = 'ZV_AHV' OR type = 'ZV_SOZ' ).
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* Bankverbindungen
        IF p_but0bk IS NOT INITIAL.
          SELECT * FROM but0bk INTO TABLE lt_but0bk
            FOR ALL ENTRIES IN lt_but000
            WHERE partner = lt_but000-partner.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* IBAN
        IF p_tiban IS NOT INITIAL.
          CLEAR lt_tiban.                                                                          "@005
          LOOP AT lt_but0bk ASSIGNING <but0bk>.
            lv_tabkey = <but0bk>-partner && <but0bk>-bkvid.
            SELECT * FROM tiban INTO CORRESPONDING FIELDS OF ls_tiban
              WHERE tabname = 'BUT0BK'
              AND   tabkey  = lv_tabkey.           "#EC "#EC CI_NOFIELD
              ADD 1 TO lv_cnt_all.
              ls_tiban-gpart = <but0bk>-partner.
              ls_tiban-bkvid = <but0bk>-bkvid.
              APPEND ls_tiban TO lt_tiban.
            ENDSELECT.
          ENDLOOP.
        ENDIF.

* Adressnummern holen
        SELECT * FROM but020 INTO TABLE lt_but020
          FOR ALL ENTRIES IN lt_but000
          WHERE partner = lt_but000-partner.

* Adressen lesen
        IF p_adrc IS NOT INITIAL AND lt_but020 IS NOT INITIAL.
          SELECT * FROM adrc INTO TABLE lt_adrc
            FOR ALL ENTRIES IN lt_but020
            WHERE addrnumber = lt_but020-addrnumber.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* Telefon Nummern
        IF p_adr2 IS NOT INITIAL AND lt_but020 IS NOT INITIAL.
          SELECT * FROM adr2 INTO TABLE lt_adr2
            FOR ALL ENTRIES IN lt_but020
            WHERE addrnumber EQ lt_but020-addrnumber
            AND   tel_number NE space.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* Fax Nummer
        IF p_adr3 IS NOT INITIAL AND lt_but020 IS NOT INITIAL.
          SELECT * FROM adr3 INTO TABLE lt_adr3
            FOR ALL ENTRIES IN lt_but020
            WHERE addrnumber EQ lt_but020-addrnumber
            AND   fax_number NE space.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* E-Mail Adresse
        IF p_adr6 IS NOT INITIAL AND lt_but020 IS NOT INITIAL.
          SELECT * FROM adr6 INTO TABLE lt_adr6
            FOR ALL ENTRIES IN lt_but020
            WHERE addrnumber EQ lt_but020-addrnumber
            AND   smtp_addr  NE space.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* Personenrecords
        IF p_but000 IS NOT INITIAL.
          SELECT * FROM adrp INTO TABLE lt_adrp
            FOR ALL ENTRIES IN lt_but000
            WHERE persnumber = lt_but000-persnumber.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
        ENDIF.

* Belege
        IF p_belege IS NOT INITIAL.
* Belege DFKKOP
* Zugriff auf DFKKOP über VTREF aus DIMAIOBPAR
* Beginn @005
*          clear lt_dfkkop.
          CLEAR: lt_dfkkop,  lt_dfkkopk, lt_dfkkesr, lt_dpayh, lt_dfkkzp,
                 lt_vvscpos, lt_svvscpos.
* Ende   @005
          SELECT insobject partner FROM dimaiobpar
            INTO TABLE lt_dimaiobpar
            FOR ALL ENTRIES IN lt_but000
           WHERE partner = lt_but000-partner.

*         Keine doppelten VTREF
          SORT lt_dimaiobpar BY insobject.
          DELETE ADJACENT DUPLICATES FROM lt_dimaiobpar COMPARING insobject.

          lv_sel_list = zcl_vcd_tools=>ov_sel_dfkkop.

          LOOP AT lt_dimaiobpar ASSIGNING FIELD-SYMBOL(<fs_iopar>) ##NEEDED.

            SELECT (lv_sel_list) FROM dfkkop INTO CORRESPONDING FIELDS OF @ls_dfkkop " CHG @006
             WHERE augst IN ('','9')
               AND vtref = @<fs_iopar>-insobject
               AND gpart = @<fs_iopar>-partner.
              ADD 1 TO lv_cnt_all.
              APPEND ls_dfkkop TO lt_dfkkop.

*           @005 Positionen auf CPD Konto mitlesen
              SELECT (lv_sel_list) FROM dfkkop INTO CORRESPONDING FIELDS OF @ls_dfkkop " CHG @006
               WHERE opbel EQ @ls_dfkkop-opbel
                 AND augst IN ('','9')
                 AND vtre2 EQ @<fs_iopar>-insobject.
                ADD 1 TO lv_cnt_all.
                APPEND ls_dfkkop TO lt_dfkkop.
              ENDSELECT.

            ENDSELECT.

          ENDLOOP.

          LOOP AT lt_dfkkop ASSIGNING <dfkkop>.

* Belege DFKKOPK (alle Belege mit derselben Geschäftsfall-Nr.)
            SELECT SINGLE opbel FROM dfkkko INTO lv_opbel
              WHERE opbel = <dfkkop>-opbel ##WARN_OK.
            IF sy-subrc EQ 0.
              lv_sel_list = zcl_vcd_tools=>ov_sel_dfkkopk.
              SELECT (lv_sel_list) FROM dfkkopk INTO CORRESPONDING FIELDS OF @ls_dfkkopk " CHG @006
                WHERE opbel = @lv_opbel.
                ADD 1 TO lv_cnt_all.
                ls_dfkkopk-gpart = <dfkkop>-gpart.
                APPEND ls_dfkkopk TO lt_dfkkopk.
              ENDSELECT.
            ENDIF.

* Belege DFKKESR
            lv_refnr = <dfkkop>-opbel && <dfkkop>-opupw && <dfkkop>-opupk && <dfkkop>-opupz.
            SELECT * FROM dfkkesr INTO CORRESPONDING FIELDS OF @ls_dfkkesr " CHG @006
              WHERE refnr = @lv_refnr.             "#EC "#EC CI_NOFIRST
              ADD 1 TO lv_cnt_all.
              ls_dfkkesr-gpart = <dfkkop>-gpart.
              APPEND ls_dfkkesr TO lt_dfkkesr.
            ENDSELECT.

* Belege DPAYH
            IF rb_trigg IS NOT INITIAL.
* DPAYH Einträge nur selektieren für Verarbeitungsart "Verarbeitung Triggertabelle"
              IF <dfkkop>-emgpa IS NOT INITIAL.
* Ausgleichsbeleg-Nr. der Auszahlungsposition auf dem CPD lesen
                SELECT SINGLE augbl FROM dfkkop INTO @lv_augbl
                  WHERE opbel = @<dfkkop>-opbel
                  AND   vktyp = '90'.                       "#EC WARNOK
                IF sy-subrc EQ 0.
* Ausgleichsbeleg muss gesetzt sein
                  IF lv_augbl IS NOT INITIAL.                                                      "@005
                    SELECT * FROM dpayh INTO CORRESPONDING FIELDS OF @ls_dpayh " CHG @006
                      WHERE doc1r = @lv_augbl.
                      ADD 1 TO lv_cnt_all.
                      ls_dpayh-gpart = <dfkkop>-gpart.
                      APPEND ls_dpayh TO lt_dpayh.
                    ENDSELECT.
                  ENDIF.                                                                           "@005
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

* Belege VVSCPOS
          lv_sel_list = zcl_vcd_tools=>ov_sel_vvscpos.
          SELECT (lv_sel_list) FROM vvscpos INTO CORRESPONDING FIELDS OF TABLE @lt_vvscpos " CHG @006
            FOR ALL ENTRIES IN @lt_but000
            WHERE gpart = @lt_but000-partner.
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
* Belege ZVCD_SVVSCPOS
          SELECT * FROM zvcd_svvscpos INTO TABLE @lt_svvscpos
            FOR ALL ENTRIES IN @lt_but000
            WHERE gpart = @lt_but000-partner.      "#EC "#EC CI_NOFIELD
          lv_cnt_all = lv_cnt_all + sy-dbcnt.
* Belege DFKKZP
          LOOP AT lt_but0id ASSIGNING <but0id>.
            SELECT * FROM dfkkzp INTO CORRESPONDING FIELDS OF @ls_dfkkzp " CHG @006
              WHERE zzv_nahv_nr = @<but0id>-idnumber. "#EC "#EC CI_NOFIELD
              ADD 1 TO lv_cnt_all.
              ls_dfkkzp-gpart = <but0id>-partner.
              APPEND ls_dfkkzp TO lt_dfkkzp.
            ENDSELECT.
          ENDLOOP.
        ENDIF.

* Felder in Verschlüsselungstabelle füllen
        IF lt_names_gp IS NOT INITIAL.
          CLEAR lt_names_gp.
        ENDIF.

        LOOP AT lt_but000 ASSIGNING <but000>.
*   Partner - Namen
          IF p_but000 IS NOT INITIAL.
            CLEAR ls_names_gp.
            ls_names_gp-strukt = 'BUT000'.
            ls_names_gp-key    = |{ <but000>-partner }|. " @006 <- ADD
            ls_names_gp-partner = <but000>-partner.
            MOVE-CORRESPONDING <but000> TO ls_names_gp-fields ##ENH_OK.
            IF ls_names_gp-fields IS NOT INITIAL.
              APPEND ls_names_gp TO lt_names_gp.
            ENDIF.

            READ TABLE lt_adrp ASSIGNING <adrp> WITH KEY persnumber = <but000>-persnumber.
            IF sy-subrc = 0.
              CLEAR ls_names_gp.
              ls_names_gp-strukt     = 'ADRP'.
              ls_names_gp-key        = |{ <adrp>-persnumber } / { <adrp>-date_from } / { <adrp>-nation }|. " @006 <- ADD
              ls_names_gp-partner    = <but000>-partner.
              ls_names_gp-persnumber = <but000>-persnumber.
              MOVE-CORRESPONDING <adrp> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDIF.
          ENDIF.

*   Bankverbindungen
          IF p_but0bk IS NOT INITIAL.
            LOOP AT lt_but0bk ASSIGNING <but0bk> WHERE partner = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'BUT0BK'.
              ls_names_gp-key     = |{ <but0bk>-partner } / { <but0bk>-bkvid }|. " @006 <- ADD
              ls_names_gp-partner = <but0bk>-partner.
              ls_names_gp-bkvid   = <but0bk>-bkvid.
              MOVE-CORRESPONDING <but0bk> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDLOOP.
*   Spezial Verschlüsselungsregel 'Z' für Felder, welche gelöscht werden sollen
*   für Methode zcl_vcd_tools=>scramble_gp_fields
            ls_spec_rule = VALUE #( fieldname = 'BANKN' rule = 'Z' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'IBAN' rule = 'Z' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
          ENDIF.

*   IBAN
          IF p_tiban IS NOT INITIAL.
            LOOP AT lt_tiban ASSIGNING <tiban> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'TIBAN'.
              ls_names_gp-key     = |{ <tiban>-banks } / { <tiban>-bankl } / { <tiban>-bankn } / { <tiban>-bkont }|. " @006 <- ADD
              ls_names_gp-partner = <tiban>-gpart.
              ls_names_gp-bkvid   = <tiban>-bkvid.
              MOVE-CORRESPONDING <tiban> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDLOOP.
*   Spezial Verschlüsselungsregel 'Z' für Felder, welche gelöscht werden sollen
*   für Methode zcl_vcd_tools=>scramble_gp_fields
            ls_spec_rule = VALUE #( fieldname = 'BANKN' rule = 'Z' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'IBAN' rule = 'Z' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
          ENDIF.

*   Adressen
          IF p_adrc IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adrc ASSIGNING <adrc> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                CLEAR ls_names_gp.
                ls_names_gp-strukt     = 'ADRC'.
                ls_names_gp-key        = |{ <adrc>-addrnumber } / { <adrc>-date_from } / { <adrc>-nation }|. " @006 <- ADD
                ls_names_gp-partner    = <but000>-partner.
                ls_names_gp-addrnumber = <adrc>-addrnumber.
                MOVE-CORRESPONDING <adrc> TO ls_names_gp-fields ##ENH_OK.
                IF ls_names_gp-fields IS NOT INITIAL.
                  APPEND ls_names_gp TO lt_names_gp.
                ENDIF.
              ENDIF.
            ENDLOOP.
*   Spezial Verschlüsselungsregel für Postleitzahl
            IF rb_stand EQ abap_true.
*   Verarbeitung "Standard mit eigener Selektion": Postleitzahl nicht anonymisieren
              ls_spec_rule = VALUE #( fieldname = 'POST_CODE1' rule = 'Y' ).
            ELSE.
*   Verarbeitung "Verarbeitung Triggertabelle": Postleitzahl Ziffern mit 9 ersetzen
              ls_spec_rule = VALUE #( fieldname = 'POST_CODE1' rule = 'C' ).
            ENDIF.
            COLLECT ls_spec_rule INTO lt_spec_rule.
          ENDIF.

*   Telefon Nummern
          IF p_adr2 IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              LOOP AT lt_adr2 ASSIGNING <adr2> WHERE addrnumber = <but020>-addrnumber.
                CLEAR ls_names_gp.
                ls_names_gp-strukt     = 'ADR2'.
                ls_names_gp-key        = |{ <adr2>-addrnumber } / { <adr2>-persnumber } / { <adr2>-date_from } / { <adr2>-consnumber }|. " @006 <- ADD
                ls_names_gp-partner    = <but000>-partner.
                ls_names_gp-addrnumber = <but020>-addrnumber.
                MOVE-CORRESPONDING <adr2> TO ls_names_gp-fields ##ENH_OK.
                IF ls_names_gp-fields IS NOT INITIAL.
                  APPEND ls_names_gp TO lt_names_gp.
                ENDIF.
              ENDLOOP.
            ENDLOOP.
*   Spezial Verschlüsselungsregel 'B' für Telefon Nummern für Methode zcl_vcd_tools=>scramble_gp_fields
            ls_spec_rule = VALUE #( fieldname = 'TEL_NUMBER' rule = 'B' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'TELNR_LONG' rule = 'B' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'TELNR_CALL' rule = 'B' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
          ENDIF.

*   Fax Nummer
          IF p_adr3 IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adr3 ASSIGNING <adr3> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                CLEAR ls_names_gp.
                ls_names_gp-strukt = 'ADR3'.
                ls_names_gp-key        = |{ <adr3>-addrnumber } / { <adr3>-persnumber } / { <adr3>-date_from } / { <adr3>-consnumber }|. " @006 <- ADD
                ls_names_gp-partner = <but000>-partner.
                ls_names_gp-addrnumber = <but020>-addrnumber.
                MOVE-CORRESPONDING <adr3> TO ls_names_gp-fields ##ENH_OK.
                IF ls_names_gp-fields IS NOT INITIAL.
                  APPEND ls_names_gp TO lt_names_gp.
                ENDIF.
              ENDIF.
            ENDLOOP.
*   Spezial Verschlüsselungsregel 'B' für Fax Nummer für Methode zcl_vcd_tools=>scramble_gp_fields
            ls_spec_rule = VALUE #( fieldname = 'FAX_NUMBER' rule = 'B' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'FAXNR_LONG' rule = 'B' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'FAXNR_CALL' rule = 'B' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
          ENDIF.

*   E-Mail Adressen
          IF p_adr6 IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adr6 ASSIGNING <adr6> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                CLEAR ls_names_gp.
                ls_names_gp-strukt     = 'ADR6'.
                ls_names_gp-key        = |{ <adr6>-addrnumber } / { <adr6>-persnumber } / { <adr6>-date_from } / { <adr6>-consnumber }|. " @006 <- ADD
                ls_names_gp-partner    = <but000>-partner.
                ls_names_gp-addrnumber = <but020>-addrnumber.
                MOVE-CORRESPONDING <adr6> TO ls_names_gp-fields ##ENH_OK.
                IF ls_names_gp-fields IS NOT INITIAL.
                  APPEND ls_names_gp TO lt_names_gp.
                ENDIF.
              ENDIF.
            ENDLOOP.
*   Spezial Verschlüsselungsregel 'A' für E-Mail Adressen für Methode zcl_vcd_tools=>scramble_gp_fields
            ls_spec_rule = VALUE #( fieldname = 'SMTP_ADDR' rule = 'A' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
            ls_spec_rule = VALUE #( fieldname = 'SMTP_SRCH' rule = 'A' ).
            COLLECT ls_spec_rule INTO lt_spec_rule.
          ENDIF.

*  Belege
          IF p_belege IS NOT INITIAL.
*  DFKKOP
            LOOP AT lt_dfkkop ASSIGNING <dfkkop> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'DFKKOP'.
              ls_names_gp-key     = |{ <dfkkop>-opbel } / { <dfkkop>-opupw } / { <dfkkop>-opupk } / { <dfkkop>-opupz }|. " @006 <- ADD
              ls_names_gp-partner = <but000>-partner.
              ls_names_gp-opbel   = <dfkkop>-opbel.
              ls_names_gp-opupk   = <dfkkop>-opupk.
              ls_names_gp-opupw   = <dfkkop>-opupw.
              ls_names_gp-opupz   = <dfkkop>-opupz.
              MOVE-CORRESPONDING <dfkkop> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
*   Spezial Verschlüsselungsregel 'Z' für Felder, welche gelöscht werden sollen
*   für Methode zcl_vcd_tools=>scramble_gp_fields
              ls_spec_rule = VALUE #( fieldname = 'EMGPA' rule = 'Z' ).
              COLLECT ls_spec_rule INTO lt_spec_rule.
              ls_spec_rule = VALUE #( fieldname = 'EMBVT' rule = 'Z' ).
              COLLECT ls_spec_rule INTO lt_spec_rule.
            ENDLOOP.
*  DFKKOPK
            LOOP AT lt_dfkkopk ASSIGNING <dfkkopk> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'DFKKOPK'.
              ls_names_gp-key     = |{ <dfkkopk>-opbel } / { <dfkkopk>-opupk }|.
              ls_names_gp-partner = <but000>-partner.
              ls_names_gp-opbel   = <dfkkopk>-opbel.
              ls_names_gp-opupk   = <dfkkopk>-opupk.
              MOVE-CORRESPONDING <dfkkopk> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDLOOP.
*  DFKKESR
            LOOP AT lt_dfkkesr ASSIGNING <dfkkesr> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'DFKKESR'.
              ls_names_gp-key     = |{ <dfkkesr>-reftp } / { <dfkkesr>-refnr }|.
              ls_names_gp-partner = <but000>-partner.
              ls_names_gp-reftp   = <dfkkesr>-reftp.
              ls_names_gp-refnr   = <dfkkesr>-refnr.
              MOVE-CORRESPONDING <dfkkesr> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
*   Spezial Verschlüsselungsregel für Postleitzahl
              IF rb_stand EQ abap_true.
*   Verarbeitung "Standard mit eigener Selektion": Postleitzahl nicht anonymisieren
                ls_spec_rule = VALUE #( fieldname = 'POST_CODE1' rule = 'Y' ).
              ELSE.
*   Verarbeitung "Verarbeitung Triggertabelle": Postleitzahl Ziffern mit 9 ersetzen
                ls_spec_rule = VALUE #( fieldname = 'POST_CODE1' rule = 'C' ).
              ENDIF.
              COLLECT ls_spec_rule INTO lt_spec_rule.
            ENDLOOP.
*  VVSCPOS
            LOOP AT lt_vvscpos ASSIGNING <vvscpos> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'VVSCPOS'.
              ls_names_gp-key     = |{ <vvscpos>-gpart } / { <vvscpos>-vtref } / { <vvscpos>-posnr } / { <vvscpos>-scposnr }|. " @006 <- ADD
              ls_names_gp-partner = <but000>-partner.
              ls_names_gp-vtref   = <vvscpos>-vtref.
              ls_names_gp-posnr   = <vvscpos>-posnr.
              ls_names_gp-scposnr = <vvscpos>-scposnr.
              MOVE-CORRESPONDING <vvscpos> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDLOOP.
*  ZVCD_SVVSCPOS
            LOOP AT lt_svvscpos ASSIGNING <svvscpos>
                                     WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt     = 'ZVCD_SVVSCPOS'.
              ls_names_gp-key     = |{ <svvscpos>-gpart } / { <svvscpos>-vtref } / { <svvscpos>-posnr } / { <svvscpos>-scposnr }|. " @006 <- ADD
              ls_names_gp-partner    = <but000>-partner.
              ls_names_gp-zzv_number = <svvscpos>-zzv_number.
              ls_names_gp-zzv_chef   = <svvscpos>-zzv_chef.
              MOVE-CORRESPONDING <svvscpos> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDLOOP.
*  DPAYH
            LOOP AT lt_dpayh ASSIGNING <dpayh> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'DPAYH'.
              ls_names_gp-key     = |{ <dpayh>-laufd } / { <dpayh>-laufi } / { <dpayh>-orign } / { <dpayh>-xvorl } / { <dpayh>-payno } / { <dpayh>-subno }|. " @006 <- ADD
              ls_names_gp-partner = <but000>-partner.
              ls_names_gp-laufd   = <dpayh>-laufd.
              ls_names_gp-laufi   = <dpayh>-laufi.
              ls_names_gp-orign   = <dpayh>-orign.
              ls_names_gp-xvorl   = <dpayh>-xvorl.
              ls_names_gp-payno   = <dpayh>-payno.
              ls_names_gp-subno   = <dpayh>-subno.
              MOVE-CORRESPONDING <dpayh> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
*   Spezial Verschlüsselungsregel für Postleitzahl
              IF rb_stand EQ abap_true.
*   Verarbeitung "Standard mit eigener Selektion": Postleitzahl nicht anonymisieren
                ls_spec_rule = VALUE #( fieldname = 'ZPST1' rule = 'Y' ).
                COLLECT ls_spec_rule INTO lt_spec_rule.
                ls_spec_rule = VALUE #( fieldname = 'ZPST2' rule = 'Y' ).
                COLLECT ls_spec_rule INTO lt_spec_rule.
              ELSE.
*   Verarbeitung "Verarbeitung Triggertabelle": Postleitzahl Ziffern mit 9 ersetzen
                ls_spec_rule = VALUE #( fieldname = 'ZPST1' rule = 'C' ).
                COLLECT ls_spec_rule INTO lt_spec_rule.
                ls_spec_rule = VALUE #( fieldname = 'ZPST2' rule = 'C' ).
                COLLECT ls_spec_rule INTO lt_spec_rule.
              ENDIF.
            ENDLOOP.
*  DFKKZP
            LOOP AT lt_dfkkzp ASSIGNING <dfkkzp> WHERE gpart = <but000>-partner.
              CLEAR ls_names_gp.
              ls_names_gp-strukt  = 'DFKKZP'.
              ls_names_gp-key     = |{ <dfkkzp>-keyz1 } / { <dfkkzp>-posza }|. " @006 <- ADD
              ls_names_gp-partner = <but000>-partner.
              ls_names_gp-keyz1   = <dfkkzp>-keyz1.
              ls_names_gp-posza   = <dfkkzp>-posza.
              MOVE-CORRESPONDING <dfkkzp> TO ls_names_gp-fields ##ENH_OK.
              IF ls_names_gp-fields IS NOT INITIAL.
                APPEND ls_names_gp TO lt_names_gp.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

*   Daten anonymisieren:
        lt_scrambled_gp = zcl_vcd_tools=>scramble_gp_fields( EXPORTING it_daten     = lt_names_gp
                                                                       iv_salt      = p_salt
                                                                       iv_algo      = p_algo
                                                                       it_spec_rule = lt_spec_rule
                                                                       iv_codepage  = p_codep
                                                                       ir_appl_log  = grf_app_log ).

*   Anonymisierte Daten zurück
        LOOP AT lt_but000 ASSIGNING <but000>.
*   Partner - Namen
          IF p_but000 IS NOT INITIAL.
            READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'BUT000'
                                                                     partner = <but000>-partner.
            IF sy-subrc = 0.
              zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                    ir_selop_range = s_field[]
                                                                    iv_not_inital = abap_true
                                                          CHANGING  cr_struc_dst = <but000> ).
              lv_cnt_but000 = lv_cnt_but000 + 1.
            ENDIF.

*   Partner Identifikationsnummern (Einträge werden gelöscht bei Verarbeitungsart: Verarbeitung Triggertabelle
            lv_cnt_but0id = lines( lt_but0id ).

            READ TABLE lt_adrp ASSIGNING <adrp> WITH KEY persnumber = <but000>-persnumber.
            IF sy-subrc = 0.

              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'ADRP'
                                                                       partner = <but000>-partner
                                                                       persnumber = <but000>-persnumber.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <adrp> ).
                lv_cnt_adrp = lv_cnt_adrp + 1.
              ENDIF.
            ENDIF.
          ENDIF.

*   Bankverbindungen
          IF p_but0bk IS NOT INITIAL.
            LOOP AT lt_but0bk ASSIGNING <but0bk> WHERE partner = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'BUT0BK'
                                                                       partner = <but000>-partner
                                                                       bkvid = <but0bk>-bkvid.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_false
                                                            CHANGING  cr_struc_dst = <but0bk> ).
                lv_cnt_but0bk = lv_cnt_but0bk + 1.
              ENDIF.
            ENDLOOP.
          ENDIF.

*   IBAN
          IF p_tiban IS NOT INITIAL.
            LOOP AT lt_tiban ASSIGNING <tiban> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'TIBAN'
                                                                       partner = <tiban>-gpart
                                                                       bkvid = <tiban>-bkvid.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <tiban> ).
                lv_cnt_tiban = lv_cnt_tiban + 1.
              ENDIF.
            ENDLOOP.
          ENDIF.

*   Adressen
          IF p_adrc IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adrc ASSIGNING <adrc> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'ADRC'
                                                                         partner = <but000>-partner
                                                                         addrnumber = <adrc>-addrnumber.
                IF sy-subrc = 0.
                  zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                        ir_selop_range = s_field[]
                                                                        iv_not_inital = abap_true
                                                              CHANGING  cr_struc_dst = <adrc> ).
                  lv_cnt_adrc = lv_cnt_adrc + 1.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

*   Telefon Nummern
          IF p_adr2 IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adr2 ASSIGNING <adr2> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'ADR2'
                                                                         partner = <but000>-partner
                                                                         addrnumber = <but020>-addrnumber.
                IF sy-subrc = 0.
                  zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                        ir_selop_range = s_field[]
                                                                        iv_not_inital = abap_true
                                                              CHANGING  cr_struc_dst = <adr2> ).
                  lv_cnt_adr2 = lv_cnt_adr2 + 1.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

*   Fax Nummer
          IF p_adr3 IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adr3 ASSIGNING <adr3> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'ADR3'
                                                                         partner = <but000>-partner
                                                                         addrnumber = <but020>-addrnumber.
                IF sy-subrc = 0.
                  zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                        ir_selop_range = s_field[]
                                                                        iv_not_inital = abap_true
                                                              CHANGING  cr_struc_dst = <adr3> ).
                  lv_cnt_adr3 = lv_cnt_adr3 + 1.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

*   E-Mail Adressen
          IF p_adr6 IS NOT INITIAL.
            LOOP AT lt_but020 ASSIGNING <but020> WHERE partner = <but000>-partner.
              READ TABLE lt_adr6 ASSIGNING <adr6> WITH KEY addrnumber = <but020>-addrnumber.
              IF sy-subrc = 0.
                READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'ADR6'
                                                                         partner = <but000>-partner
                                                                         addrnumber = <but020>-addrnumber.
                IF sy-subrc = 0.
                  zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                        ir_selop_range = s_field[]
                                                                        iv_not_inital = abap_true
                                                              CHANGING  cr_struc_dst = <adr6> ).
                  lv_cnt_adr6 = lv_cnt_adr6 + 1.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

*  Belege
          IF p_belege IS NOT INITIAL.
*  DFKKOP
            LOOP AT lt_dfkkop ASSIGNING <dfkkop> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'DFKKOP'
                                                                       partner = <but000>-partner
                                                                       opbel = <dfkkop>-opbel
                                                                       opupk = <dfkkop>-opupk
                                                                       opupw = <dfkkop>-opupw
                                                                       opupz = <dfkkop>-opupz.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_false
                                                            CHANGING  cr_struc_dst = <dfkkop> ).
                lv_cnt_dfkkop = lv_cnt_dfkkop + 1.
              ENDIF.
            ENDLOOP.
*  DFKKOPK
            LOOP AT lt_dfkkopk ASSIGNING <dfkkopk> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'DFKKOPK'
                                                                       partner = <but000>-partner
                                                                       opbel = <dfkkopk>-opbel
                                                                       opupk = <dfkkopk>-opupk.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <dfkkopk> ).
                lv_cnt_dfkkopk = lv_cnt_dfkkopk + 1.
              ENDIF.
            ENDLOOP.
*  DFKKESR
            LOOP AT lt_dfkkesr ASSIGNING <dfkkesr> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'DFKKESR'
                                                                       partner = <but000>-partner
                                                                       reftp = <dfkkesr>-reftp
                                                                       refnr = <dfkkesr>-refnr.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <dfkkesr> ).
                lv_cnt_dfkkesr = lv_cnt_dfkkesr + 1.
              ENDIF.
            ENDLOOP.
*  VVSCPOS
            LOOP AT lt_vvscpos ASSIGNING <vvscpos> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'VVSCPOS'
                                                                       partner = <but000>-partner
                                                                       vtref = <vvscpos>-vtref
                                                                       posnr = <vvscpos>-posnr
                                                                       scposnr = <vvscpos>-scposnr.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <vvscpos> ).
                lv_cnt_vvscpos = lv_cnt_vvscpos + 1.
              ENDIF.
            ENDLOOP.
*  ZVCD_SVVSCPOS
            LOOP AT lt_svvscpos ASSIGNING <svvscpos> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'ZVCD_SVVSCPOS'
                                                                       partner = <but000>-partner
                                                                       zzv_number = <svvscpos>-zzv_number
                                                                       zzv_chef = <svvscpos>-zzv_chef.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <svvscpos> ).
                lv_cnt_svvscpos = lv_cnt_svvscpos + 1.
              ENDIF.
            ENDLOOP.
*  DPAYH
            LOOP AT lt_dpayh ASSIGNING <dpayh> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'DPAYH'
                                                                       partner = <but000>-partner
                                                                       laufd = <dpayh>-laufd
                                                                       laufi = <dpayh>-laufi
                                                                       orign = <dpayh>-orign
                                                                       xvorl = <dpayh>-xvorl
                                                                       payno = <dpayh>-payno
                                                                       subno = <dpayh>-subno.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <dpayh> ).
                lv_cnt_dpayh = lv_cnt_dpayh + 1.
              ENDIF.
            ENDLOOP.
*  DFKKZP
            LOOP AT lt_dfkkzp ASSIGNING <dfkkzp> WHERE gpart = <but000>-partner.
              READ TABLE lt_scrambled_gp ASSIGNING <names_gp> WITH KEY strukt = 'DFKKZP'
                                                                       partner = <but000>-partner
                                                                       keyz1 = <dfkkzp>-keyz1
                                                                       posza = <dfkkzp>-posza.
              IF sy-subrc = 0.
                zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <names_gp>-fields
                                                                      ir_selop_range = s_field[]
                                                                      iv_not_inital = abap_true
                                                            CHANGING  cr_struc_dst = <dfkkzp> ).
                lv_cnt_dfkkzp = lv_cnt_dfkkzp + 1.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

* Update der DB
        IF p_test IS INITIAL.

* Personenrecords
          IF p_but000 IS NOT INITIAL.
            UPDATE but000 FROM TABLE lt_but000.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'BUT000'.
            ENDIF.
            UPDATE adrp FROM TABLE lt_adrp.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ADRP'.
            ENDIF.
          ENDIF.

* Partner Identifikationen -> Löschen bei Verarbeitungsart: Verarbeitung Trigger
          IF p_but0id IS NOT INITIAL.
            IF rb_trigg EQ abap_true.
              DELETE but0id FROM TABLE lt_but0id.
              IF sy-subrc = 0.
                lv_cnt_done = lv_cnt_done + sy-dbcnt.
              ELSE.
                RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'BUT0ID'.
              ENDIF.
            ENDIF.
          ENDIF.

* Bankverbindungen
          IF p_but0bk IS NOT INITIAL.
            UPDATE but0bk FROM TABLE lt_but0bk.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'BUT0BK'.
            ENDIF.
          ENDIF.

* IBAN
          IF p_tiban IS NOT INITIAL.
            UPDATE tiban FROM TABLE lt_tiban.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'TIBAN'.
            ENDIF.
          ENDIF.

* Adressen
          IF p_adrc IS NOT INITIAL.
            UPDATE adrc FROM TABLE lt_adrc.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ADRC'.
            ENDIF.
          ENDIF.

* Telefon Nummern
          IF p_adr2 IS NOT INITIAL.
            UPDATE adr2 FROM TABLE lt_adr2.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ADR2'.
            ENDIF.
          ENDIF.

* Fax Nummern
          IF p_adr3 IS NOT INITIAL.
            UPDATE adr3 FROM TABLE lt_adr3.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ADR3'.
            ENDIF.
          ENDIF.

* E-Mail Adressen
          IF p_adr6 IS NOT INITIAL.
            UPDATE adr6 FROM TABLE lt_adr6.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ADR6'.
            ENDIF.
          ENDIF.

* Belege
          IF p_belege IS NOT INITIAL.
* DFKKOP
            lt_upd_list = zcl_vcd_tools=>ot_upd_dfkkop.
            IF lt_upd_list IS INITIAL.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e135(zvcd2) WITH 'DFKKOP'.
            ENDIF.

            LOOP AT lt_dfkkop  INTO ls_dfkkop.
              UPDATE dfkkop SET (lt_upd_list)
                WHERE opbel = @ls_dfkkop-opbel
                  AND opupw = @ls_dfkkop-opupw
                  AND opupk = @ls_dfkkop-opupk
                  AND opupz = @ls_dfkkop-opupz.
              IF sy-subrc = 0.
                lv_cnt_done = lv_cnt_done + 1.
              ELSE.
                RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'DFKKOP'.
              ENDIF.
            ENDLOOP.
* DFKKOPK
            lt_upd_list = zcl_vcd_tools=>ot_upd_dfkkopk.
            IF lt_upd_list IS INITIAL.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e135(zvcd2) WITH 'DFKKOP'.
            ENDIF.

            LOOP AT lt_dfkkopk  INTO ls_dfkkopk.
              UPDATE dfkkopk SET (lt_upd_list)
                WHERE opbel = @ls_dfkkopk-opbel
                  AND opupk = @ls_dfkkopk-opupk.
              IF sy-subrc = 0.
                lv_cnt_done = lv_cnt_done + 1.
              ELSE.
                RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'DFKKOPK'.
              ENDIF.
            ENDLOOP.
* DFKKESR
            UPDATE dfkkesr FROM TABLE lt_dfkkesr.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'DFKKESR'.
            ENDIF.
* VVSCPOS
            lt_upd_list = zcl_vcd_tools=>ot_upd_vvscpos.
            IF lt_upd_list IS INITIAL.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e135(zvcd2) WITH 'VVSCPOS'.
            ENDIF.
            LOOP AT lt_vvscpos INTO ls_vvscpos.
              UPDATE vvscpos SET (lt_upd_list)
                WHERE gpart   = @ls_vvscpos-gpart
                  AND vtref   = @ls_vvscpos-vtref
                  AND posnr   = @ls_vvscpos-posnr
                  AND scposnr = @ls_vvscpos-scposnr.

              IF sy-subrc = 0.
                lv_cnt_done = lv_cnt_done + 1.
              ELSE.
                RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'VVSCPOS'.
              ENDIF.
            ENDLOOP.
* ZVCD_SVVSCPOS
            UPDATE zvcd_svvscpos FROM TABLE lt_svvscpos.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'ZVCD_SVVSCPOS'.
            ENDIF.
* DPAYH
            UPDATE dpayh FROM TABLE lt_dpayh.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'DPAYH'.
            ENDIF.
* DFKKZP
            UPDATE dfkkzp FROM TABLE lt_dfkkzp.
            IF sy-subrc = 0.
              lv_cnt_done = lv_cnt_done + sy-dbcnt.
            ELSE.
              RAISE EXCEPTION TYPE zcx_vcd_appl_error MESSAGE e089(zvcd) WITH 'DFKKZP'.
            ENDIF.
          ENDIF.
* Verarbeitete Triggereinträge in Tabelle ZVCD_DSG_TRIGGER auf Status 'A' setzen
        ELSE.
          lv_cnt_done = lv_cnt_done + lines( lt_scrambled_gp ).
* ZVCD_CONTR_SACHB und BUT0ID werden gelöscht d.h. nicht anonymisiert und
* sind deshalb in in LT_SCRAMBLED_GP enthalten und müssen nachträglich addiert werden
          ADD lv_cnt_contr_sachb TO lv_cnt_done.
          ADD lv_cnt_but0id TO lv_cnt_done.
          IF p_alv IS NOT INITIAL.
* Simulation -> ALV
            LOOP  AT lt_names_gp ASSIGNING <names_gp>.
              APPEND INITIAL LINE TO lt_scramble_fields_alv ASSIGNING <scramble_gp_alv>.
              MOVE-CORRESPONDING <names_gp> TO <scramble_gp_alv>.
              MOVE-CORRESPONDING <names_gp>-fields TO <scramble_gp_alv>.
              CLEAR <scramble_gp_alv>-linecolor.
            ENDLOOP.
            LOOP  AT lt_scrambled_gp ASSIGNING <scramble_gp>.
              APPEND INITIAL LINE TO lt_scramble_fields_alv ASSIGNING <scramble_gp_alv>.
              MOVE-CORRESPONDING <scramble_gp> TO <scramble_gp_alv>.
              READ TABLE lt_names_gp ASSIGNING <names_gp> WITH KEY strukt     = <scramble_gp>-strukt
                                                                   partner    = <scramble_gp>-partner
                                                                   addrnumber = <scramble_gp>-addrnumber
                                                                   persnumber = <scramble_gp>-persnumber
                                                                   bkvid      = <scramble_gp>-bkvid
                                                                   opbel      = <scramble_gp>-opbel
                                                                   opupw      = <scramble_gp>-opupw
                                                                   opupk      = <scramble_gp>-opupk
                                                                   opupz      = <scramble_gp>-opupz
                                                                   reftp      = <scramble_gp>-reftp
                                                                   refnr      = <scramble_gp>-refnr
                                                                   vtref      = <scramble_gp>-vtref
                                                                   posnr      = <scramble_gp>-posnr
                                                                   scposnr    = <scramble_gp>-scposnr
                                                                   zzv_number = <scramble_gp>-zzv_number
                                                                   zzv_chef   = <scramble_gp>-zzv_chef
                                                                   laufd      = <scramble_gp>-laufd
                                                                   laufi      = <scramble_gp>-laufi
                                                                   orign      = <scramble_gp>-orign
                                                                   xvorl      = <scramble_gp>-xvorl
                                                                   payno      = <scramble_gp>-payno
                                                                   subno      = <scramble_gp>-subno
                                                                   keyz1      = <scramble_gp>-keyz1
                                                                   posza      = <scramble_gp>-posza.
              IF sy-subrc = 0.
                MOVE-CORRESPONDING <names_gp>-fields TO <scramble_gp_alv>.
              ENDIF.
              zcl_vcd_tools=>move_corresponding_in_range( EXPORTING ir_struc_src = <scramble_gp>-fields
                                                                    ir_selop_range = s_field[]
                                                                    iv_not_inital = abap_false
                                                          CHANGING cr_struc_dst = <scramble_gp_alv> ).
              <scramble_gp_alv>-linecolor = 'C210'. "Green
            ENDLOOP.
          ENDIF.
        ENDIF.
* Beginn @005
        IF p_test IS INITIAL.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
* Ende   @005
      ENDDO.

*     Status bei Triggerverarbeitung in jedem Fall setzen
      IF rb_trigg = abap_true.
        PERFORM update_trigger_to_status_a USING lt_dsg_trigger.
      ENDIF.

*     Commit und Rollback nicht abhängig machen von ALV-Liste
      IF p_test IS INITIAL.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.                            "#EC "#EC CI_ROLLBACK
      ENDIF.

      IF p_alv IS NOT INITIAL.
        IF p_test IS NOT INITIAL.

*     Initialisierung ALV
          SORT lt_scramble_fields_alv BY partner linecolor strukt.
          ls_layout-info_fieldname = 'LINECOLOR'.
          IF ls_variant IS INITIAL.
            ls_variant = zcl_vcd_alv_services=>variant_init( iv_variant = p_layout iv_repid = sy-repid ).
          ENDIF.

          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_grid_title       = sy-title
              i_callback_program = sy-repid
              is_layout          = ls_layout
              i_structure_name   = 'ZVCD_SCRAMBLE_FIELDS_ALV'
              i_save             = 'A'
              is_variant         = ls_variant
            TABLES
              t_outtab           = lt_scramble_fields_alv
            EXCEPTIONS
              program_error      = 1
              OTHERS             = 2.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_vcd_appl_error
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.

*Ausgabe Protokoll
      MESSAGE s000(zvcd) WITH TEXT-028 lv_cnt_contr_sachb INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-012 lv_cnt_but000 INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-020 lv_cnt_but0id INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-015 lv_cnt_but0bk INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-021 lv_cnt_tiban INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-013 lv_cnt_adrp INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-014 lv_cnt_adrc INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-018 lv_cnt_adr2 INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-019 lv_cnt_adr3 INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-017 lv_cnt_adr6 INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-016 lv_cnt_dfkkop INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-022 lv_cnt_dfkkopk INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-023 lv_cnt_dfkkesr INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-024 lv_cnt_vvscpos INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-025 lv_cnt_svvscpos INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-027 lv_cnt_dpayh INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

      MESSAGE s000(zvcd) WITH TEXT-026 lv_cnt_dfkkzp INTO lv_dummy.
      grf_app_log->add_message( i_probclass = c_probclass_high ).

    CATCH zcx_vcd_appl_error INTO lx_error.
      ls_msg = lx_error->get_msg( ).
      grf_app_log->add_message( i_probclass = c_probclass_high i_msg = ls_msg ).
      lv_cnt_error = lv_cnt_all.
      CLEAR lv_cnt_done.
      ROLLBACK WORK.                              "#EC "#EC CI_ROLLBACK
  ENDTRY.

  grf_app_log->add_footer( i_lcnt_all = lv_cnt_all i_lcnt_done = lv_cnt_done i_lcnt_error = lv_cnt_error ).

  IF p_lgsa IS NOT INITIAL.
    grf_app_log->save_log( ).
    COMMIT WORK.
  ENDIF.

  IF p_lgds IS NOT INITIAL.
    grf_app_log->display_log( ).
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_GPART_FROM_TRIGGER
*&---------------------------------------------------------------------*
*       Partner-Nr. anhand der Einträge der Triggertabelle ermitteln
*----------------------------------------------------------------------*
FORM get_gpart_from_trigger  USING    pt_dsg_trigger TYPE STANDARD TABLE
                             CHANGING pt_part        TYPE ANY TABLE
                                      pt_contr_sachb TYPE STANDARD TABLE.

  DATA:
    lt_abrkat     TYPE RANGE OF zvcd_abrkat,
    ls_part       LIKE s_part,
    lv_anonymised TYPE boole_d,
    lv_chdat      TYPE bu_chdat.
  DATA:
    lt_partner    TYPE TABLE OF  bu_partner.

  LOOP AT pt_dsg_trigger ASSIGNING <dsg_trigger> ##GEN_OK.
    TRY.
        CASE <dsg_trigger>-type.
          WHEN 'C'. "Contract
            zcl_vcd_fkk_contract=>convert_id_erw_to_id( EXPORTING iv_contract_erw = CONV #( <dsg_trigger>-value )
                                                        IMPORTING ev_contract     = DATA(lv_contract_id)
                                                                  ev_abrkat       = DATA(lv_abrkat) ).
*           Vertrag Zuordnung Sachbearbeiter lesen -> Sachbearbeiterzuordnung muss gelöscht werden
            IF lv_abrkat IS NOT INITIAL.
              lt_abrkat = VALUE #( ( sign = 'I' option = 'EQ' low = lv_abrkat ) ).
            ENDIF.
            PERFORM check_partner_is_anonymised USING    <dsg_trigger>-value
                                                         <dsg_trigger>-type
                                                CHANGING lv_anonymised lv_chdat.
            IF lv_anonymised EQ abap_true.
              "AHV-Nr. &1 wurde am &2 bereits anonymisiert!
              MESSAGE w154(zvcd2) WITH <dsg_trigger>-value lv_chdat INTO lv_dummy.
              grf_app_log->add_message( i_probclass = c_probclass_high ).
              CONTINUE.
            ENDIF.
            SELECT * FROM zvcd_contr_sachb
              APPENDING TABLE pt_contr_sachb
              WHERE contract_id EQ lv_contract_id
              AND   abrkat      IN lt_abrkat.
          WHEN 'A'. "AHV-Nummer
            PERFORM check_partner_is_anonymised USING    <dsg_trigger>-value
                                                         <dsg_trigger>-type
                                                CHANGING lv_anonymised lv_chdat.
            IF lv_anonymised EQ abap_true.
              "AHV-Nr. &1 wurde am &2 bereits anonymisiert!
              MESSAGE w084(zvcd2) WITH <dsg_trigger>-value lv_chdat INTO lv_dummy.
              grf_app_log->add_message( i_probclass = c_probclass_high ).
              CONTINUE.
            ENDIF.
            SELECT partner
              INTO TABLE lt_partner
              FROM but0id
              WHERE type = 'ZV_AHV'
              AND   idnumber = <dsg_trigger>-value ##WARN_OK.
            IF sy-subrc EQ 0.
*             Prüfen, ob Partner schon anonymisiert wurde
              LOOP AT lt_partner ASSIGNING FIELD-SYMBOL(<ahv_part>).
                ls_part = VALUE #( sign = 'I' option = 'EQ' low = <ahv_part> ).
                COLLECT ls_part INTO pt_part.
              ENDLOOP.
            ELSE.
              MESSAGE w004(fsbp_tc) WITH 'ZV_AHV' <dsg_trigger>-value INTO lv_dummy.
              "Keinen Partner zu Identifikationsnummer &1 und Nummerntyp &2 gefunden
              grf_app_log->add_message( i_probclass = c_probclass_high ).
            ENDIF.
          WHEN 'S'. "Sozialversicherungsnummer
            PERFORM check_partner_is_anonymised USING   <dsg_trigger>-value
                                                        <dsg_trigger>-type
                                               CHANGING lv_anonymised lv_chdat.
            IF lv_anonymised EQ abap_true.
              "Soz-Nr. &1 wurde am &2 bereits anonymisiert!
              MESSAGE w085(zvcd2) WITH <dsg_trigger>-value lv_chdat INTO lv_dummy.
              grf_app_log->add_message( i_probclass = c_probclass_high ).

              CONTINUE.
            ENDIF.
            SELECT partner
              INTO TABLE lt_partner
              FROM but0id
              WHERE type = 'ZV_SOZ'
              AND   idnumber = <dsg_trigger>-value ##WARN_OK.
            IF sy-subrc EQ 0.
*             Prüfen, ob Partner schon anonymisiert wurde
              LOOP AT lt_partner ASSIGNING FIELD-SYMBOL(<soz_part>).
                ls_part = VALUE #( sign = 'I' option = 'EQ' low = <soz_part> ).
                COLLECT ls_part INTO pt_part.
              ENDLOOP.
            ELSE.
              MESSAGE w004(fsbp_tc) WITH 'ZV_SOZ' <dsg_trigger>-value INTO lv_dummy.
              "Keinen Partner zu Identifikationsnummer &1 und Nummerntyp &2 gefunden
              grf_app_log->add_message( i_probclass = c_probclass_high ).
            ENDIF.
        ENDCASE.
      CATCH zcx_vcd_appl_error INTO lx_error ##NO_HANDLER.
    ENDTRY.
  ENDLOOP.

* Falls kein Personen zu anonymisieren -> Dummy Eintrag in PT_PART einfügen,
* damit keine Person anonymisiert wird. Bei leerer Select-Option würden sämtliche Partner anonymisiert
  IF pt_part IS INITIAL.
    ls_part = VALUE #( sign = 'I' option = 'EQ' low = space ).
    COLLECT ls_part INTO pt_part.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_PARTNER_IS_ANONYMISED
*&---------------------------------------------------------------------*
*       Prüfen, ob Partner bereits anonymisiert wurde
*----------------------------------------------------------------------*
FORM check_partner_is_anonymised  USING    pv_value      TYPE zvcd_tr_value
                                           pv_type       TYPE zvcd_tr_type
                                  CHANGING cv_anonymised TYPE boole_d
                                           cv_chdat      TYPE bu_chdat.
  CLEAR: cv_anonymised.

* Lesen Einträge mit Partner und Stati A "Wurde anonymisiert" oder S "Status wurde an BUTOS gemeldet"
  SELECT SINGLE chdat
    FROM zvcd_dsg_trigger
    INTO cv_chdat
    WHERE type  = pv_type
    AND value EQ pv_value
    AND ( status EQ 'A'  OR status EQ 'S' ).                "#EC WARNOK

  IF sy-subrc EQ 0.
    cv_anonymised = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_TRIGGER_TO_STATUS_A
*&---------------------------------------------------------------------*
*       Verarbeitete Triggereinträge in Tabelle ZVCD_DSG_TRIGGER
*       auf Status A "Wurde anonymisiert" setzen
*----------------------------------------------------------------------*
FORM update_trigger_to_status_a USING pt_dsg_trigger TYPE tt_dsg_trigger  ##NEEDED
     RAISING zcx_vcd_appl_error.

* Jede einzelne Zeile updaten
  DATA: ls_guid TYPE sysuuid_c,
        lt_guid LIKE TABLE OF ls_guid.

  LOOP AT pt_dsg_trigger ASSIGNING FIELD-SYMBOL(<dsg_trigger2>) ##GEN_OK.
    TRY.
*       Statusupdate auf A "Wurde anonymisiert"
        UPDATE zvcd_dsg_trigger
          SET status = 'A'
              gpart  = <dsg_trigger2>-gpart
              chdat  = sy-datum
              chtim  = sy-uzeit
          WHERE guid  = <dsg_trigger2>-guid
            AND type  = <dsg_trigger2>-type
            AND value = <dsg_trigger2>-value.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE e083(zvcd2) WITH <dsg_trigger2>-guid INTO lv_dummy.
          "Statusänderung in ZVCD_DSG_TRIGGER für GUID &1 nicht erfolgreich
          zcx_vcd_appl_error=>raise_ex( ).
        ENDIF.
        COLLECT <dsg_trigger2>-guid INTO lt_guid.

      CATCH zcx_vcd_appl_error INTO lx_error ##NO_HANDLER.
        zcx_vcd_appl_error=>raise_ex( ).
    ENDTRY.
  ENDLOOP.

* Nur ausgeben wenn auch etwas in der internen Tabelle ist
  IF pt_dsg_trigger IS NOT INITIAL.
    LOOP AT lt_guid ASSIGNING FIELD-SYMBOL(<fs_guid>).
      MESSAGE i082(zvcd2) WITH <fs_guid> 'A' INTO lv_dummy.
      "Einträge in ZVCD_DSG_TRIGGER mit GUID &1 wurden auf Status &2 gesetzt
      grf_app_log->add_message( i_probclass = c_probclass_high ).
    ENDLOOP.
  ENDIF.

ENDFORM.
