*&---------------------------------------------------------------------*
*&  Include           ZVCD_CONSTANTS
*&---------------------------------------------------------------------*

************************************************************************
*****     Bitte Beachten !                                         *****
************************************************************************

* In diesem Include werden alle Programm/Funktionsübergreifenden Konstanden
* Vorsicht bei Änderungen! Es sind potenziell alle Programme betroffen die
* dieses Include beinhalten.

* Konstanten sollen wie folgt benannt werden:
* C_VVVVV_NNNNN: wobei
* VVVVV = Name/Typ der Variablen
* NNNNN = Name der Ausprägung.

* Der Zusatz  ist notwendig um die Fehleranzeige bei der erweiterten
* Syntaxprüfung zu unterdrücken..

************************************************************************
* Änderungen
************************************************************************
* Nr.  Datum      Name        Änderung
* @001 20.11.2013 MMAN        Task 5569: Anpassung Vertragsnr. für DIGIS 10-stellig
* @002 03.12.2014 SNI2        Task 6538: Mahnverfahren FES+
* @003 23.09.2020 BST8        PBI 280146 Prüfung Kombination IBAN/QR-IBAN Zahlweg mit der IBAN
************************************************************************

CONSTANTS ##needed:
  c_aktyp_create                TYPE aktyp_kk VALUE '01',
  c_aktyp_change                TYPE aktyp_kk VALUE '02',
  c_aktyp_modify                TYPE aktyp_kk VALUE '04',
  c_chind_insert                TYPE bu_chind     VALUE 'I',
  c_chind_modify                TYPE bu_chind     VALUE 'M',
  c_chind_update                TYPE bu_chind     VALUE 'U',
  c_chind_delete                TYPE bu_chind     VALUE 'D',
  c_true(000001)                TYPE c     VALUE 'X',
  c_false(000001)               TYPE c     VALUE ' ',
  c_no_data(000001)             TYPE c     VALUE '/',
  c_null(000002)                TYPE c     VALUE '0',
  c_num_false(000001)           TYPE n     VALUE '0',
  c_num_true(000001)            TYPE n     VALUE '1',
  c_divider                     TYPE symsgv      VALUE '--------------------',
  c_date_infinity               TYPE sy-datum    VALUE '99991231',
  c_x(1)                        TYPE c     VALUE 'X',
  c_boole_true                  TYPE boole VALUE 'X',
  c_boole_false                 TYPE boole VALUE 'X',
  c_negative                    TYPE c     VALUE '-',
  c_positive                    TYPE c     VALUE '+',
  c_land1                       TYPE land1 VALUE 'CH',
  c_string_test                 TYPE c     VALUE 'S',
  c_max_valut                   TYPE n     VALUE '5',
  c_appl_vers                   TYPE c     VALUE 'V',

* Länder
  c_schweiz                     TYPE land1 VALUE 'CH',
  c_liechtenstein               TYPE land1 VALUE 'LI',

* error constants
  c_msgty_error                 TYPE msgty VALUE 'E',
  c_msgty_info                  TYPE msgty VALUE 'I',
  c_msgty_warning               TYPE msgty VALUE 'W',
  c_msgty_success               TYPE msgty VALUE 'S',
  c_msgty_abort                 TYPE msgty VALUE 'A',


* problem class
  c_probclass_very_high         TYPE bal_s_msg-probclass VALUE '1',
  c_probclass_high              TYPE bal_s_msg-probclass VALUE '2',
  c_probclass_medium            TYPE bal_s_msg-probclass VALUE '3',
  c_probclass_low               TYPE bal_s_msg-probclass VALUE '4',
  c_probclass_none              TYPE bal_s_msg-probclass VALUE ' ',

* Belegtyp
  c_bltyp_gsfnr_storno          TYPE bltp1_vk VALUE '99',
  c_bltyp_zahlplan              TYPE bltp1_vk VALUE '30',
  c_bltyp_nebenbuch             TYPE bltp1_vk VALUE '20',
  c_bltyp_umbuch                TYPE bltp1_vk VALUE '22',
  c_bltyp_hb                    TYPE bltp1_vk VALUE '23',

* Belegtyp am Belegkopf
  c_bltyp_kkk                   TYPE bltyp_kk VALUE '1',  " Bltyp Keine Konto Korrent Positionen

* Ausgleichs information
  c_aginf_ebpow                 TYPE aginf_kk VALUE '1',  "  Beleg hat auch echte Positionen ohne Wiederholung ausg.
  c_aginf_ebpmw                 TYPE aginf_kk VALUE '2',  " Beleg hat auch echte Positionen mit Wiederholung ausg.
  c_aginf_augst                 TYPE augst_kk VALUE '9',  " Beleg ist ausgeglichen
  c_aginf_open                  TYPE augst_kk VALUE ' ',     " Beleg ist offen

* Belegstatus MyLife
  c_bel_stat_cleared            TYPE zvcd_bel_status VALUE '2',  " Beleg ist ausgeglichen
  c_bel_stat_partclr            TYPE zvcd_bel_status VALUE '3',  " Beleg ist teilausgeglichen
  c_bel_stat_open               TYPE zvcd_bel_status VALUE '1',  " Beleg ist offen

* Applikation
  c_applk_vers                  TYPE applk_kk VALUE 'V',               "Versicherung

* Buchungskreis
  c_bukrs_rasl                  TYPE bukrs VALUE 'CH01',                    "Schweiz
  c_bukrs_sls2s                 TYPE bukrs VALUE 'V010',               "SL 2te Saule
  c_bukrs_bvgsl                 TYPE bukrs VALUE 'V020',             "BVG Swiss Life
  c_bukrs_sszsl                 TYPE bukrs VALUE 'V030',  "Sammelstiftung Zusatz. SL
  c_bukrs_bvst                  TYPE bukrs VALUE 'V040',                       "BVST
  c_bukrs_prem                  TYPE bukrs VALUE 'V060',           "Business Premium
  c_bukrs_fes                   TYPE bukrs VALUE 'V070',           "Firmeneigene Stifungen
  c_bukrs_pool                  TYPE bukrs VALUE 'V090',
  c_bukrs_vors                  TYPE bukrs VALUE 'V+',

* Buchungskreisgruppen
  c_ccode_leistung              TYPE bukrs VALUE 'ALLE',

* Kostenrechnungskreis
  c_kokrs_ch01                  TYPE kokrs VALUE 'CH01',

* Kontenplan
  c_ktopl_zv00                  TYPE ktopl VALUE 'ZV00',      " Kontenplan Vorsorge

* Ausgleichsgrund
  c_ag_pflege_dialog            TYPE augrd_kk VALUE '08' ,      " Kontenpflege
  c_ag_masch_ausglch            TYPE augrd_kk VALUE '15' ,  " Maschinelles Ausgleichen
  c_ag_ruecklaufer              TYPE augrd_kk VALUE '10' ,        " Rückläufer
  c_ag_rueckn_ausgl             TYPE augrd_kk VALUE '11' ,  " Rücknahme Ausgleich
  c_ag_storno                   TYPE augrd_kk VALUE '05' ,            " Storno
  c_ag_eing_zahlung             TYPE augrd_kk VALUE '01' ,   " Eingangszahlung
  c_ag_ausg_zahlung             TYPE augrd_kk VALUE '02' ,   " Ausgangszahlung
  c_ag_masch_ausb               TYPE augrd_kk VALUE '14' ,        " Ausbuchung
  c_ag_ausbuchung               TYPE augrd_kk VALUE '04' ,        " Ausbuchung
  c_ag_umbuchung                TYPE augrd_kk VALUE '03' ,  " Posten Transferieren

*- I_PROCESS Werte (siehe Exit 0020)
  c_proc_storno                 TYPE c VALUE '1',                          " Storno
  c_proc_ausgrn                 TYPE c VALUE '2',                " Rückn. Ausgleich
  c_proc_rcklfr                 TYPE c VALUE '3',                      " Rücklaufer

* Guthaben Liste - states
  c_state_free                  TYPE cfc_state VALUE '01',
  c_state_in_work               TYPE cfc_state VALUE '02',
  c_state_done                  TYPE cfc_state VALUE '03',
  c_state_error                 TYPE cfc_state VALUE '04',

* Statistikkennzeichen
  c_stakz_gebuehr               TYPE stakz_kk VALUE 'G',

* Verrechnugnsartem
  c_verart_man_kontenpflege     TYPE verart_kk VALUE '03',
  c_verart_masch_kontenpflege   TYPE verart_kk VALUE '04',

* Schlüssel Verwendungszweck
  c_key_vertrag                 TYPE zvcd_keygrp VALUE 'VT',
  c_key_soznr                   TYPE zvcd_keygrp VALUE 'SN',
  c_key_bvgant                  TYPE zvcd_keygrp VALUE 'BV',
  c_nachkomma                   TYPE string VALUE '\.\d\d|\,\d\d',
  c_type_soz                    TYPE bu_id_type VALUE 'ZV_SOZ',
  c_type_ahv                    TYPE bu_id_type VALUE 'ZV_AHV',
  c_type_ver                    TYPE bu_id_type VALUE 'ZV_VER',
  c_bukrs_ch01                  TYPE bukrs VALUE 'CH01',
  c_ktopl_ch01                  TYPE ktopl VALUE 'CH01',

*Schlüssel Suchmuster
  c_key_patt_vertr              TYPE zvcd_patname VALUE 'VERTRNR',
  c_key_patt_soznr              TYPE zvcd_patname VALUE 'SOZVNR',

* Buchungsbereiche
  c_buber_masch_ausgl           TYPE buber_kk VALUE '1025',
  c_buber_ausgl_rueck           TYPE buber_kk VALUE 'VZ61',
  c_buber_mwst_konto            TYPE buber_kk VALUE '0010',
  c_buber_intercomp             TYPE buber_kk VALUE '0030',
  c_buber_rundung               TYPE buber_kk VALUE '0040',
  c_buber_hb_buchen             TYPE buber_kk VALUE 'VZ51',
  c_buber_verz                  TYPE buber_kk VALUE 'VZ52',
  c_buber_mwskz                 TYPE buber_kk VALUE 'VZ53',
  c_buber_unif                  TYPE buber_kk VALUE 'VZ55',
  c_buber_sperren               TYPE buber_kk VALUE 'VZ56',
  c_buber_freigabe              TYPE buber_kk VALUE 'VZ57',
  c_buber_eizas                 TYPE buber_kk VALUE 'VZ64',
  c_buber_guv                   TYPE buber_kk VALUE 'VC21',
  c_buber_zuskont               TYPE buber_kk VALUE 'VZ66',
  c_buber_abwvert               TYPE buber_kk VALUE 'VZ67',

*c_muss(1)           TYPE c        VALUE 'M',

* Herkunftsschlüssel des Belegs
  c_herkf_ma                    TYPE herkf_kk VALUE '04',  " masch. Ausgleich
  c_hkft_storno                 TYPE herkf_kk VALUE '02',  " Storno
  c_hkft_zahllauf               TYPE herkf_kk VALUE '06',
  c_hkft_zinsbeleg              TYPE herkf_kk VALUE '07',  " Zinsbeleg aus Saldenverzinsung
  c_hkft_sollst                 TYPE herkf_kk VALUE 'V1',
  c_hkft_kkvor                  TYPE herkf_kk VALUE 'V8',
  c_hkft_rnausgl                TYPE herkf_kk VALUE '09',  " Ausgleich Rucknähme
  c_hkft_zahlstpl               TYPE herkf_kk VALUE '05',  " Zahlungsstapel
  c_hkft_rueckstpl              TYPE herkf_kk VALUE '08',  " Rücklaüferstapel
  c_hkft_ratenplan              TYPE herkf_kk VALUE '12',  " Ratenplan
  c_hkft_storno_zusatz          TYPE herkf_kk VALUE '52',  " Zusatzbuchung bei Storno oder Rückläufer


  " Kontenstand: Zeilenaufbau / Variante
  c_var_interne                 TYPE varnr_zkk VALUE 'Z01',             " Interne
  c_var_kunden_01               TYPE varnr_zkk VALUE 'K01',        " Kundendienst

  " Kontenstand: Listtyp für die Kontenstandanzeige
  c_lstyp_op                    TYPE lstyp_kk VALUE '2000',       " Alle offene Posten
  c_lstyp_all                   TYPE lstyp_kk VALUE '1000',  " Alle offene & ausgeglichene Posten

  " Kontenstand: Sortiervariante für die Kontenstandanzeige
  c_srvar_kv_std                TYPE srvar_kk VALUE 'A01',           " KV Standard

* Zahlwege
  c_zahlweg_iban_inland_ezag    VALUE 'A',  "IBAN Zahlung Inland (Post)
  c_zahlweg_esr_ezag            VALUE 'B',  "ESR Post (Post)
  c_zahlweg_inland_anw_ezag     VALUE 'C',  "Postanweisung Inland (Post)
  c_zahlweg_ausland_anw_ezag    VALUE 'D',  "Postanweisung Ausland (Post)
  c_zahlweg_ausland_post_ezag   VALUE 'E',  "Postkonto Ausland (Post)
  c_zahlweg_ausland_bank_ezag   VALUE 'F',  "Bankkonto Ausland (Post)
  c_zahlweg_inland_postkto_ezag VALUE 'G',  "Bankkonto Ausland (Post)
  c_zahlweg_iban_dta            VALUE 'H',  "IBAN Zahlung In-/Ausland (Bank)
  c_zahlweg_inland_dta          VALUE 'I',  "Zahlung Inland (Bank)
  c_zahlweg_ausland_dta         VALUE 'J',  "Zahlung Ausland (Bank)
  c_zahlweg_qr_post             VALUE 'K',  "QR Zahlung (Post)
  c_zahlweg_qr_bank             VALUE 'Q',  "QR Zahlung (Bank)

* Zahlwege - XPlan
  c_xp_pymet_esr                VALUE 'E',                                   " ESR
  c_xp_pymet_panweisung         VALUE 'P',                  "Post anweisung
  c_xp_pymet_iban               VALUE '2',
  c_xp_pymet_ueberweisung       VALUE '5',
  c_xp_pymet_post               VALUE '9',
  c_xp_pymet_qriban             VALUE 'Q', "@003

* Belegarten
  c_blart_verzinsung            TYPE blart_kk VALUE '90',
  c_blart_ein_zahlung           TYPE blart_kk VALUE '30',
  c_blart_manuell               TYPE blart_kk VALUE '50',
  c_blart_xplan                 TYPE blart_kk VALUE 'XP',
  c_blart_unifinanz             TYPE blart_kk VALUE 'UN',
  c_blart_nvs                   TYPE blart_kk VALUE 'NV',
  c_blart_ss                    TYPE blart_kk VALUE '91',      "Stiller Storno
  c_blart_ausgleich             TYPE blart_kk VALUE '60',      "Ausgleichsbuchungen
  c_blart_rucknahme             TYPE blart_kk VALUE '73',      "Rücknahme Ausgleich
  c_blart_storno                TYPE blart_kk VALUE '71',      "Storno
  c_blart_mig                   TYPE blart_kk VALUE 'MI',
  c_blart_ausbuchung            TYPE blart_kk VALUE '83',
  c_blart_asem                  TYPE blart_kk VALUE 'AS',
  c_blart_mitversicherung       TYPE blart_kk VALUE 'MV',
  c_blart_digis                 TYPE blart_kk VALUE 'DI',
  c_blart_ratenplan_gebuehr     TYPE blart_kk VALUE '93',

  c_infoc_type_zahlung          TYPE infcotype_kk VALUE 'Z001',   " Zahlungen
  c_infoc_type_ausz_rl          TYPE infcotype_kk VALUE 'Z002',   " Auszahlungsrückläufer
  c_infoc_type_print_doc        TYPE infcotype_kk VALUE 'Z003',   " Druck Stück
  c_infoc_type_kobu             TYPE infcotype_kk VALUE 'Z004',   " Zahlungen KOBU

* Profitcenter
  c_prctr_vt_kver               TYPE prctr VALUE 'VT-KVER',

* Buchungskreis CpD's
  c_cpd_rasl                    TYPE bukrs VALUE '9000',                     "Schweiz
  c_cpd_sls2s                   TYPE bukrs VALUE '9010',                "SL 2te Saule
  c_cpd_bvgsl                   TYPE bukrs VALUE '9020',              "BVG Swiss Life
  c_cpd_sszsl                   TYPE bukrs VALUE '9030',   "Samel Stiftung Zusatz. SL

* Mapping Codes Buchungsstoff
  c_gsf_uebertragfmslhn_frmi    TYPE zvcd_map_key  VALUE 'UEBERTRAGFMSLHN_FRMI',

* Vorgesetzter
  c_zvcd_vorgesetzte            TYPE domname VALUE 'ZVCD_VORGESETZTE',

* VO Typen
  c_vo_btrgskto                 TYPE insobjecttypc_md VALUE '01',
  c_vo_reszhlgkto               TYPE insobjecttypc_md VALUE '02',
  c_vo_vrtr_abwcklng            TYPE insobjecttypc_md VALUE '03',
  c_vo_agbr                     TYPE insobjecttypc_md VALUE '10',
  c_vo_freie_mittel             TYPE insobjecttypc_md VALUE '11',
  c_vo_ueberschuss              TYPE insobjecttypc_md VALUE '12',
  c_vo_aktien_sl                TYPE insobjecttypc_md VALUE '16',
  c_vo_cpd                      TYPE insobjecttypc_md VALUE '90',
  c_vo_pers                     TYPE insobjecttypc_md VALUE '50',
  c_vo_liq                      TYPE insobjecttypc_md VALUE 'L1',
  c_vo_beit_konto               TYPE insobjecttypc_md VALUE '01',
  c_vo_res_zahl                 TYPE insobjecttypc_md VALUE '02',
  c_vo_sifo                     TYPE insobjecttypc_md VALUE '81',
  c_vo_qst                      TYPE insobjecttypc_md VALUE '80',
  c_vo_ueberschuss_rentner      TYPE insobjecttypc_md VALUE '05',  "@0005
  c_vo_praemien                 TYPE insobjecttypc_md VALUE '20',  "Abgetretene Versicherungen 3b
  c_vo_mv_abrechnung            TYPE insobjecttypc_md VALUE '70',
  c_vo_mv_aenderung             TYPE insobjecttypc_md VALUE '74',

* Beziehungstype VO
  c_bez_iobj_vertrag            TYPE zvcd_bez_iobj VALUE 'V',
  c_bez_iobj_person             TYPE zvcd_bez_iobj VALUE 'P',
  c_bez_iobj_dritte             TYPE zvcd_bez_iobj VALUE 'D',
  c_bez_iobj_mitvers            TYPE zvcd_bez_iobj VALUE 'M',

* Feldsteuerung VwZwck.
  c_muss                        TYPE zvcd_fstatn VALUE 'M',
  c_kann                        TYPE zvcd_fstatn VALUE 'K',

* Geschäftsfall Art
  c_gsf_art_leistung            TYPE zvcd_gsf_art VALUE 'L',

* korrespondenz Typ
  c_cotyp_ratenplan             TYPE cotyp VALUE '0005',
  c_cotyp_zinsna                TYPE cotyp VALUE 'ZINN',
  c_cotyp_zinsna_old            TYPE cotyp VALUE '0007',
  c_cotyp_vertragsauszug        TYPE cotyp VALUE 'V032',
  c_cotyp_zahlbest              TYPE cotyp VALUE 'ZAHL',

* Währungen
  c_waers_chf                   TYPE waers VALUE 'CHF',
  c_waers_eur                   TYPE waers VALUE 'EUR',

* Nummerkreis
  c_nr_adext                    TYPE nrobj VALUE 'ZV_ADEXT',
  c_nr_spoolid                  TYPE nrobj VALUE 'ZV_SPOOLID',
  c_nr_man_gsfnr                TYPE nrobj VALUE 'ZV_M_GSFNR',
  c_nr_fscd                     TYPE nrobj VALUE 'ZV_FSCD',              "für XBLNR
  c_nr_man_oid                  TYPE nrobj VALUE 'ZV_M_OID',             "für XBLNR

* GP Art
  c_bpkind_person               TYPE bu_bpkind VALUE '3000',
  c_bkind_arbtsgb               TYPE bu_bpkind VALUE '2000',
  c_bkind_sachbrb               TYPE bu_bpkind VALUE '4000',
  c_bkind_ze                    TYPE bu_bpkind VALUE '5000',

* GP Gruppe
  c_bpgroup_person              TYPE bu_group VALUE '3000',
  c_bpgroup_ze                  TYPE bu_group VALUE '5000',
  c_bpgroup_zexp                TYPE bu_group VALUE '5100',

* GP Berechtigungsgruppe
  c_augrp_person                TYPE bu_augrp VALUE '3000',
  c_augrp_ze                    TYPE bu_augrp VALUE '5000',

* GP Type
  c_bptype_person               TYPE bu_type VALUE '1',
  c_bptype_orga                 TYPE bu_type VALUE '2',

* Fixed GP
  c_gp_sifo                     TYPE bu_partner VALUE '0000008100',        "SIFO Partner
  c_gp_qst                      TYPE bu_partner VALUE '8000',             "QST Partner
  c_gp_ze                       TYPE bu_partner VALUE '5000',             "Zahlungsempfänger

* AnwendungsLog
  c_object_rept                 TYPE balobj_d     VALUE 'ZVCD_REPORTS',
  c_object_srvc                 TYPE balobj_d     VALUE 'ZVCD_SERVICES',
  c_subobj_reset                TYPE balsubobj    VALUE 'ZVCD_MYSEARCH_RESET',
  c_subobj_mahn                 TYPE balsubobj    VALUE 'ZVCD_MYSEARCH_MAHN',
  c_subobj_buch                 TYPE balsubobj    VALUE 'ZVCD_MYSEARCH_BUCH',
  c_subobj_masp                 TYPE balsubobj    VALUE 'ZVCD_MYSEARCH_MASP',
  c_subobj_zins                 TYPE balsubobj    VALUE 'ZVCD_MYSEARCH_ZINS',
  c_subobj_jbtr                 TYPE balsubobj    VALUE 'ZVCD_JAHR_BTR',
  c_subobj_mig                  TYPE balsubobj    VALUE 'ZVCD_MIG',
  c_subobj_zekl                 TYPE balsubobj    VALUE 'ZVCD_ZE_KL',
  c_subobj_asem                 TYPE balsubobj    VALUE 'ZVCD_ASEM',
  c_subobj_archiv               TYPE balsubobj    VALUE 'ZVCD_ARCHIV',
  c_subobj_unif                 TYPE balsubobj    VALUE 'ZVCD_UNIF_TRAMAG',
  c_subobj_begl                 TYPE balsubobj    VALUE 'ZVCD_BEGL_DTA_EZAG',
  c_subobj_orig                 TYPE balsubobj    VALUE 'ZVCD_CONTRACT_ORIGIN',
  c_subobj_cons                 TYPE balsubobj    VALUE 'ZVCD_KONSOLIDIERUNG',
  c_subobj_ktou                 TYPE balsubobj    VALUE 'ZVCD_KTOUBERWACHUNG',
  c_subobj_tafista              TYPE balsubobj    VALUE 'ZVCD_TAFISTA',
  c_subobj_dsg                  TYPE balsubobj    VALUE 'ZVCD_DSG',
  c_subobj_ausbwsdiff           TYPE balsubobj    VALUE 'ZVCD_AUSB_WSDIFF',


* Sperretyp
  c_proid_mahn                  TYPE proid_kk VALUE '01',
  c_proid_buch                  TYPE proid_kk VALUE '09',

* Sperrobjekt
  c_lotyp_vo                    TYPE lotyp_kk VALUE '22',

* Sperrgrund
  c_lockr_verl                  TYPE lockr_kk VALUE '4',


* Windowmode
  c_wmode_create                TYPE wmode_kk VALUE '01',
  c_wmode_change                TYPE wmode_kk VALUE '02',
  c_wmode_display               TYPE wmode_kk VALUE '03',
  c_wmode_check                 TYPE wmode_kk VALUE '07',

* Mahnungs Variante
  c_mvari_11                    TYPE mvari_vk VALUE '10',

* Mahnverfahren
  c_mahnv_11                    TYPE mahnv_kk VALUE '10',
  c_mahnv_40                    TYPE mahnv_kk VALUE '40',

* Mahnstufe
  c_mahns_os                    TYPE mahns_kk VALUE '02',
*c_mahns_vgm TYPE mahns_kk VALUE '03',
  c_mahns_vgm                   TYPE mahns_kk VALUE '05',           " Task 5798 SNI2

* Mahnstufentyp
  c_mstyp_01                    TYPE mstyp_kk VALUE '01',
  c_mstyp_02                    TYPE mstyp_kk VALUE '02',
  c_mstyp_03                    TYPE mstyp_kk VALUE '03',
  c_mstyp_04                    TYPE mstyp_kk VALUE '04',
  c_mstyp_05                    TYPE mstyp_kk VALUE '05',
  c_mstyp_06                    TYPE mstyp_kk VALUE '06',
  c_mstyp_07                    TYPE mstyp_kk VALUE '07',
  c_mstyp_08                    TYPE mstyp_kk VALUE '08',
  c_mstyp_09                    TYPE mstyp_kk VALUE '09',
  c_mstyp_10                    TYPE mstyp_kk VALUE '10',
  c_mstyp_11                    TYPE mstyp_kk VALUE '11',
  c_mstyp_99                    TYPE mstyp_kk VALUE '99',
  c_mstyp_v1                    TYPE mstyp_kk VALUE 'V1',
  c_mstyp_v2                    TYPE mstyp_kk VALUE 'V2',
  c_mstyp_va                    TYPE mstyp_kk VALUE 'VA',
  c_mstyp_vx                    TYPE mstyp_kk VALUE 'VX',
  c_mstyp_ze                    TYPE mstyp_kk VALUE 'ZE',            "Bug 5875 SNI2

* Mahnverfahrensstatus
  c_mvsta_running               TYPE mvsta_vk VALUE '00',

* Mahnsperrgrund
  c_dun_reason_1                TYPE mansp_kk VALUE '1',  "Vorübergehender Liquiditätsengpass
  c_dun_reason_2                TYPE mansp_kk VALUE '2',  "     Pendente Mutationen
  c_dun_reason_3                TYPE mansp_kk VALUE '3',  "Zahlung in den nächsten Tagen
  c_dun_reason_4                TYPE mansp_kk VALUE '4',  " Verlängerung Mahnsperre
  c_dun_reason_5                TYPE mansp_kk VALUE '5',  " Sperre Team Insolvenzen
  c_dun_reason_6                TYPE mansp_kk VALUE '6',  "       Vertragskündigung
  c_dun_reason_7                TYPE mansp_kk VALUE '7',  "     Sperre Mahngebühren
  c_dun_reason_8                TYPE mansp_kk VALUE '8',  "Rückwirkende Beitragsrechnung


* Zahlungsgrund
  c_zhlgrd_storno               TYPE zvcd_zagrd VALUE '599',      " Storno auf Res. Zahlungskonto
  c_zhlgrd_btrgsknt             TYPE zvcd_zagrd VALUE '511',    "  Beitragskonto
  c_zhlgrd_aabgreserve          TYPE zvcd_zagrd VALUE '512', " Arbeitgeberbeitragsreserve
  c_zhlgrd_fzl                  TYPE zvcd_zagrd VALUE '501',         " FZL (Eintritt)
  c_zhlgrd_fzp_neu              TYPE zvcd_zagrd VALUE '507',     "FZP Neu Vertrag
  c_zhlgrd_fzp_einbau           TYPE zvcd_zagrd VALUE '508',  "     FZP Einbau

* Konto für MWSt welches nicht verdichtet werden darf
  c_mwst_konto                  TYPE hkont_kk VALUE '0039010341',

* Address Typen
  c_addr_korr                   TYPE bu_adrkind VALUE 'Z_KORRADR',

* Adressverwendungen
  c_adrkind_deft                TYPE bu_adrkind VALUE 'XXDEFAULT',
  c_adrkind_domi                TYPE bu_adrkind VALUE 'Z_DOMIADR',
  c_zzv_addr_type_domi          TYPE zvcd_gpart_addr_type VALUE 'Z_DOMICILEADDRESS',
  c_zzv_korr_type_primary       TYPE zvcd_korrespondez_typ VALUE 'PRIMARY',

* Unifinanz: KVRW Interne Vertrags-nummer Kreis
* Beginn @001
*c_vrtr_unfnz1 TYPE zvcd_vertr_nr VALUE 'CDV000',
*c_vrtr_unfnz2 TYPE zvcd_vertr_nr VALUE 'CDV999',
*c_vrtr_unfnz3 TYPE zvcd_vertr_nr VALUE 'CDCH01',
  c_vrtr_unfnz1                 TYPE zvcd_vertr_nr_erw VALUE 'CDV000',
  c_vrtr_unfnz2                 TYPE zvcd_vertr_nr_erw VALUE 'CDV999',
  c_vrtr_unfnz3                 TYPE zvcd_vertr_nr_erw VALUE 'CDCH01',
* Ende @001

* Geschäftsfallart
  c_zvcd_gf_art_kulanz_allg     TYPE zvcd_gf_art VALUE 'K',  "Kulanz Allgemein
  c_zvcd_gf_art_kulanz_zins     TYPE zvcd_gf_art VALUE 'L',  "Kulanz Zinsen
  c_zvcd_gf_art_kulanz_geb      TYPE zvcd_gf_art VALUE 'M',  "Kulanz Gebühren
  c_zvcd_gf_art_abschr_guth     TYPE zvcd_gf_art VALUE 'R',  "Abschreibung Guthaben
  c_zvcd_gf_art_abschr_aust     TYPE zvcd_gf_art VALUE 'S',  "Abschreibung Ausstand "@005

* Art der statistischen Belegposition
  c_zvcd_stat_kennz_ratenplan   TYPE stakz_kk VALUE 'R',  "Ratenplan

* MySearch Art der Datenlieferung
  c_lauft_mahn_sp               TYPE zvcd_lauft VALUE 'S',       "Mahnsperre
  c_lauft_zins                  TYPE zvcd_lauft VALUE 'Z',            "Zinssatz
  c_lauft_mahn                  TYPE zvcd_lauft VALUE 'M',           "Mahndaten
  c_lauft_buch                  TYPE zvcd_lauft VALUE 'B',           "Buchungen

* GUIDs für MySearch /My Life
  c_btyp_kopf                   TYPE zvcd_btyp VALUE '0',             "Belegkopf
  c_btyp_nbpos                  TYPE zvcd_btyp VALUE '1',   "Belegposition Nebenbuch
  c_btyp_hbpos                  TYPE zvcd_btyp VALUE '2',    "Belegposition Hauptbuch
  c_btyp_saldo                  TYPE zvcd_btyp VALUE '3',    "Belegposition Hauptbuch

* Send Status for MySearch/My Life
  c_sendstat_to_send            TYPE zvcd_sendstatus VALUE ' ',   "Zu versenden
  c_sendstat_sent               TYPE zvcd_sendstatus VALUE '1',   "Versandt
  c_sendstat_error              TYPE zvcd_sendstatus VALUE '9',   "Fehler
  c_sendstat_ignore             TYPE zvcd_sendstatus VALUE 'X',   "Fehler

* Geschäftspartner Rolle GP-rolle
  c_gp_rol_zvcd01               TYPE bu_partnerrole VALUE 'ZVCD01',   "SL Mitarbeiter
  c_gp_roll_kkk                 TYPE bu_partnerrole VALUE 'MKK',

* Hauptvorgänge
  c_hvorg_1080                  TYPE hvorg_kk VALUE '1080',         "Verwendung
  c_hvorg_1000                  TYPE hvorg_kk VALUE '1000',  "Beitragsabrechnung
  c_hvorg_zins                  TYPE hvorg_kk VALUE '2100',


* Teilvorgänge
  c_tvorg_0190                  TYPE tvorg_kk VALUE '0190',  "Auszahlung Kapital
  c_tvorg_0100                  TYPE tvorg_kk VALUE '0100',   "Beitragsrechnung
  c_tvorg_kkaugl                TYPE tvorg_kk VALUE '8931',  "Saldovortrag Eröffnung
  c_tvorg_svaugl                TYPE tvorg_kk VALUE '8910',  "Saldovortrag Ausgleich
  c_tvorg_ausb_guth             TYPE tvorg_kk VALUE '0625',  "Ausbuchung Guthaben
  c_tvorg_ausb_ford             TYPE tvorg_kk VALUE '0626',  "Ausbuchung Forderung

* Laufzeit Protokoll Massenläufe
  c_versch_aldate               TYPE align_number_kk VALUE 365,


* Transaktions Typ
  c_transtyp_01                 TYPE zvcd_trans_typ VALUE '01',  "  Nebenbuch Buchung
  c_transtyp_02	                TYPE zvcd_trans_typ VALUE '02',  "Hauptbuchbuchung (mit CH01)
  c_transtyp_03	                TYPE zvcd_trans_typ VALUE '03',  "Nebenbuch Umbuchung
  c_transtyp_04	                TYPE zvcd_trans_typ VALUE '04',  "Geschäftsfall Storno
  c_transtyp_05	                TYPE zvcd_trans_typ VALUE '05',  "Hauptbuchbuchung ( Vorsorge BUKRS )
  c_transtyp_06	                TYPE zvcd_trans_typ VALUE '06',  "Hauptbuchbuchung ( Unifinanz mit CH01)
  c_transtyp_07	                TYPE zvcd_trans_typ VALUE '07',  "Hauptbuchumbuchung (CH01)

* Mappingschlüssel Zahlplan
  c_mapkey_xplan                TYPE zvcd_mapkey VALUE 'XP',      "Mapping XPlan "ToDo: Muss nach Bereinigung gelöscht werden
  c_mapkey_unifinanz            TYPE zvcd_mapkey VALUE 'UF',  "Mapping XUnifinanz
  c_mapkey_standard             TYPE zvcd_mapkey VALUE 'ST',   "Mapping Standard

* Spezielle Laufidentifikationen
  c_laufi_os1_1                 TYPE laufi VALUE 'OS1',             " Laufidentifikation für Orientierungsschreiben 1/1tel

* Kennzeichen Zinssimulation
  c_inkl_zins_ausdt             TYPE zvcd_inkl_zins VALUE 'A',  " Zinsen bis Ausstellungsdatum simulieren
  c_inkl_zins_kein_zins         TYPE zvcd_inkl_zins VALUE '',  " Keine Zinsen simulieren

* ESR Grund Bestellung aus My Life

  c_esr_grund_01                TYPE zvcd_esr_grund VALUE '01',   "Beitragskonto
  c_esr_grund_02                TYPE zvcd_esr_grund VALUE '02',   "Arbeitgeberbeitrag
  c_esr_grund_03                TYPE zvcd_esr_grund VALUE '03',   "Freizugikeitsleistung
  c_esr_grund_04                TYPE zvcd_esr_grund VALUE '04',   "Rückzahlung WEF
  c_esr_grund_05                TYPE zvcd_esr_grund VALUE '05',   "Wiedereinkauf Scheidung
  c_esr_grund_06                TYPE zvcd_esr_grund VALUE '06',   "Einkauf Vorsorgeleistung
  c_esr_grund_07                TYPE zvcd_esr_grund VALUE '07',   "Einkauf Ausfinanzierung vz. P.
  c_esr_grund_08                TYPE zvcd_esr_grund VALUE '08',   "Durchführungskosten Vorb. WEF
  c_esr_grund_09                TYPE zvcd_esr_grund VALUE '09',    "Durchführungskosten Verpfänd.

* Vertrags Status
  c_vertr_status_act            TYPE zvcd_vertr_status   VALUE 'ACTIVE',  "Vertragsstatus Active
  c_vertr_status_deact          TYPE zvcd_vertr_status VALUE 'INACTIVE',  "Vertragsstatus Inactive

* Gültigkeitsdaten
  c_valid_null                  TYPE bu_central_date_from_di VALUE '00010101',
  c_valid_end                   TYPE bu_central_date_to_di VALUE '99991231',

* Massenaktivitätstypen
  c_aktyp_ma                    TYPE aktyp_kk VALUE '0004',
  c_aktyp_payrun                TYPE aktyp_kk VALUE 'SOLL',

* Massenaktivität Identifikationsmerkmale
  c_laufi_payrun                TYPE laufi_kk VALUE 'XXX',                 "Identifikationsmerkmal für produktiven Sollstellungslauf

* GSFNR Status
  c_gf_stat_process             TYPE zvcd_gf_stat VALUE 'P',
  c_gf_stat_gebucht             TYPE zvcd_gf_stat VALUE 'X',
  c_gf_stat_fehler              TYPE zvcd_gf_stat VALUE ' ',

* Berechtigungsgruppen
  c_augrp_cpd                   TYPE augrp_md VALUE '1000',

* Tagesendverarbeitung Datum für TVARVC/TVARV
  c_name_dat                    TYPE rvari_vnam VALUE 'ZVCD_AKT_DATUM',
  c_type_dat                    TYPE rsscr_kind VALUE 'P',
  c_numb_dat                    TYPE tvarv_numb VALUE '0000',
* Tagesendverarbeitung Zeit für TVARVC/TVARV
  c_name_zeit                   TYPE rvari_vnam VALUE 'ZVCD_AKT_ZEIT',
  c_type_zeit                   TYPE rsscr_kind VALUE 'P',
  c_numb_zeit                   TYPE tvarv_numb VALUE '0000',
* Letzter Zahllauf in Tagesendverarbeitung für TVARVC/TVARV
  c_name_last_payrun            TYPE rvari_vnam VALUE 'ZVCD_LAST_PAYRUN',
  c_type_last_payrun            TYPE rsscr_kind VALUE 'P',
  c_numb_last_payrun            TYPE tvarv_numb VALUE '0000',
*Tagesendverarbeitung Returncode für TVARVC/TVARV
  c_rc                          TYPE sysubrc VALUE 0,

* Selektionstypen für Zahlstapel
  c_selty_zuord                 TYPE seltx_kk VALUE 'Z',

* Positionstypen
  c_postyp_nb                   TYPE zvcd_postyp VALUE 'G',
  c_postyp_hb                   TYPE zvcd_postyp VALUE 'H',

  c_asterisk                    TYPE c VALUE '*',

  c_cont_type_fzp               TYPE zvcd_contract_type VALUE 'FZPL',
  c_cont_type_stnd              TYPE zvcd_contract_type VALUE 'STND',

  c_zero_rate                   TYPE azinssatz VALUE '0.00',

* Rollen MyBuchung, MyAuszahlung und MyTransfer
  c_role_rw                     TYPE zvcd_role VALUE 'R',
  c_role_sv                     TYPE zvcd_role VALUE 'S',
  c_role_kd                     TYPE zvcd_role VALUE '',

* Bereiche für Steuerung statusabhängige Aktionen
  c_area_koausdef               TYPE fkber_short VALUE 'KDEF',
  c_area_koaussim               TYPE fkber_short VALUE 'KINF',
  c_area_verzinsung             TYPE fkber_short VALUE 'VERZ',

* Funktionen
  c_funkt_jahresabschl          TYPE apb_lpd_fcode VALUE 'MACT_V032',
  c_funkt_service_veraufl       TYPE apb_lpd_fcode VALUE 'SRVC_VA',
  c_funkt_service               TYPE apb_lpd_fcode VALUE 'SRVC',
  c_funkt_dokkoaus              TYPE apb_lpd_fcode VALUE 'DOKKOAUS',
  c_funkt_dokos                 TYPE apb_lpd_fcode VALUE 'DOKOS',
  c_funkt_ziserv                TYPE apb_lpd_fcode VALUE 'ZISERV',
  c_funkt_saldserv              TYPE apb_lpd_fcode VALUE 'SALDSERV',
  c_funkt_gvert                 TYPE apb_lpd_fcode VALUE 'GVERT',
  c_funkt_myzinssim             TYPE apb_lpd_fcode VALUE 'MYZINSSIM',
  c_funkt_maverz                TYPE apb_lpd_fcode VALUE 'MACT',
  c_funkt_masimverz             TYPE apb_lpd_fcode VALUE 'MACTSIM',


* Aktionen
  c_action_archiv               TYPE bbp_bapi_waktion VALUE 'ARCHIV',
  c_action_ausschl              TYPE bbp_bapi_waktion VALUE 'AUSSCHL',

* ESR Typen
  c_esr_type                    TYPE zvcd_esr_typ VALUE 'ESR',
  c_besr_type                   TYPE zvcd_esr_typ VALUE 'BESR',
  c_esr_type_srvc(1)            TYPE c VALUE 'E',
  c_besr_type_srvc(1)           TYPE c VALUE 'B',

* Origin aus Buchungsschnittstelle
  c_origin_unif                 TYPE origin_di VALUE 'UNIFINANZ',
  c_origin_digis                TYPE origin_di VALUE 'DIGIS',

* Systemkoordinator
  c_systcoor                    TYPE bu_partner VALUE 'U3FK',           "@002

  c_log_filename_mahn_abgv      TYPE rsbfilenameappserver VALUE 'ZVCD_MAHN_ABGV_D',

  c_intbu_0032                  TYPE intbu_kk VALUE 'Z',
  c_ktosl_bvr                   TYPE ktosl_kk VALUE 'BVR',

* Parallelisierungsobjekte MA
  c_object_vkont                TYPE object_kk VALUE 'VKONT',
  c_object_gpart                TYPE object_kk VALUE 'GPART',

* Email Absender
  c_sender                      TYPE ad_smtpadr VALUE 'no-reply@swisslife.ch' ##NO_TEXT.
