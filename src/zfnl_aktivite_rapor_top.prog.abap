*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_RAPOR_TOP
*&---------------------------------------------------------------------*
CLASS gcl_class DEFINITION DEFERRED.

TABLES: znfl_prj_t_001, proj, znfl_prj_m_001, znfl_prj_m_002, zfnl_aktvt_01, prps.
DATA: go_ctrl     TYPE REF TO gcl_class,
      go_ctrl_log TYPE REF TO gcl_class.

SELECTION-SCREEN : BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_bukrs  TYPE znfl_prj_t_001-bukrs DEFAULT '1000'.
  SELECT-OPTIONS: s_projek   FOR  znfl_prj_t_001-prj_kod,
                  s_muster   FOR  znfl_prj_t_001-musteri,
                  s_fmstri   FOR  znfl_prj_t_001-fat_musteri,
                  s_sproje   FOR  znfl_prj_t_001-pspnr,
                  s_pypoge   FOR  zfnl_aktvt_01-posid,
                  s_aktvte   FOR  zfnl_aktvt_01-tarih,
                  s_danis    FOR  zfnl_aktvt_01-danisman,
                  s_fatdan    FOR  zfnl_aktvt_01-fat_danisman.


  PARAMETERS: p_rad1 RADIOBUTTON GROUP gr1 USER-COMMAND test DEFAULT 'X',
              p_rad2 RADIOBUTTON GROUP gr1,
              p_rad3 RADIOBUTTON GROUP gr1,
              p_rad4 RADIOBUTTON GROUP gr1,
              p_rad5 RADIOBUTTON GROUP gr1.


SELECTION-SCREEN : END OF BLOCK bl1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sproje.
*  PERFORM pspnr_help CHANGING s_sproje.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pypoge.
*  PERFORM pypoge_help CHANGING s_pypoge.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_danis.
*  PERFORM pydanis_help CHANGING s_danis.
