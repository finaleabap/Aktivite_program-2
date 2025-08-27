*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_YONETIMI_TOP
*&---------------------------------------------------------------------*

CLASS gcl_class DEFINITION DEFERRED.
TABLES : zfnl_aktvt_01.

DATA: go_ctrl     TYPE REF TO gcl_class,
      go_ctrl_log TYPE REF TO gcl_class.

DATA : gs_main    TYPE zfnl_aktvt_s_01.
DATA : gs_is      TYPE zfnl_aktvt_s_is_01.
DATA : gv_okcode  TYPE syst_ucomm.
DATA : gv_index   TYPE index.
DATA : gv_kntrl   TYPE char1.
DATA gt_akt TYPE TABLE OF zfnl_aktvt_01.
*DATA : gs_modul   TYPE zfnl_danisman.

DATA: gv_begin_date TYPE sy-datum,
      gv_end_date   TYPE sy-datum.

CONTROLS table TYPE TABSTRIP.

*SELECTION-SCREEN : BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
*
*SELECTION-SCREEN : END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF SCREEN 120 AS WINDOW.

  SELECT-OPTIONS:s_tarih FOR zfnl_aktvt_01-tarih .

SELECTION-SCREEN END OF SCREEN 120.


SELECTION-SCREEN BEGIN OF SCREEN 130 AS WINDOW.

  SELECT-OPTIONS : s_aktg FOR zfnl_aktvt_01-srch_getir.
  SELECT-OPTIONS : s_aktv FOR zfnl_aktvt_01-aktive_num.

SELECTION-SCREEN END OF SCREEN 130.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_aktg-low.
  PERFORM pspnr_help CHANGING s_aktg-low.
*INITIALIZATION.
