*&---------------------------------------------------------------------*
*& Include          ZFNL_PROJE_YONETIMI_TOP
*&---------------------------------------------------------------------*

CLASS gcl_class DEFINITION DEFERRED.

DATA: go_ctrl     TYPE REF TO gcl_class,
      go_ctrl_log TYPE REF TO gcl_class.

DATA : gs_t_001 TYPE znfl_prj_s_001.
DATA : gv_sont  TYPE jahrper.
DATA : gv_maliyet TYPE znfl_prj_s_001-toplam_maliyet.
DATA : gv_efor    TYPE znfl_prj_s_001-top_efor.
DATA : gv_okcode TYPE char1.

CONTROLS proje_av TYPE TABSTRIP.


*SELECTION-SCREEN : BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
*
*SELECTION-SCREEN : END OF BLOCK bl1.
