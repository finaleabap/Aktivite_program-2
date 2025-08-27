*&---------------------------------------------------------------------*
*& Include          ZFNL_PROJE_YONETIMI_MDL
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*  DATA : lv_trh1 TYPE datum.
*  DATA : lv_trh2 TYPE datum.
*  DATA : months  TYPE  pea_scrmm.

  SET PF-STATUS 'PF_STATUS_0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN go_ctrl->mc_back OR go_ctrl->mc_leave OR go_ctrl->mc_exit.
      LEAVE TO SCREEN 0.
    WHEN go_ctrl->mc_prjg.
      go_ctrl->proje_getir( ).
    WHEN go_ctrl->mc_kaydet.
      go_ctrl->kaydet( ).
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_0100 OUTPUT.
  go_ctrl->display_alv( it_data = go_ctrl->mt_out ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_SCREEN_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_screen_0100 OUTPUT.

  IF gs_t_001-prj_kod IS INITIAL.
    gs_t_001-prj_kod = '$        1'.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'GS_T_001-FAT_GUN' .
      IF gs_t_001-ay_sonu = 'X'.
        screen-input = '0'.
        CLEAR gs_t_001-fat_gun.
      ELSE.
        screen-input = '1'.
      ENDIF.
*    ELSEIF  screen-name = 'GS_T_001-FAT_GUN'.
*      IF gs_t_001-ay_sonu = 'X'.
*        screen-active   = '0'.
*        screen-required = ' '.
*      ELSE.
*        screen-active   = '1'.
*        screen-required = 'X'.
**      ENDIF.
*      ENDIF.
    ENDIF.



    MODIFY SCREEN.
  ENDLOOP.

  IF gs_t_001-don_bas IS INITIAL.
    gs_t_001-don_bas = |{ sy-datum(04) }0{ sy-datum+4(02) }|.
  ENDIF.
  gs_t_001-don_bit = gv_sont.

  SELECT SINGLE mus_tanim FROM znfl_prj_m_001
                          INTO gs_t_001-mus_tanim
                         WHERE musteri = gs_t_001-musteri.

  SELECT SINGLE mus_tanim FROM znfl_prj_m_002
                          INTO gs_t_001-fat_mus_tanim
                         WHERE fat_musteri = gs_t_001-fat_musteri.


ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  DATA : lv_trh1 TYPE datum.
  DATA : lv_trh2 TYPE datum.
  DATA : months  TYPE  pea_scrmm.
* SET PF-STATUS 'xxxxxxxx'.
  SET PF-STATUS 'PF_STATUS_0100'."EXCLUDING 'SAVE'.



* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN go_ctrl->mc_back OR go_ctrl->mc_leave OR go_ctrl->mc_exit.
      LEAVE TO SCREEN 0.
    WHEN go_ctrl->mc_prjg.
      go_ctrl->proje_getir( ).
    WHEN go_ctrl->mc_kaydet.
      go_ctrl->kaydet( ).
  ENDCASE.

  CLEAR gv_okcode.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0201 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
*  SET PF-STATUS 'PF_STATUS_0100'.

*
*  IF gs_t_001-prj_kod IS INITIAL.
*    gs_t_001-prj_kod = '$        1'.
*  ENDIF.
*  LOOP AT SCREEN.
*    IF screen-name = 'GS_T_001-FAT_GUN' .
*      IF gs_t_001-ay_sonu = 'X'.
*        screen-input = '0'.
*        CLEAR gs_t_001-fat_gun.
*      ELSE.
*        screen-input = '1'.
*      ENDIF.
**    ELSEIF  screen-name = 'GS_T_001-FAT_GUN'.
**      IF gs_t_001-ay_sonu = 'X'.
**        screen-active   = '0'.
**        screen-required = ' '.
**      ELSE.
**        screen-active   = '1'.
**        screen-required = 'X'.
***      ENDIF.
**      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
*
*  IF gs_t_001-don_bas IS INITIAL.
*    gs_t_001-don_bas = |{ sy-datum(04) }0{ sy-datum+4(02) }|.
*  ENDIF.
*  gs_t_001-don_bit = gv_sont.
*
*  SELECT SINGLE mus_tanim FROM znfl_prj_m_001
*                          INTO gs_t_001-mus_tanim
*                         WHERE musteri = gs_t_001-musteri.
*
*  SELECT SINGLE mus_tanim FROM znfl_prj_m_002
*                          INTO gs_t_001-fat_mus_tanim
*                         WHERE fat_musteri = gs_t_001-fat_musteri.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.

  CASE sy-ucomm.
*    WHEN go_ctrl->mc_back OR go_ctrl->mc_leave OR go_ctrl->mc_exit.
*      LEAVE TO SCREEN 0.
*    WHEN go_ctrl->mc_prjg.
*      go_ctrl->proje_getir( ).
*    WHEN go_ctrl->mc_kaydet.
*      go_ctrl->kaydet( ).
    WHEN 'SSTAB1'.
      proje_av-activetab = sy-ucomm.
    WHEN 'SSTAB2'.
      proje_av-activetab = sy-ucomm.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0211 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0211 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

    LOOP AT SCREEN.

    IF screen-name = 'GS_T_001-FAT_DURUMU'.
      IF gs_t_001-tur = '2'.
        screen-input     = 0.
        screen-invisible = 1.
*        screen-required  = 0.
      ELSE.
        screen-input     = 1.
        screen-invisible = 0.
*        screen-required  = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0211  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0211 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0212 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0212 OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'GS_T_001-FAT_GUN' .
      IF gs_t_001-ay_sonu = 'X'.
        screen-input = '0'.
        CLEAR gs_t_001-fat_gun.
      ELSE.
        screen-input = '1'.
      ENDIF.
*    ELSEIF  screen-name = 'GS_T_001-FAT_GUN'.
*      IF gs_t_001-ay_sonu = 'X'.
*        screen-active   = '0'.
*        screen-required = ' '.
*      ELSE.
*        screen-active   = '1'.
*        screen-required = 'X'.
**      ENDIF.
*      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0212  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0212 INPUT.

  CASE sy-ucomm.
    WHEN '&EFOR'.
      go_ctrl->calculate_efor( ).
    WHEN '&MALIYET'.
      go_ctrl->calculate_maliyet( ).
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHANGE_AY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_ay INPUT.
*  DATA : lv_trh1 TYPE datum.
*  DATA : lv_trh2 TYPE datum.
*  DATA : months  TYPE  pea_scrmm.

  CHECK gs_t_001-don_bit IS NOT INITIAL.
  IF gs_t_001-don_bit < gs_t_001-don_bas.
    MESSAGE 'Sözleşme bitiş tarihi başlangıçtan küçük olamaz' TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  lv_trh1 = |{ gs_t_001-don_bas(04) }{ gs_t_001-don_bas+5(02) }01|.
  lv_trh2 = |{ gs_t_001-don_bit(04) }{ gs_t_001-don_bit+5(02) }01|.

  CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
    EXPORTING
      date1         = lv_trh2
      date2         = lv_trh1
      output_format = '04'
    IMPORTING
*     YEARS         =
      months        = months
*     DAYS          =
*   EXCEPTIONS
*     OVERFLOW_LONG_YEARS_BETWEEN       = 1
*     INVALID_DATES_SPECIFIED           = 2
*     OTHERS        = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  gs_t_001-ay = months.
  gv_sont = gs_t_001-don_bit.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_DONEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_donem INPUT.

  DATA : lv_tarih TYPE datum.

  lv_tarih = |{ gs_t_001-don_bas(04) }{ gs_t_001-don_bas+5(02) }01|.

  CALL FUNCTION 'HR_PSD_DATES_ADD_MONTHS'
    EXPORTING
      v_date   = lv_tarih
      v_months = CONV i( gs_t_001-ay )
    IMPORTING
      e_date   = lv_tarih.

  gs_t_001-don_bit = |{ lv_tarih(04) }0{ lv_tarih+4(02) }|.
  gv_sont = gs_t_001-don_bit.



ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0220 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0220 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.

ENDMODULE.
