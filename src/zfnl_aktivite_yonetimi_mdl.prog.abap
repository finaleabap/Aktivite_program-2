*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_YONETIMI_MDL
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE gv_okcode.
    WHEN go_ctrl->mc_back OR go_ctrl->mc_leave OR go_ctrl->mc_exit.
      LEAVE TO SCREEN 0.
*    WHEN go_ctrl->mc_kaydet.
*      DATA(lv_err) = go_ctrl->check_data( ).
*      CHECK lv_err IS INITIAL.
*      go_ctrl->save_data( ).
*
  ENDCASE.

*  CLEAR gv_okcode.

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
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
*MODULE module_fatura_tip.

  go_ctrl->screen_parameters( ).

  LOOP AT SCREEN.
    IF screen-name = 'GS_MAIN-MUS_DANIS'.
      screen-required  = 1.
    ENDIF.

    IF screen-name = 'GS_MAIN-ONY_DANISMAN'.
      IF gs_main-fatura_tip = '5'.
        screen-input     = 1.
        screen-invisible = 0.
        screen-required  = 1.
      ELSE.
        screen-input     = 0.
        screen-invisible = 1.
        screen-required  = 0.
        CLEAR gs_main-ony_danisman.
      ENDIF.
    ENDIF.

    IF screen-name = 'GS_MAIN-FAT_DANISMAN'.
      IF gs_main-fatura_tip = '1' OR gs_main-fatura_tip = '2' OR gs_main-fatura_tip = '3' OR gs_main-fatura_tip = '6'.
        screen-input     = 1.
        screen-invisible = 0.
        screen-required  = 1.

      ELSE.
        screen-input     = 0.
        screen-invisible = 1.
        screen-required  = 0.
*        CLEAR GS_MAIN-FAT_DANISMAN.
      ENDIF.
    ENDIF.

    IF screen-name = 'GS_MAIN-FAT_DURUMU'.
      IF gs_main-fatura_tip = '1' OR gs_main-fatura_tip = '2' OR gs_main-fatura_tip = '6'.
        screen-input     = 1.
        screen-invisible = 0.
*        screen-required  = 1.

      ELSE.
        screen-input     = 0.
        screen-invisible = 1.
*        screen-required  = 0.
*        CLEAR GS_MAIN-FAT_DANISMAN.
      ENDIF.
    ENDIF.


    IF screen-name = 'GS_MAIN-FATURALANMAMA_NEDENI'.
      IF gs_main-fat_durumu = '2' or gs_main-fatura_tip = '7'.
        screen-input     = 1.
        screen-invisible = 0.
*        screen-required  = 1.

      ELSE.
        screen-input     = 0.
        screen-invisible = 1.
*        screen-required  = 0.
*        CLEAR GS_MAIN-FAT_DANISMAN.
      ENDIF.
    ENDIF.


    IF screen-name = 'GS_MAIN-ONAY_AD_SOYAD'.
      IF gs_main-fatura_tip = '5'.
        screen-input     = 1.
        screen-invisible = 0.
        screen-required  = 1.
      ELSE.
        screen-input     = 0.
        screen-invisible = 1.
        screen-required  = 0.
        CLEAR gs_main-onay_ad_soyad.
      ENDIF.
    ENDIF.

    IF screen-name = 'GS_MAIN-IPT_NEDEN'.
      IF gs_main-durum = '3'.
        screen-input     = 1.
        screen-invisible = 0.
        screen-required  = 1.
      ELSE.
        screen-input     = 0.
        screen-invisible = 1.
        screen-required  = 0.
        CLEAR gs_main-ipt_neden.
      ENDIF.
    ENDIF.

    "*  ) added by Ali Rıza Genç on 30.08.2023 16:20:44
    IF ( screen-name = 'GS_MAIN-FATURA_EFOR'
      OR screen-name = 'GS_MAIN-FATURA_EFOR_GUN'
      OR screen-name = 'GS_MAIN-FATURA_EFOR_TOP'
      OR screen-name = 'GS_MAIN-FAT_DANISMAN'
      OR screen-name = 'GS_MAIN-SAAT'
      OR screen-name = 'GS_MAIN-GUN'  )
      AND gs_main-fatura_tip = '07'.
      screen-input     = 0.
      screen-invisible = 1.
      screen-required  = 0.
      CLEAR: gs_main-fatura_efor, gs_main-fatura_efor_gun,  gs_main-fatura_efor_top, gs_main-gun.
    ENDIF.
    .
    IF ( screen-name = 'GS_MAIN-FATURA_EFOR'
         OR screen-name = 'GS_MAIN-FATURA_EFOR_GUN'
         OR screen-name = 'GS_MAIN-FATURA_EFOR_TOP'
         OR screen-name = 'GS_MAIN-FAT_DANISMAN'
         OR screen-name = 'GS_MAIN-SAAT'
         OR screen-name = 'GS_MAIN-GUN'  )
         AND gs_main-fat_durumu = '02'.
      screen-input     = 0.
      screen-invisible = 1.
      screen-required  = 0.
      CLEAR: gs_main-fatura_efor, gs_main-fatura_efor_gun,  gs_main-fatura_efor_top, gs_main-gun.
    ENDIF.

    .
    MODIFY SCREEN .
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE gv_okcode..
    WHEN go_ctrl->mc_back OR go_ctrl->mc_leave OR go_ctrl->mc_exit.
      LEAVE TO SCREEN 0.
    WHEN go_ctrl->mc_temizl.
      CLEAR gs_main.
      gs_main-bukrs        = '1000'.
      gs_main-mus_danis    = 'FINALE'.

    WHEN '&BUT'.

      DATA : ch_text TYPE catsxt_longtext_itab.
      DATA : cs_text TYPE txline.
      DATA : lv_int TYPE p DECIMALS 3.
      DATA : lv_int1 TYPE i.

      SELECT SINGLE ek_alan FROM zfnl_aktvt_01
        INTO @DATA(lv_ek_alan)
        WHERE zfnl_aktvt_01~bukrs       EQ @gs_main-bukrs
          AND zfnl_aktvt_01~aktive_num  EQ @gs_main-aktive_num
          AND zfnl_aktvt_01~kayit_tarih EQ @gs_main-kayit_tarih.

      CLEAR: ch_text , cs_text.

      lv_int = strlen( lv_ek_alan ).
      lv_int1 = 0.
      WHILE lv_int / 72 > 0.
        cs_text = lv_ek_alan+lv_int1(72).
        APPEND cs_text TO ch_text.
        lv_int = lv_int - 72.
        lv_int1 = lv_int1 + 72.
      ENDWHILE.


      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Açıklama girinizi(Maksimum 1000 karakter)'
*         IM_DISPLAY_MODE = space
          im_start_column = 10
          im_start_row    = 10
        CHANGING
          ch_text         = ch_text.

      CLEAR: gs_main-ek_alan.
      LOOP AT ch_text INTO DATA(ls_text).
        gs_main-ek_alan = |{ gs_main-ek_alan } { ls_text }|.
      ENDLOOP.

    WHEN go_ctrl->mc_kaydet.
      DATA(lv_err) = go_ctrl->check_data( ).
      CHECK lv_err IS INITIAL.
      go_ctrl->save_data( iv_new = abap_false ).
    WHEN go_ctrl->mc_yeni_kyt.
      lv_err = go_ctrl->check_data( ).
      CHECK lv_err IS INITIAL.
      go_ctrl->save_data( iv_new = abap_true ).
    WHEN go_ctrl->mc_sil.
      go_ctrl->delete_data( ).
      CLEAR gs_main.
      gs_main-bukrs        = '1000'.
      gs_main-mus_danis    = 'FINALE'.
    WHEN go_ctrl->mc_akt_getir.
      CALL SELECTION-SCREEN 130 STARTING AT 10 5 ENDING AT 90 10.
      go_ctrl->akt_getir( ).
    WHEN 'REFR'.



      READ TABLE go_ctrl->mt_aktivite INTO DATA(ls_aktivite) INDEX gv_index.
      SELECT SINGLE * FROM zfnl_aktvt_01
                      INTO gs_main
                     WHERE aktive_num = ls_aktivite-aktive_num.

    WHEN go_ctrl->mc_is_bul.
      CALL SCREEN 0200 STARTING AT 10 10 ENDING AT 130 30.

  ENDCASE.
  CLEAR gv_okcode.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODULE_MUSETERI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE module_museteri INPUT.

  SELECT SINGLE mus_tanim FROM znfl_prj_m_001
                          INTO gs_main-mus_tanim
                         WHERE musteri = gs_main-musteri.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODULE_FAT_MUSTERI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE module_fat_musteri INPUT.

  SELECT SINGLE mus_tanim FROM znfl_prj_m_002
                          INTO gs_main-fat_mus_tanim
                         WHERE fat_musteri = gs_main-fat_musteri.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODULE_PRJ_KOD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE module_prj_kod INPUT.

  SELECT SINGLE prj_ack FROM znfl_prj_t_001
                        INTO gs_main-prj_ack
                       WHERE prj_kod = gs_main-prj_kod.

  CLEAR : gs_main-posid.

ENDMODULE.
"-------------------> ebilgin 31.08.2023 15:13:21
MODULE module_pyp_ogesi INPUT.

  SELECT SINGLE post1 FROM prps
                      INTO gs_main-post1
                      WHERE posid EQ gs_main-posid.

ENDMODULE.
"<------------------- ebilgin 31.08.2023 15:13:21
*&---------------------------------------------------------------------*
*&      Module  MODULE_FATURA_TIP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE module_fatura_tip INPUT.

  IF gs_main-fatura_tip = 01.
    gs_main-efor_statu = 'P'.
  ENDIF.
  IF gs_main-fatura_tip = 02.
    gs_main-efor_statu = 'O'.
  ENDIF.
  IF gs_main-fatura_tip = 03.
    gs_main-efor_statu = 'O'.
  ENDIF.
  IF gs_main-fatura_tip = 04.
    gs_main-efor_statu = 'O'.
  ENDIF.
  IF gs_main-fatura_tip = 05.
    gs_main-efor_statu = 'B'.
  ENDIF.
  IF gs_main-fatura_tip = 06.
    gs_main-efor_statu = 'O'.
  ENDIF.
  IF gs_main-fatura_tip = 06.
    gs_main-efor_statu = 'R'.
  ENDIF.


ENDMODULE.
MODULE module_posid INPUT.

  DATA: ls_posid TYPE char30.
  ls_posid = gs_main-posid.

  SELECT SINGLE posid, post1 FROM prps
    INTO @DATA(ls_prps)
    WHERE posid = @ls_posid.

  IF ls_prps IS INITIAL.

    MESSAGE 'Hatalı Giriş Yaptınız. Lütfen Search Help Üzerinden Seçim Yapınız.' TYPE 'I' DISPLAY LIKE 'E'.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0111 OUTPUT.
  go_ctrl->display_alv( it_data = go_ctrl->mt_aktivite ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0112 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0112 OUTPUT.
  go_ctrl->display_alv_onay( it_data = go_ctrl->mt_onay ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CASE sy-ucomm.
    WHEN 'AKTIVITE'.
      table-activetab = sy-ucomm.
    WHEN 'ONAY'.
      table-activetab = sy-ucomm.
  ENDCASE.

*  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0112 INPUT.
  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
* SET PF-STATUS 'PF_STATUS_0110'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF_STATUS_0200'.

  go_ctrl->display_alv_is( it_data = go_ctrl->mt_islist ).

  SELECT SINGLE mus_tanim FROM znfl_prj_m_001
                           INTO gs_is-mus_tanim
                          WHERE musteri = gs_is-musteri.


  SELECT SINGLE mus_tanim FROM znfl_prj_m_002
                           INTO gs_is-fat_mus_tanim
                          WHERE fat_musteri = gs_is-fat_musteri.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'GETIR'.
      go_ctrl->is_belgesi_getir( ).
    WHEN '&EKLE'.
      go_ctrl->danisman_ekle( ).
    WHEN 'TAMAM'.
      go_ctrl->is_getir( ).
    WHEN 'IPTAL'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  POSID_HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE posid_help INPUT.

  DATA : returncode.
  DATA : return_tab LIKE TABLE OF ddshretval.
  DATA : s_return_tab LIKE ddshretval.
  SELECT SINGLE * FROM znfl_prj_t_001
                  INTO @DATA(ls_prj)
                 WHERE prj_kod = @gs_main-prj_kod.

  SELECT psphi,posid,post1 FROM prps
                     INTO TABLE @DATA(lt_prps)
                          WHERE psphi = @ls_prj-pspnr.



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'POSID' "=> alan
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_MAIN-POSID' "=> alan adi
      value_org   = 'S'
    TABLES
      value_tab   = lt_prps " => alan tablosu(kendimiz oluşturabiliriz)
      return_tab  = return_tab.


  CHECK return_tab[] IS NOT INITIAL.
  READ TABLE return_tab INTO s_return_tab INDEX 1.
  gs_main-posid = s_return_tab-fieldval.

ENDMODULE.
