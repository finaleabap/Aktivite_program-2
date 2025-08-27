*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_YONETIMI_FRM
*&---------------------------------------------------------------------*
FORM user_command USING p_ucom
                        p_selfiield TYPE slis_selfield.
  CASE p_ucom.
    WHEN '&IC1'.
      CLEAR gs_main.
      READ TABLE gt_akt INTO DATA(ls_akt) INDEX p_selfiield-tabindex.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_akt TO gs_main.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDFORM.
