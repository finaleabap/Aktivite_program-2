*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_RAPOR_MDL
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

  CASE sy-ucomm.
    WHEN go_ctrl->mc_back OR go_ctrl->mc_leave OR go_ctrl->mc_exit.
      LEAVE TO SCREEN 0.
    WHEN '&buton'.   "burada gui statusten koyduğumuz butonu aktifleştiririz.
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
