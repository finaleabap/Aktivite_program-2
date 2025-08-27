*&---------------------------------------------------------------------*
*& Report ZFNL_AKTIVITE_RAPOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFNL_AKTIVITE_RAPOR.

INCLUDE ZFNL_AKTIVITE_RAPOR_TOP.
INCLUDE ZFNL_AKTIVITE_RAPOR_CLS.
INCLUDE ZFNL_AKTIVITE_RAPOR_MDL.

INITIALIZATION.

 go_ctrl = gcl_class=>get_instance( ).
 go_ctrl->initialization( ).

START-OF-SELECTION .

 go_ctrl->run( ).
