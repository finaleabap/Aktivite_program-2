*------------------------------------------------------------------------*
*& Report   : ZFNL_AKTIVITE_YONETIMI
*------------------------------------------------------------------------*
* Title     : Finale Aktivite Takip Programı
* Object ID :
*------------------------------------------------------------------------*
* Programmer: Ömer Faruk Uray #OFU#
*------------------------------------------------------------------------*
* Description:
*------------------------------------------------------------------------*
**************************************************************************
*             H I S T O R Y   O F   R E V I S I O N S
**************************************************************************
*       Date          Programmer         Consultant         Description
*  ---------------  -------------------  -------------    ---------------*
*  30.03.2023        OFU                                  New Development
*
*------------------------------------------------------------------------*
REPORT zfnl_aktivite_yonetimi.

INCLUDE zfnl_aktivite_yonetimi_top.
INCLUDE zfnl_aktivite_yonetimi_cls.
INCLUDE zfnl_aktivite_yonetimi_mdl.
INCLUDE zfnl_aktivite_yonetimi_FRM.


INITIALIZATION.
  go_ctrl = gcl_class=>get_instance( ).
  go_ctrl->initialization( ).

START-OF-SELECTION .
  go_ctrl->run( ).
