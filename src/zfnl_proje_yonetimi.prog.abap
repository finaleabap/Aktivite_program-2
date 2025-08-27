*&---------------------------------------------------------------------*
*& Report ZFNL_PROJE_YONETIMI
*------------------------------------------------------------------------*
* Title     : SAP Proje Yönetimi Uygulaması
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
*  13.07.2023        OFU                 Hasan USLU       Proje Yönetimi
*
*------------------------------------------------------------------------*
REPORT zfnl_proje_yonetimi.

INCLUDE zfnl_proje_yonetimi_top.
INCLUDE zfnl_proje_yonetimi_cls.
INCLUDE zfnl_proje_yonetimi_mdl.


INITIALIZATION.
  go_ctrl = gcl_class=>get_instance( ).
  go_ctrl->initialization( ).

START-OF-SELECTION .
  go_ctrl->run( ).
