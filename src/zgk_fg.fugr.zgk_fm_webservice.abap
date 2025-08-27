FUNCTION ZGK_FM_WEBSERVICE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IP_TITLE) TYPE  CHAR10 OPTIONAL
*"     VALUE(IP_FIRSTNAME) TYPE  CHAR30 OPTIONAL
*"     VALUE(IP_LASTNAME) TYPE  CHAR30 OPTIONAL
*"  EXPORTING
*"     VALUE(EP_NAME) TYPE  STRING
*"----------------------------------------------------------------------

  CONCATENATE ip_title ip_firstname ip_lastname INTO ep_name SEPARATED BY space.


ENDFUNCTION.
