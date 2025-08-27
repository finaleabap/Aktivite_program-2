FUNCTION zsample_interface_00001650.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
*"  EXPORTING
*"     VALUE(E_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
*"----------------------------------------------------------------------

*-------------- Initialize Output by using the following line ----------
* E_POSTAB = I_POSTAB.

  DATA: ls_bseg TYPE bseg,
        ls_lfa1 TYPE lfa1.

  e_postab = i_postab.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_bseg
    FROM bseg
    WHERE bukrs = i_postab-bukrs
      AND belnr = i_postab-belnr
      AND gjahr = i_postab-gjahr
      AND buzei = i_postab-buzei.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_lfa1
    FROM lfa1
    WHERE lifnr = ls_bseg-lifnr.

    e_postab-zzvend_name = ls_lfa1-name1.

  endfunction.
