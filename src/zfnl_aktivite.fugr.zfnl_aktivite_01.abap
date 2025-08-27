function zfnl_aktivite_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_STRUCTURE) TYPE  ZFNL_AKTVT_01 OPTIONAL
*"----------------------------------------------------------------------
  data: ls_log type zfnl_aktvt_01.
  data: lv_rtn type inri-returncode.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZAKTVT_NUM'
    importing
      number                  = is_structure-aktive_num
      returncode              = lv_rtn
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.

  move-corresponding is_structure to ls_log.
  modify zfnl_aktvt_01 from ls_log.
  commit work.

endfunction.
