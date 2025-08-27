*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZNFL_PRJ_M_002
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZNFL_PRJ_M_002     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
