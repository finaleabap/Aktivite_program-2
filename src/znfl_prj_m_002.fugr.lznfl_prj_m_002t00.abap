*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNFL_PRJ_M_002..................................*
DATA:  BEGIN OF STATUS_ZNFL_PRJ_M_002                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNFL_PRJ_M_002                .
CONTROLS: TCTRL_ZNFL_PRJ_M_002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZNFL_PRJ_M_002                .
TABLES: ZNFL_PRJ_M_002                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
