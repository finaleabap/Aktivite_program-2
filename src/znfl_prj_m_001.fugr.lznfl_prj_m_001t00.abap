*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNFL_PRJ_M_001..................................*
DATA:  BEGIN OF STATUS_ZNFL_PRJ_M_001                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNFL_PRJ_M_001                .
CONTROLS: TCTRL_ZNFL_PRJ_M_001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZNFL_PRJ_M_001                .
TABLES: ZNFL_PRJ_M_001                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
