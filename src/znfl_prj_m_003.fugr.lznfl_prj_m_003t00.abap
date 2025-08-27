*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNFL_PRJ_M_003..................................*
DATA:  BEGIN OF STATUS_ZNFL_PRJ_M_003                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNFL_PRJ_M_003                .
CONTROLS: TCTRL_ZNFL_PRJ_M_003
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZNFL_PRJ_M_003                .
TABLES: ZNFL_PRJ_M_003                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
