PROCESS BEFORE OUTPUT.
  MODULE status_0101.
*
PROCESS AFTER INPUT.
  FIELD gs_main-musteri     MODULE module_museteri    ON CHAIN-REQUEST.
  FIELD gs_main-fat_musteri MODULE module_fat_musteri ON CHAIN-REQUEST.
  FIELD gs_main-prj_kod     MODULE module_prj_kod     ON CHAIN-REQUEST.
  "*  ) added by Ali Rıza Genç on 06.09.2023 14:00:10
  FIELD gs_main-posid       MODULE module_posid   ON CHAIN-REQUEST.
  "*  ) end of adding by  Ali Rıza Genç on 06.09.2023 14:00:10
  FIELD gs_main-post1       MODULE module_pyp_öğesi   ON CHAIN-REQUEST.


  MODULE user_command_0101.

*  FIELD gs_main-fatura_tip  MODULE module_fatura_tip  ON CHAIN-REQUEST.

PROCESS ON VALUE-REQUEST.

  FIELD gs_main-posid MODULE posid_help.
