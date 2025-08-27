PROCESS BEFORE OUTPUT.
  MODULE status_0100.
  MODULE init_screen_0100.
  MODULE display_0100.
*
PROCESS AFTER INPUT.
  MODULE user_command_0100.

  FIELD gs_t_001-ay        MODULE change_donem ON CHAIN-REQUEST .
  FIELD gs_t_001-don_bas   MODULE change_ay ON CHAIN-REQUEST .
  FIELD gs_t_001-don_bit   MODULE change_ay ON CHAIN-REQUEST .
