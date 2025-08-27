*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_YONETIMI_CLS
*&---------------------------------------------------------------------*

class gcl_class definition.

  public section.

    class-data : mo_instance type ref to gcl_class.
    class-data : mt_out      type standard table of itab.
    class-data : mo_grid1  type ref to cl_gui_alv_grid,
                 mo_grid2  type ref to cl_gui_alv_grid,
                 mo_grid3  type ref to cl_gui_alv_grid,
                 mo_cutom1 type ref to cl_gui_custom_container,
                 mo_cutom2 type ref to cl_gui_custom_container,
                 mo_cutom3 type ref to cl_gui_custom_container.
    class-data : mt_aktivite type table of zfnl_aktvt_s_01.
    class-data : mt_onay     type table of zfnl_aktvt_s_01.
    class-data : mt_islist   type table of zfnl_aktvt_is_02.
*    CLASS-DATA : gr_date     TYPE RANGE OF datum.


    constants : mc_container1 type char20        value 'CC1',
                mc_container2 type char20        value 'CC2',
                mc_container3 type char20        value 'CC3',
                mc_structure  type dd02l-tabname value 'ZFNL_AKTVT_S_01',
                mc_structure2 type dd02l-tabname value 'ZFNL_AKTVT_IS_02',
                mc_selmod     type char1         value 'A',
                mc_back       type char10        value 'BACK',
                mc_leave      type char10        value 'LEAVE',
                mc_exit       type char10        value 'EXIT',
                mc_kaydet     type char10        value 'KAYDET',
                mc_temizl     type char10        value 'TEMIZLE',
                mc_sil        type char10        value 'SIL',
                mc_is_bul     type char10        value '&IS_BUL',
                mc_yeni_kyt   type char10        value 'YENI_KYT',
                mc_akt_getir  type char10        value 'AKT_GETIR'

                .


    data : ms_layout  type lvc_s_layo,
           ms_variant type disvariant.

    data : mt_fieldcat type lvc_t_fcat.



    class-methods:
      class_constructor,
      get_instance returning value(ro_instance) type ref to gcl_class.

    methods:
      get_data
        returning value(r_error) type char1,
      display_alv
        importing
          it_data type standard table,
      display_alv_onay
        importing
          it_data type standard table,
      display_alv_is
        importing
          it_data type standard table,
      run,

      handle_hotspot_click_aktivite
        for event hotspot_click of cl_gui_alv_grid
        importing
          e_row_id
          e_column_id
          es_row_no
          sender,

      handle_hotspot_click_onay
        for event hotspot_click of cl_gui_alv_grid
        importing
          e_row_id
          e_column_id
          es_row_no
          sender,


      double_click
        for event double_click of cl_gui_alv_grid
        importing e_row
                  e_column ,

      handle_toolbar_set
        for event toolbar of cl_gui_alv_grid
        importing
          e_object
          e_interactive,

      handle_user_command
        for event user_command of cl_gui_alv_grid
        importing
          e_ucomm,
      handle_data_changed
        for event data_changed of cl_gui_alv_grid
        importing er_data_changed,

      initialization,

      refresh_alv,

      refresh_alv_is,

      check_data
        returning value(rv_err) type char1,

      save_data
        importing
          iv_new type char1,

      delete_data
        for event hotspot_click of cl_gui_alv_grid
        importing
          e_row_id
          e_column_id
          es_row_no,

      screen_parameters,

      is_belgesi_getir,

      danisman_ekle,

      is_getir,

      akt_getir
      .

  private section.

    methods:
      fill_data,
      create_fieldcat
        importing
          iv_structure_name type dd02l-tabname
          it_data           type standard table optional
        exporting
          et_fieldcat       type lvc_t_fcat,

      create_object
        importing
          iv_container     type c
        exporting
          er_grid          type ref to cl_gui_alv_grid
          value(ev_custom) type ref to cl_gui_custom_container,

      set_layout
        importing
          iv_zebra  type char1 optional
          iv_edit   type char1 optional
          iv_sel    type char1 optional
          iv_toll_b type char1 optional
          iv_cwidth type char1 optional
        changing
          cs_layout type lvc_s_layo,

      set_handler
        importing
          ir_grid type ref to cl_gui_alv_grid ,

      get_aktivite_num
        returning value(rv_aknum) type char10 ,

      get_date    ,

      get_month_range,

      get_domain_value.




endclass.

class gcl_class implementation.
  method class_constructor.

    if mo_instance is not bound.
      mo_instance = new #( ).
    endif.

  endmethod.


  method run.

    me->get_data( ).
    get_domain_value( ).
*    CHECK lv_err IS INITIAL.
    me->fill_data( ).

    call screen 0100.

  endmethod.

  method get_instance.

    ro_instance = mo_instance.

  endmethod.

  method get_data.

    select 01~*,'@5H@' as icon from zfnl_aktvt_01 as 01
       into corresponding fields of table @mt_aktivite
            where danisman = @sy-uname
*      OR fat_danisman = @sy-uname
              and tarih        in @s_tarih.

    select 01~*,'@5Y@' as icon from zfnl_aktvt_01 as 01
       into corresponding fields of table @mt_onay
            where ony_danisman = @sy-uname
              and efor_statu   = 'B'.

    sort mt_aktivite descending by aktive_num.
    sort mt_onay     descending by aktive_num.

*    IF mt_out IS INITIAL.
*      r_error = abap_true.
*    ENDIF.

  endmethod.

  method fill_data.

    loop at mt_aktivite into data(ls_aktivite).
      ls_aktivite-aktivite_gun = ls_aktivite-aktivite_sure * '0.125'.
      modify mt_aktivite from ls_aktivite.
    endloop.


  endmethod.

  method create_fieldcat.

    call function 'LVC_FIELDCATALOG_MERGE'
      exporting
*       I_BUFFER_ACTIVE        =
        i_structure_name       = iv_structure_name
*       I_CLIENT_NEVER_DISPLAY = 'X'
*       I_BYPASSING_BUFFER     =
      changing
        ct_fieldcat            = et_fieldcat[]
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.

    read table et_fieldcat assigning field-symbol(<fs_fcat>) with key fieldname = 'ICON'.
    if sy-subrc = 0.
      <fs_fcat>-hotspot    = abap_true.
      <fs_fcat>-fix_column = abap_true.
      <fs_fcat>-key        = abap_true.
    endif.

  endmethod.

  method display_alv.
    data : ls_stable type lvc_s_stbl .

    if mo_cutom1 is initial.

      me->create_fieldcat(
        exporting
          iv_structure_name = mc_structure
          it_data           = mt_aktivite
        importing
          et_fieldcat       = mt_fieldcat
      ).

      me->create_object(
        exporting
          iv_container = mc_container1
        importing
          er_grid      = mo_grid1
          ev_custom    = mo_cutom1
      ).

      me->set_layout(
*        EXPORTING
*          iv_zebra  = abap_true
*          iv_edit   = abap_true
*          iv_sel    = mc_selmod
*          iv_toll_b = abap_true
        changing
          cs_layout = ms_layout
      ).

      ms_variant-report   = sy-repid.
      ms_variant-username = sy-uname.
      ms_variant-variant  = '/AKTV'.

*      me->set_handler( ir_grid = mo_grid1 ).

*      SET HANDLER go_ctrl->handle_toolbar_set FOR mo_grid1.
      set handler go_ctrl->handle_hotspot_click_aktivite for mo_grid1.
      set handler go_ctrl->handle_toolbar_set for mo_grid1.
      set handler go_ctrl->handle_user_command for mo_grid1.

      call method mo_grid1->set_table_for_first_display
        exporting
          i_save                        = 'A'
          i_default                     = 'X'
          is_layout                     = ms_layout
          is_variant                    = ms_variant
*         it_toolbar_excluding          = lt_exclude
        changing
          it_outtab                     = mt_aktivite[]
          it_fieldcatalog               = mt_fieldcat[]
*         it_sort                       = t_sort
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4.

    else.


      me->refresh_alv( ).

    endif.


  endmethod.

  method display_alv_onay.
    data : ls_stable type lvc_s_stbl .

    if mo_cutom2 is initial.

      me->create_fieldcat(
        exporting
          iv_structure_name = mc_structure
          it_data           = mt_onay
        importing
          et_fieldcat       = mt_fieldcat
      ).

      me->create_object(
        exporting
          iv_container = mc_container2
        importing
          er_grid      = mo_grid2
          ev_custom    = mo_cutom2
      ).

      me->set_layout(
*        EXPORTING
*          iv_zebra  = abap_true
*          iv_edit   = abap_true
*          iv_sel    = mc_selmod
*          iv_toll_b = abap_true
        changing
          cs_layout = ms_layout
      ).

      ms_variant-report   = sy-repid.
      ms_variant-username = sy-uname.
      ms_variant-variant  = '/ONAY'.

*      me->set_handler( ir_grid = mo_grid2 ).

      set handler go_ctrl->handle_hotspot_click_onay for mo_grid2.
      set handler go_ctrl->double_click for mo_grid2.

      call method mo_grid2->set_table_for_first_display
        exporting
          i_save                        = 'A'
          i_default                     = 'X'
          is_layout                     = ms_layout
          is_variant                    = ms_variant
*         it_toolbar_excluding          = lt_exclude
        changing
          it_outtab                     = mt_onay[]
          it_fieldcatalog               = mt_fieldcat[]
*         it_sort                       = t_sort
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4.

    else.


      me->refresh_alv( ).

    endif.


  endmethod.

  method display_alv_is.
    data : ls_stable type lvc_s_stbl .
    data : lt_fieldcat type lvc_t_fcat.


    if mo_cutom3 is initial.

      me->create_fieldcat(
        exporting
          iv_structure_name = mc_structure2
          it_data           = mt_islist
        importing
          et_fieldcat       = lt_fieldcat
      ).

      me->create_object(
        exporting
          iv_container = mc_container3
        importing
          er_grid      = mo_grid3
          ev_custom    = mo_cutom3
      ).

      me->set_layout(
*        EXPORTING
*          iv_zebra  = abap_true
*          iv_edit   = abap_true
*          iv_sel    = mc_selmod
*          iv_toll_b = abap_true
        changing
          cs_layout = ms_layout
      ).

      ms_variant-report   = sy-repid.
      ms_variant-username = sy-uname.
      ms_variant-variant  = '/ONAY'.

*      me->set_handler( ir_grid = mo_grid2 ).

*      SET HANDLER go_ctrl->handle_hotspot_click_onay FOR mo_grid3.
*      SET HANDLER go_ctrl->double_click FOR mo_grid3.

      call method mo_grid3->set_table_for_first_display
        exporting
          i_save                        = 'A'
          i_default                     = 'X'
          is_layout                     = ms_layout
          is_variant                    = ms_variant
*         it_toolbar_excluding          = lt_exclude
        changing
          it_outtab                     = mt_islist[]
          it_fieldcatalog               = lt_fieldcat[]
*         it_sort                       = t_sort
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4.

    else.


      me->refresh_alv( ).

    endif.


  endmethod.

  method create_object.

    free ev_custom.

    create object ev_custom
      exporting
        container_name = iv_container.

    create object er_grid
      exporting
        i_parent = ev_custom.

  endmethod.

  method set_layout.

    cs_layout-zebra      = iv_zebra.
    cs_layout-edit       = iv_edit.
    cs_layout-sel_mode   = iv_sel.
    cs_layout-no_toolbar = iv_toll_b.
    cs_layout-cwidth_opt = iv_cwidth.


  endmethod.


  method handle_hotspot_click_aktivite.
    data: ls_col_id   type lvc_s_col.

    read table mt_aktivite into data(ls_aktivite) index e_row_id-index.
    gv_index = e_row_id-index.
*
*    SELECT SINGLE * FROM zfnl_aktvt_01
*                    INTO gs_main
*                   WHERE aktive_num = ls_aktivite-aktive_num.

    cl_gui_cfw=>set_new_ok_code( new_code = 'REFR' ) .

*    BREAK-POINT.
  endmethod.                    "handle_hotspot_click

  method handle_hotspot_click_onay.
    data: ls_col_id   type lvc_s_col.

    read table mt_onay into data(ls_onay) index e_row_id-index.
*    gv_index = e_row_id-index.

    select single * from zfnl_aktvt_01
                    into @data(ls_aktvt)
                   where aktive_num = @ls_onay-aktive_num.

    data(lv_qust) = |Üzerinizdeki eforu onaylamak ister misiniz?|.
    data : ans(1).

    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = 'Efor onaylama'
        text_question         = lv_qust
        text_button_1         = 'Evet'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Hayır'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
        popup_type            = 'ICON_RELEASE'
      importing
        answer                = ans.
    if ans = 2.
    elseif ans = 1.
      ls_aktvt-efor_statu = 'O'.
    endif.

    modify zfnl_aktvt_01 from ls_aktvt.

    me->get_data( ).
    me->refresh_alv( ).

*    BREAK-POINT.
  endmethod.                    "handle_hotspot_click

  method double_click.

*    READ TABLE mt_out INTO DATA(ls_out) INDEX e_row-index.
*    CASE e_column.
*      WHEN 'BELNR'.
*        SET PARAMETER ID 'BEL' FIELD ls_out-belnr.
*        SET PARAMETER ID 'BUK' FIELD ls_out-bukrs.
*        SET PARAMETER ID 'GJR' FIELD ls_out-gjahr.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*    ENDCASE.
    case e_column.
      when 'AKTIVE_NUM'.
        gv_index = e_row-index.
        cl_gui_cfw=>set_new_ok_code( new_code = 'REFR' ) .
    endcase.
    me->refresh_alv( ).


  endmethod.

  method handle_toolbar_set.
    data:  ty_toolbar      type stb_button.

    clear ty_toolbar.
    clear ty_toolbar.
    "  · ekrana buton ekleme

    clear ty_toolbar.
    move 3 to ty_toolbar-butn_type. "separator
    append ty_toolbar to e_object->mt_toolbar.


    ty_toolbar-function = 'FILTRE'. "name of btn to  catch click
    ty_toolbar-butn_type = 0.
    move icon_date  to ty_toolbar-icon.
    ty_toolbar-text = 'Tarih Filtresi'.
    append ty_toolbar  to e_object->mt_toolbar.
*
*    ty_toolbar-function = 'SIL'. "name of btn to  catch click
*    ty_toolbar-butn_type = 0.
*    MOVE icon_delete_row  TO ty_toolbar-icon.
*    ty_toolbar-text = 'Sil'.
*    APPEND ty_toolbar  TO e_object->mt_toolbar.

    delete e_object->mt_toolbar
       where function eq '&DETAIL'
          or function eq '&&SEP00'
          or function eq '&CHECK'
          or function eq '&REFRESH'
          or function eq '&&SEP01'
          or function eq '&LOCAL&CUT'
          or function eq '&LOCAL&COPY'
          or function eq '&LOCAL&PASTE'
          or function eq '&LOCAL&UNDO'
          or function eq '&&SEP02'
          or function eq '&LOCAL&APPEND'
          or function eq '&LOCAL&INSERT_ROW'
          or function eq '&LOCAL&DELETE_ROW'
          or function eq '&LOCAL&COPY_ROW'
          or function eq '&&SEP03'
          or function eq '&&SEP06'
          or function eq '&GRAPH'
          or function eq '&&SEP07'
          or function eq '&INFO'.

  endmethod.                    "handle_toolbar_set


  method handle_user_command.
    data: index_rows type lvc_t_row,
          index      like line of index_rows.

    clear index_rows. refresh index_rows.

    call method mo_grid1->get_selected_rows
      importing
        et_index_rows = index_rows.

    data: wr_data_changed type ref to cl_alv_changed_data_protocol.
    data: lt_rows  type lvc_t_row,
          lt_index type  lvc_s_row-index.

    data : gi_index_rows type lvc_t_row .
    data : wa_index_rows type line of lvc_t_row .

    case e_ucomm.
      when 'FILTRE'.
        me->get_date( )..
    endcase.

    me->refresh_alv( ).

  endmethod.                    "handle_user_command



  method handle_data_changed.
    data: ls_good      type lvc_s_modi,
          ls_sflight   type sflight,
          ls_good_cost type lvc_s_modi.

    data: ls_modi type lvc_s_modi.
    data: ls_stbl type lvc_s_stbl.

    field-symbols : <fs_table> type any table.


    assign er_data_changed->mp_mod_rows->* to field-symbol(<fs_mod_rows>).
    assign er_data_changed->mt_mod_cells to field-symbol(<fs_cells>).

    loop at er_data_changed->mt_good_cells into ls_good.

      data(lv_name) = '<FS_MOD_ROWS>'.
      assign (lv_name) to <fs_table>.
      loop at <fs_table> assigning field-symbol(<fs_tab>).
*        ls_table = <fs_tab>.
*        READ TABLE gt_table ASSIGNING FIELD-SYMBOL(<fs_line_table>) index .
        case ls_good-fieldname.
          when 'ALIS_TUTAR' or 'MARK_UP'.

          when 'TICKET'.

          when 'VADE_AYI'.

        endcase.

      endloop.
    endloop.

    me->refresh_alv( ).


  endmethod. "handle_data_changed

  method set_handler.

    create object go_ctrl.

    set handler go_ctrl->double_click         for ir_grid.
    set handler go_ctrl->handle_data_changed  for ir_grid.
*    SET HANDLER go_ctrl->handle_hotspot_click FOR ir_grid.
    set handler go_ctrl->handle_toolbar_set   for ir_grid.
    set handler go_ctrl->handle_user_command  for ir_grid.


  endmethod.

  method initialization.
    gs_main-bukrs        = '1000'.
    gs_main-kayit_tarih  = sy-datum.
    gs_main-tarih        = sy-datum.
    gs_main-danisman     = sy-uname.
    gs_main-fat_danisman = sy-uname.
    gs_main-mus_danis    = 'FINALE'.

    me->get_month_range( ).




  endmethod.

  method refresh_alv.
    data : ls_stable type lvc_s_stbl .
    ls_stable-col = abap_true.
    ls_stable-row = abap_true.

    call method mo_grid1->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

    call method mo_grid2->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

  endmethod.

  method refresh_alv_is.
    data : ls_stable type lvc_s_stbl .
    ls_stable-col = abap_true.
    ls_stable-row = abap_true.

    call method mo_grid3->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

  endmethod.

  method get_aktivite_num.

    data: lv_rtn type inri-returncode.

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = '01'
        object                  = 'ZAKTVT_NUM'
      importing
        number                  = rv_aknum
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


  endmethod.

  method check_data.

*    IF gs_main-aktivite_sure IS INITIAL.
*      MESSAGE 'Lütfen aktivite süresi giriniz' TYPE 'I' DISPLAY LIKE 'E'.
*      rv_err = abap_true.
*      RETURN.
*    ENDIF.

    if gs_main-danisman is initial.
      message 'Lütfen danışman seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-efor_statu is initial.
      message 'Lütfen efor statü seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-fat_durumu eq '2'.
      if gs_main-faturalanmama_nedeni is initial.
        message 'Lütfen faturalanmama nedenini giriniz' type 'I' display like 'E'.
        rv_err = abap_true.
        return.
      endif.
    endif.


    if gs_main-durum is initial.
      message 'Lütfen iş durum seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-fat_musteri is initial.
      message 'Lütfen fatura müşterisi seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-fatura_tip is initial.
      message 'Lütfen faturalama tipi seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

*    IF gs_main-is_num IS INITIAL.
*      MESSAGE 'Lütfen iş seçimi yapınız' TYPE 'I' DISPLAY LIKE 'E'.
*      rv_err = abap_true.
*      RETURN.
*    ENDIF.

    if gs_main-musteri is initial.
      message 'Lütfen müşteri seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-prj_kod is initial.
      message 'Lütfen proje kodu seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-posid is initial.
      message 'Lütfen pyp öğesi seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.

    if gs_main-statu is initial.
      message 'Lütfen statü seçimi yapınız' type 'I' display like 'E'.
      rv_err = abap_true.
      return.
    endif.


  endmethod.

  method save_data.
    data : ls_aktivite type zfnl_aktvt_01.
    data: lv_msg      type string.
    data: lv_yk_msg      type string.
    data(lv_f_efor) = gs_main-fatura_efor.
    data(lv_efor_gun) = gs_main-fatura_efor_gun.
    data(lv_efor_top) = gs_main-fatura_efor_top.
    data(lv_efor_tip) = gs_main-fatura_tip.
    data(lv_fat_danisman) = gs_main-fat_danisman.


    if gs_main-konum_durum eq 'X'.
      gs_main-konum_durum = 'ONSITE'.
    else.
      gs_main-konum_durum = 'REMOTE'.
    endif.

    clear gs_main-kayit_tarih.
    gs_main-kayit_tarih = sy-datum.

    gs_main-kayit_atan = sy-uname.
    if gs_main-aktive_num is initial or iv_new = abap_true.
      gs_main-aktive_num = me->get_aktivite_num( ).
    endif.


    if gs_main-fat_danisman ne gs_main-danisman and iv_new = abap_true.
      clear gs_main-fatura_efor.
      clear gs_main-fatura_efor_gun.
      clear gs_main-fatura_efor_top.
      gs_main-fatura_tip = '07'.
      gs_main-efor_statu = 'R'.
    endif.

    move-corresponding gs_main to ls_aktivite.
    modify zfnl_aktvt_01 from ls_aktivite.
    commit work.

    "---->icayci 11.09.2023 17:51
    if gs_main-fat_danisman ne gs_main-danisman and iv_new = abap_true.
      clear gs_main-aktivite_sure.
      gs_main-fatura_efor = lv_f_efor.
      gs_main-fatura_efor_gun = lv_efor_gun.
      gs_main-fatura_efor_top = lv_efor_top.
      gs_main-danisman = gs_main-fat_danisman.
      gs_main-aktive_num = me->get_aktivite_num( ).
      gs_main-fatura_tip = lv_efor_tip.
      perform efor_statu.
      move-corresponding gs_main to ls_aktivite.
      modify zfnl_aktvt_01 from ls_aktivite.
      commit work.

      clear gs_main-danisman.
      gs_main-danisman = sy-uname.
    endif.
    "<----icayci 11.09.2023 17:51

    if gs_main-fat_danisman ne gs_main-danisman.
      concatenate gs_main-danisman 'Ve' gs_main-fat_danisman ' İçin Efor Kaydedildi' into lv_msg separated by space.
      concatenate gs_main-danisman 'Ve' gs_main-fat_danisman ' İçin Yeni Efor Kaydedildi' into lv_yk_msg separated by space.
    else.
      concatenate gs_main-danisman  ' Efor Kaydedildi' into lv_msg separated by space.
      concatenate gs_main-danisman  ' Yeni Efor Kaydedildi' into lv_yk_msg separated by space.
    endif.

*    CONCATENATE gs_main-danisman 'Ve' gs_main-fat_danisman ' İçin Efor Kaydedildi' INTO lv_msg SEPARATED BY space.
*    CONCATENATE gs_main-danisman 'Ve' gs_main-fat_danisman ' İçin Yeni Efor Kaydedildi' INTO lv_yk_msg SEPARATED BY space.

    if sy-ucomm eq 'KAYDET'.
      message lv_msg type 'I'.

    elseif sy-ucomm eq 'YENI_KYT'.
      message lv_yk_msg type 'I'.
    endif.

    if gv_kntrl is initial.
      me->get_data( ).
      me->fill_data( ).
      me->get_domain_value( ).
      me->refresh_alv( ).
    endif.

  endmethod.
  method delete_data.


    data: lv_ans(1).
    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = ' işlem gerçekleşiyor'
        text_question         = 'Silmek istediğinize emin misiniz?'
        text_button_1         = 'Evet'
        icon_button_1         = ' ICON_CHECKED'
        text_button_2         = 'Hayır'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ''
        popup_type            = 'ICON_MESSAGE_ERROR'
      importing
        answer                = lv_ans.

    if lv_ans eq '1'.
      read table mt_aktivite into data(ls_aktivite) index gv_index.
      if sy-subrc = 0.
        delete mt_aktivite index gv_index.
        delete from zfnl_aktvt_01 where aktive_num eq ls_aktivite-aktive_num.
      endif.
    elseif lv_ans eq '2'.
      return.
    endif.
    me->refresh_alv( ).

  endmethod.

  method screen_parameters.


*    IF gs_main-fatura_tip = 01.
*      gs_main-efor_statu = 'P'.
*    ENDIF.
*    IF gs_main-fatura_tip = 02.
*      gs_main-efor_statu = 'O'.
*    ENDIF.
*    IF gs_main-fatura_tip = 03.
*      gs_main-efor_statu = 'O'.
*    ENDIF.
*    IF gs_main-fatura_tip = 04.
*      gs_main-efor_statu = 'O'.
*    ENDIF.
*    IF gs_main-fatura_tip = 05.
*      gs_main-efor_statu = 'B'.
*    ENDIF.
*    IF gs_main-fatura_tip = 06.
*      gs_main-efor_statu = 'O'.
*    ENDIF.
*    IF gs_main-fatura_tip = 07.
*      gs_main-efor_statu = 'R'.
*    ENDIF.


    if gs_main-fatura_tip = 01.
      if gs_main-fat_durumu = 01.
        gs_main-efor_statu = 'O'.
      else.
        gs_main-efor_statu = 'R'.
      endif.
    endif.
    if gs_main-fatura_tip = 02.
      if gs_main-fat_durumu = 01.
        gs_main-efor_statu = 'O'.
      else.
        gs_main-efor_statu = 'R'.
      endif.
    endif.
    if gs_main-fatura_tip = 03.
      gs_main-efor_statu = 'O'.
    endif.
    if gs_main-fatura_tip = 04.
      gs_main-efor_statu = 'O'.
    endif.
    if gs_main-fatura_tip = 05.
      gs_main-efor_statu = 'B'.
    endif.
    if gs_main-fatura_tip = 06.
      if gs_main-fat_durumu = 01.
        gs_main-efor_statu = 'O'.
      else.
        gs_main-efor_statu = 'R'.
      endif.
    endif.
    if gs_main-fatura_tip = 07.
      gs_main-efor_statu = 'R'.
    endif.


    gs_main-fatura_efor_top = ( gs_main-fatura_efor * '0.125' ) + gs_main-fatura_efor_gun.


*    SELECT SINGLE MODUL FROM ZFNL_DANISMAN
*                        INTO @gs_modul-modul
*      WHERE danisman EQ @gs_main-danisman.

    select single butxt from t001
                        into gs_main-butxt
                       where bukrs = gs_main-bukrs.

    select single * from znfl_prj_t_001
               into @data(ls_prj)
                   where bukrs   = @gs_main-bukrs
                     and prj_kod = @gs_main-prj_kod .
    if sy-subrc = 0.

      gs_main-prj_ack     = ls_prj-prj_ack.
      gs_main-musteri     = ls_prj-musteri.
      gs_main-fat_musteri = ls_prj-fat_musteri.
      if gs_main-fatura_tip is initial.
        gs_main-fatura_tip = ls_prj-fat_tip.
      endif.

    endif.

    select single mus_tanim from znfl_prj_m_001
                             into gs_main-mus_tanim
                            where musteri = gs_main-musteri.


    select single mus_tanim from znfl_prj_m_002
                             into gs_main-fat_mus_tanim
                            where fat_musteri = gs_main-fat_musteri.

*    CLEAR gs_main-fat_durumu.
    if gs_main-fat_durumu is initial.

      select single fat_durumu from znfl_prj_t_001
                               into gs_main-fat_durumu
                              where musteri = gs_main-musteri.
    endif.

    select single post1 from prps
                        into gs_main-post1
                       where posid = gs_main-posid.


    select single danisman from zfnl_danisman
                        into gs_main-ad_soyad
                        where zfnl_danisman~kullaniici eq gs_main-danisman.

    select single danisman from zfnl_danisman
                        into gs_main-onay_ad_soyad
                        where zfnl_danisman~kullaniici eq gs_main-ony_danisman.


*    SELECT SINGLE prj_ack FROM znfl_prj_t_001
*                           INTO gs_main-prj_ack
*                          WHERE prj_kod = gs_main-prj_kod.

*LOOP AT gs_main INTO data(ls_main).
*
*ENDLOOP.


  endmethod.

  method get_date.

    me->get_month_range( ).
    call selection-screen 120 starting at 10 5 ending at 90 10.
    me->get_data( ).
    me->fill_data( ).
    me->refresh_alv( ).

  endmethod.

  method is_belgesi_getir.
    data : returncode.
    data : ivals type table of sval,
           xvals type sval.
    data : lv_isnum type zfnl_aktvt_is_01.

    clear: xvals, ivals[].
    xvals-tabname   = 'ZFNL_AKTVT_IS_02'.
    xvals-fieldname = 'IS_NUM'.
    append xvals to ivals.

    call function 'POPUP_GET_VALUES'
      exporting
        popup_title     = 'İş belgesi seçiniz'
      importing
        returncode      = returncode
      tables
        fields          = ivals
      exceptions
        error_in_fields = 1
        others          = 2.

    check returncode is initial.

    read table ivals into xvals with key fieldname = 'IS_NUM'.
    if sy-subrc  = 0.
      condense xvals-value.
      lv_isnum = xvals-value.
    else.
      message 'Lütfen belge giriniz' type 'I'.
      return.
    endif.

    select single * from zfnl_aktvt_is_01
                    into gs_is
                   where is_num = lv_isnum.


    select * from zfnl_aktvt_is_02
       into table mt_islist
            where is_num = lv_isnum.


  endmethod.

  method danisman_ekle.
    data : ls_is type zfnl_aktvt_is_02.

    move-corresponding gs_is to ls_is.
    modify zfnl_aktvt_is_02 from ls_is.


  endmethod.

  method is_getir.

    move-corresponding gs_is to gs_main.

  endmethod.

  method get_month_range.
    call function 'HR_JP_MONTH_BEGIN_END_DATE'
      exporting
        iv_date             = sy-datum
      importing
        ev_month_begin_date = s_tarih-low
        ev_month_end_date   = s_tarih-high.

    data: lr_tarih type range of sy-datum.
    append value #( sign = 'I'
                    option = 'BT'
                    low = s_tarih-low
                    high = s_tarih-high ) to s_tarih.


  endmethod.
  method get_domain_value.
    data: gt_tab  type table of dd07v,
          gwa_tab type dd07v.
    data: gt_d_tab  type table of dd07v,
          gwa_d_tab type dd07v.
    data: g_domain type dd07l-domname.
    data: g_d_domain type dd07l-domname.

    g_domain = 'ZFNL_DM_FATURA_TIP'.
    call function 'GET_DOMAIN_VALUES'
      exporting
        domname         = g_domain
      tables
        values_tab      = gt_tab
      exceptions
        no_values_found = 1
        others          = 2.

    g_d_domain = 'ZFNL_DM_DURUM'.
    call function 'GET_DOMAIN_VALUES'
      exporting
        domname         = g_d_domain
      tables
        values_tab      = gt_d_tab
      exceptions
        no_values_found = 1
        others          = 2.

    loop at mt_aktivite into data(ls_aktivite).
      read table gt_tab into gwa_tab with key domvalue_l = ls_aktivite-fatura_tip.
      ls_aktivite-fat_tip_tanim = gwa_tab-ddtext.
      read table gt_d_tab into gwa_d_tab with key domvalue_l = ls_aktivite-durum.
      ls_aktivite-durum_tanim = gwa_d_tab-ddtext.
      modify mt_aktivite from ls_aktivite.
    endloop.

  endmethod.
  method akt_getir.
    data lt_fcat type lvc_t_fcat.
    data ls_layo type lvc_s_layo.

    select * from zfnl_aktvt_01
      into corresponding fields of table @gt_akt
      where danisman   in @s_aktg
        and aktive_num in @s_aktv.

    call function 'LVC_FIELDCATALOG_MERGE'
      exporting
        i_structure_name = 'ZFNL_AKTGET_S'
      changing
        ct_fieldcat      = lt_fcat.

    call function 'REUSE_ALV_GRID_DISPLAY_LVC'
      exporting
        i_callback_program      = sy-repid
        i_structure_name        = 'ZFNL_AKTGET_S'
        is_layout_lvc           = ls_layo
        it_fieldcat_lvc         = lt_fcat
        i_callback_user_command = 'USER_COMMAND'
*       it_events               = gt_events
        i_screen_start_column   = 60
        i_screen_start_line     = 15
        i_screen_end_column     = 200
        i_screen_end_line       = 50
      tables
        t_outtab                = gt_akt.

  endmethod.

endclass.
*&---------------------------------------------------------------------*
*& Form pspnr_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- S_AKTG
*&---------------------------------------------------------------------*
form pspnr_help  changing p_s_aktg.

  data : returncode.
  data : return_tab type table of ddshretval.
  data : s_return_tab type ddshretval.


  select isim, sap_user, modul,danisman from znfl_prj_m_003
                  into table @data(lt_prj).



  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'SRCH_GETIR' "=> alan
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_AKTG' "=> alan adi
      value_org   = 'S'
    tables
      value_tab   = lt_prj " => alan tablosu(kendimiz oluşturabiliriz)
      return_tab  = return_tab.


  check return_tab[] is not initial.
  read table return_tab into s_return_tab index 1.
  p_s_aktg = s_return_tab-fieldval.
endform.
*&---------------------------------------------------------------------*
*& Form efor_statu
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form efor_statu .
  clear gs_main-efor_statu.

  if gs_main-fatura_tip = 01.
    if gs_main-fat_durumu = 01.
      gs_main-efor_statu = 'O'.
    else.
      gs_main-efor_statu = 'R'.
    endif.
  endif.
  if gs_main-fatura_tip = 02.
    if gs_main-fat_durumu = 01.
      gs_main-efor_statu = 'O'.
    else.
      gs_main-efor_statu = 'R'.
    endif.
  endif.
  if gs_main-fatura_tip = 03.
    gs_main-efor_statu = 'O'.
  endif.
  if gs_main-fatura_tip = 04.
    gs_main-efor_statu = 'O'.
  endif.
  if gs_main-fatura_tip = 05.
    gs_main-efor_statu = 'B'.
  endif.
  if gs_main-fatura_tip = 06.
    if gs_main-fat_durumu = 01.
      gs_main-efor_statu = 'O'.
    else.
      gs_main-efor_statu = 'R'.
    endif.
  endif.
  if gs_main-fatura_tip = 07.
    gs_main-efor_statu = 'R'.
  endif.
endform.
