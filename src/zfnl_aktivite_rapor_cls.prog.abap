*&---------------------------------------------------------------------*
*& Include          ZFNL_AKTIVITE_RAPOR_CLS
*&---------------------------------------------------------------------*
class gcl_class definition.

  public section.

    class-data : mo_instance type ref to gcl_class.
    class-data : mt_out      type standard table of zfnl_aktvt_s. "burayada oluşturduğumuz tabloyu yazıyoruz
    class-data : mo_grid  type ref to cl_gui_alv_grid,
                 mo_cutom type ref to cl_gui_custom_container.

    constants : mc_container type char20        value 'CC1', "buraya da screende layout içindeki containerin adını yazıyoruz.
                mc_structure type dd02l-tabname value 'ZFNL_AKTVT_S',  "şuraya oluşturduğumuz tablonun  strcucterini yazıyoruz
                mc_selmod    type char1         value 'A',
                mc_back      type char10        value 'BACK',
                mc_leave     type char10        value 'LEAVE',
                mc_exit      type char10        value 'EXIT'.

    constants : mc_i_red type char30        value '@5C@',
                mc_i_gre type char30        value '@5B@',
                mc_i_yel type char30        value '@5D@',
                mc_i_ok  type char30        value '@01@',
                mc_i_non type char30        value '@02@',
                mc_i_hst type char30        value '@96@'.

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

      run,

      handle_hotspot_click

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

      refresh_alv

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
          iv_container type c
        exporting
          er_grid      type ref to cl_gui_alv_grid,

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
          ir_grid type ref to cl_gui_alv_grid  .

endclass.
class gcl_class implementation.

  method class_constructor.

    if mo_instance is not bound.
      mo_instance = new #( ).
    endif.

  endmethod.

  method run.
    data(lv_err) = me->get_data( ).
    check lv_err is initial.
    me->fill_data( ).
*    display_alv( mt_out ).
    call screen 0100.

  endmethod.

  method get_instance.

    ro_instance = mo_instance.

  endmethod.

  method get_data.

    data: lv_status type zfnl_aktvt_01-efor_statu.

    case 'X'.
      when p_rad1.
        clear: lv_status.
      when p_rad2.
        lv_status = 'O'.
      when p_rad3.
        lv_status = 'P'.
      when p_rad4.
        lv_status = 'B'.
      when others .
        lv_status = 'R'.
    endcase.



    data: lr_range type range of zfnl_aktvt_01-efor_statu.



    select efor_statu from zfnl_aktvt_01
      into table @data(lt_status).

    if lv_status is not initial.
      append value #(   sign = 'I'
                              option = 'EQ'
                              low = lv_status )
                              to lr_range.
    endif.


    select     t1~bukrs, t1~prj_kod, t1~prj_ack, t1~pspnr, t1~musteri, t1~fat_musteri,
               t2~post1,
               t3~mus_tanim,
               t4~aktive_num, t4~aktivite_ack, t4~posid, t4~tarih, t4~aktivite_sure, t4~fatura_efor_top, t4~durum, t4~ipt_neden, t4~statu, t4~danisman, t4~efor_statu, t4~kayit_atan, t4~fat_danisman, t4~fat_durumu, t4~konum_durum, t4~mus_danis, t4~ticket,
               t4~ek_alan, t4~fatura_efor,
               t5~post1 as post2,
               t6~isim, t6~modul,
               t7~mus_tanim as mus_tanim_2
                from znfl_prj_t_001 as t1
          left join proj as t2
          on t1~bukrs eq t2~vbukr
          and t1~pspnr eq t2~pspnr
          left join znfl_prj_m_001 as t3
          on t1~musteri eq t3~musteri
          left join zfnl_aktvt_01 as t4
          on t1~bukrs eq t4~bukrs
          and t1~prj_kod eq t4~prj_kod
          left join prps as t5
          on  t5~posid eq t4~posid
          and t4~bukrs eq t5~pbukr
          left join znfl_prj_m_003 as t6
          on t4~danisman eq t6~danisman
          left join znfl_prj_m_002 as t7
          on t1~fat_musteri eq t7~fat_musteri
                into corresponding fields of table @mt_out
                where t4~efor_statu in @lr_range
      and t1~prj_kod in @s_projek
      and t1~musteri in @s_muster
      and t1~fat_musteri in @s_fmstri
      and t1~pspnr in @s_sproje
      and t4~posid in @s_pypoge
      and t4~tarih in @s_aktvte
      and t4~fat_danisman in @s_fatdan
      and t4~danisman in @s_danis.

    if mt_out is initial.
      r_error = abap_true.
    endif.

  endmethod.

  method fill_data.

    loop at mt_out into data(ls_out).

      ls_out-aktivite_gun = ls_out-aktivite_sure * '0.125'.
      modify mt_out from ls_out.

      if ls_out-ipt_neden is not initial.
        ls_out-line_color = 'C610'.
        modify mt_out from ls_out.
      endif.

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

  endmethod.

  method display_alv.

    data : ls_stable type lvc_s_stbl .

    if mo_cutom is initial.

      me->create_fieldcat(
        exporting
          iv_structure_name = mc_structure
          it_data           = mt_out
        importing
          et_fieldcat       = mt_fieldcat
      ).

      me->create_object(
        exporting
          iv_container = mc_container
        importing
          er_grid      = mo_grid

      ).
      me->set_layout(
        exporting
          iv_zebra  = abap_true
*          iv_edit   = abap_true
          iv_sel    = mc_selmod
          iv_toll_b = abap_false
          iv_cwidth = 'X'
        changing
          cs_layout = ms_layout
      ).

      ms_variant-report   = sy-repid.
      ms_variant-username = sy-uname.
      me->set_handler( ir_grid = mo_grid ).

      call method mo_grid->set_table_for_first_display
        exporting
          i_save                        = 'A'
          i_default                     = 'X'
          is_layout                     = ms_layout
          is_variant                    = ms_variant
*         it_toolbar_excluding          = lt_exclude
        changing
          it_outtab                     = mt_out[]
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

  method create_object.

    free mo_cutom.

    create object mo_cutom
      exporting
        container_name = iv_container.

    create object er_grid
      exporting
        i_parent = mo_cutom.

  endmethod.

  method set_layout.

    cs_layout-zebra      = iv_zebra.
    cs_layout-edit       = iv_edit.
    cs_layout-sel_mode   = iv_sel.
    cs_layout-no_toolbar = iv_toll_b.
    cs_layout-cwidth_opt = iv_cwidth.
    cs_layout-info_fname = 'LINE_COLOR'.

  endmethod.

  method handle_hotspot_click.


  endmethod.                    "handle_hotspot_click

  method double_click.

    case e_column.

      when 'VBELN'.

      when 'RADIO1'.

    endcase.

    me->refresh_alv( ).

  endmethod.

  method handle_toolbar_set.

    data:  ty_toolbar      type stb_button.
*
    clear ty_toolbar.
*    CLEAR ty_toolbar.
*
*    "  · ekrana buton ekleme
*
    ty_toolbar-function = 'YENILE'. "name of btn to  catch click
    ty_toolbar-butn_type = 0.
    move icon_refresh  to ty_toolbar-icon.
    ty_toolbar-text = 'Yenile'.
    append ty_toolbar  to e_object->mt_toolbar.
*
*    ty_toolbar-function = 'SIL'. "name of btn to  catch click
*    ty_toolbar-butn_type = 0.
*    MOVE icon_delete_row  TO ty_toolbar-icon.
*    ty_toolbar-text = 'Sil'.
*    APPEND ty_toolbar  TO e_object->mt_toolbar.
*
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

    call method mo_grid->get_selected_rows
      importing
        et_index_rows = index_rows.



    data: wr_data_changed type ref to cl_alv_changed_data_protocol.

    data: lt_rows  type lvc_t_row,

          lt_index type  lvc_s_row-index.

    data : gi_index_rows type lvc_t_row .
    data : wa_index_rows type line of lvc_t_row .

    case e_ucomm.

      when 'YENILE'.
        me->get_data( ).
        me->fill_data( ).

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

          when 'URUN'.

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
    set handler go_ctrl->handle_hotspot_click for ir_grid.
    set handler go_ctrl->handle_toolbar_set   for ir_grid.
    set handler go_ctrl->handle_user_command  for ir_grid.

  endmethod.


  method initialization.

  endmethod.



  method refresh_alv.

    data : ls_stable type lvc_s_stbl .

    ls_stable-col = abap_true.
    ls_stable-row = abap_true.

    call method mo_grid->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

  endmethod.

endclass.
*&---------------------------------------------------------------------*
*& Form pspnr_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form pspnr_help changing p_p_pspnr.

  data : returncode.
  data : return_tab like table of ddshretval.
  data : s_return_tab like ddshretval.


  select bukrs, prj_ack, musteri, pspnr  from znfl_prj_t_001
                  into table @data(lt_prj).





  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'PSPNR' "=> alan
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_SPROJE' "=> alan adi
      value_org   = 'S'
    tables
      value_tab   = lt_prj " => alan tablosu(kendimiz oluşturabiliriz)
      return_tab  = return_tab.


  check return_tab[] is not initial.
  read table return_tab into s_return_tab index 1.
  p_p_pspnr = s_return_tab-fieldval.

endform.
*&---------------------------------------------------------------------*
*& Form pypoge_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- S_PYPOGE
*&---------------------------------------------------------------------*
form pypoge_help  changing p_s_pypoge.

  data : returncode.
  data : return_tab like table of ddshretval.
  data : s_return_tab like ddshretval.


  select post1, posid  from prps
                  into table @data(lt_oge).





  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'POSID' "=> alan
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_PYPOGE' "=> alan adi
      value_org   = 'S'
    tables
      value_tab   = lt_oge " => alan tablosu(kendimiz oluşturabiliriz)
      return_tab  = return_tab.


  check return_tab[] is not initial.
  read table return_tab into s_return_tab index 1.
  p_s_pypoge = s_return_tab-fieldval.

endform.
*&---------------------------------------------------------------------*
*& Form pydanis_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- S_DANIS
*&---------------------------------------------------------------------*
form pydanis_help  changing p_s_danis.
  data : returncode.
  data : return_tab like table of ddshretval.
  data : s_return_tab like ddshretval.


  select danisman, modul, kullaniici from zfnl_danisman
                  into table @data(lt_danis).





  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'KULLANIICI' "=> alan
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_DANIS' "=> alan adi
      value_org   = 'S'
    tables
      value_tab   = lt_danis " => alan tablosu(kendimiz oluşturabiliriz)
      return_tab  = return_tab.


  check return_tab[] is not initial.
  read table return_tab into s_return_tab index 1.
  p_s_danis = s_return_tab-fieldval.
endform.
*&---------------------------------------------------------------------*
*& Form kosul_range
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form kosul_range
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
