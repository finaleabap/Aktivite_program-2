*&---------------------------------------------------------------------*
*& Include          ZFNL_PROJE_YONETIMI_CLS
*&---------------------------------------------------------------------*

CLASS gcl_class DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA : mo_instance TYPE REF TO gcl_class.
    CLASS-DATA : mt_out      TYPE STANDARD TABLE OF znfl_prj_s_002.
    CLASS-DATA : mo_grid  TYPE REF TO cl_gui_alv_grid,
                 mo_cutom TYPE REF TO cl_gui_custom_container.


    CONSTANTS : mc_container TYPE char20        VALUE 'CC1',
                mc_structure TYPE dd02l-tabname VALUE 'ZNFL_PRJ_S_002',
                mc_selmod    TYPE char1         VALUE 'A',
                mc_back      TYPE char10        VALUE 'BACK',
                mc_leave     TYPE char10        VALUE 'LEAVE',
                mc_exit      TYPE char10        VALUE 'EXIT',
                mc_prjg      TYPE char10        VALUE 'PRJ_GTR',
                mc_kaydet    TYPE char10        VALUE 'SAVE'
                .


    DATA : ms_layout  TYPE lvc_s_layo,
           ms_variant TYPE disvariant.

    DATA : mt_fieldcat TYPE lvc_t_fcat.



    CLASS-METHODS:
      class_constructor,
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO gcl_class.

    METHODS:
      get_data
        RETURNING VALUE(r_error) TYPE char1,
      display_alv
        IMPORTING
          it_data TYPE STANDARD TABLE,
      run,

      handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id
          es_row_no
          sender,


      double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row
                  e_column ,

      handle_toolbar_set
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,

      handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,
      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      initialization,

      refresh_alv,

      proje_getir,

      kaydet,

      check_data
        RETURNING VALUE(rv_error) TYPE char1,

      calculate_efor,

      calculate_maliyet


      .

  PRIVATE SECTION.

    METHODS:
      fill_data,
      create_fieldcat
        IMPORTING
          iv_structure_name TYPE dd02l-tabname
          it_data           TYPE STANDARD TABLE OPTIONAL
        EXPORTING
          et_fieldcat       TYPE lvc_t_fcat,

      create_object
        IMPORTING
          iv_container TYPE c
        EXPORTING
          er_grid      TYPE REF TO cl_gui_alv_grid,

      set_layout
        IMPORTING
          iv_zebra  TYPE char1 OPTIONAL
          iv_edit   TYPE char1 OPTIONAL
          iv_sel    TYPE char1 OPTIONAL
          iv_toll_b TYPE char1 OPTIONAL
          iv_cwidth TYPE char1 OPTIONAL
        CHANGING
          cs_layout TYPE lvc_s_layo,

      set_handler
        IMPORTING
          ir_grid TYPE REF TO cl_gui_alv_grid,

      ekle
        IMPORTING
          index_rows TYPE lvc_t_row,

      sil
        IMPORTING
          index_rows TYPE lvc_t_row     ,

      check_proje
        RETURNING VALUE(ev_return) TYPE char1  ,

      get_proje_kod
        RETURNING VALUE(rv_pkodu) TYPE char10     .

ENDCLASS.

CLASS gcl_class IMPLEMENTATION.
  METHOD class_constructor.

    IF mo_instance IS NOT BOUND.
      mo_instance = NEW #( ).
    ENDIF.

  ENDMETHOD.


  METHOD run.

    CALL SCREEN 0200.

  ENDMETHOD.

  METHOD get_instance.

    ro_instance = mo_instance.

  ENDMETHOD.

  METHOD get_data.


    IF mt_out IS INITIAL.
      r_error = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD fill_data.


  ENDMETHOD.

  METHOD create_fieldcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       I_BUFFER_ACTIVE        =
        i_structure_name       = iv_structure_name
*       I_CLIENT_NEVER_DISPLAY = 'X'
*       I_BYPASSING_BUFFER     =
      CHANGING
        ct_fieldcat            = et_fieldcat[]
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    LOOP AT et_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>) WHERE fieldname = 'DANISMAN'
                                                             OR fieldname = 'KAYNAK'
                                                             OR fieldname = 'EFOR'
                                                             OR fieldname = 'DON_BAS'
                                                             OR fieldname = 'DON_BIT'
                                                             OR fieldname = 'RATE'
                                                             OR fieldname = 'PB'.
      <fs_fcat>-edit = 'X'.
    ENDLOOP.

  ENDMETHOD.

  METHOD display_alv.
    DATA : ls_stable TYPE lvc_s_stbl .

    IF mo_cutom IS INITIAL.

      me->create_fieldcat(
        EXPORTING
          iv_structure_name = mc_structure
          it_data           = mt_out
        IMPORTING
          et_fieldcat       = mt_fieldcat
      ).

      me->create_object(
        EXPORTING
          iv_container = mc_container
        IMPORTING
          er_grid      = mo_grid
      ).

      me->set_layout(
        EXPORTING
          iv_zebra  = abap_true
*          iv_edit   = abap_true
          iv_sel    = mc_selmod
*          iv_toll_b = abap_true
        CHANGING
          cs_layout = ms_layout
      ).

      ms_variant-report   = sy-repid.
      ms_variant-username = sy-uname.

      CALL METHOD mo_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      CALL METHOD mo_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      me->set_handler( ir_grid = mo_grid ).
      CALL METHOD mo_grid->set_table_for_first_display
        EXPORTING
          i_save                        = 'A'
          i_default                     = 'X'
          is_layout                     = ms_layout
          is_variant                    = ms_variant
*         it_toolbar_excluding          = lt_exclude
        CHANGING
          it_outtab                     = mt_out[]
          it_fieldcatalog               = mt_fieldcat[]
*         it_sort                       = t_sort
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.




    ELSE.


      me->refresh_alv( ).

    ENDIF.


  ENDMETHOD.

  METHOD create_object.

    FREE mo_cutom.

    CREATE OBJECT mo_cutom
      EXPORTING
        container_name = iv_container.

    CREATE OBJECT er_grid
      EXPORTING
        i_parent = mo_cutom.

  ENDMETHOD.

  METHOD set_layout.

    cs_layout-zebra      = iv_zebra.
    cs_layout-edit       = iv_edit.
    cs_layout-sel_mode   = iv_sel.
    cs_layout-no_toolbar = iv_toll_b.
    cs_layout-cwidth_opt = iv_cwidth.


  ENDMETHOD.


  METHOD handle_hotspot_click.
    DATA: ls_col_id   TYPE lvc_s_col.

*    BREAK-POINT.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD double_click.

*    READ TABLE mt_out INTO DATA(ls_out) INDEX e_row-index.
*    CASE e_column.
*      WHEN 'BELNR'.
*        SET PARAMETER ID 'BEL' FIELD ls_out-belnr.
*        SET PARAMETER ID 'BUK' FIELD ls_out-bukrs.
*        SET PARAMETER ID 'GJR' FIELD ls_out-gjahr.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*    ENDCASE.

    me->refresh_alv( ).


  ENDMETHOD.

  METHOD handle_toolbar_set.
    DATA:  ty_toolbar      TYPE stb_button.

    CLEAR ty_toolbar.
    CLEAR ty_toolbar.
    "  · ekrana buton ekleme

    CLEAR ty_toolbar.
    MOVE 3 TO ty_toolbar-butn_type. "separator
    APPEND ty_toolbar TO e_object->mt_toolbar.


    ty_toolbar-function = 'EKLE'. "name of btn to  catch click
    ty_toolbar-butn_type = 0.
    MOVE icon_insert_row  TO ty_toolbar-icon.
    ty_toolbar-text = 'Ekle'.
    APPEND ty_toolbar  TO e_object->mt_toolbar.

    ty_toolbar-function = 'SIL'. "name of btn to  catch click
    ty_toolbar-butn_type = 0.
    MOVE icon_delete_row  TO ty_toolbar-icon.
    ty_toolbar-text = 'Sil'.
    APPEND ty_toolbar  TO e_object->mt_toolbar.

    DELETE e_object->mt_toolbar
       WHERE function EQ '&DETAIL'
          OR function EQ '&&SEP00'
          OR function EQ '&CHECK'
          OR function EQ '&REFRESH'
          OR function EQ '&&SEP01'
          OR function EQ '&LOCAL&CUT'
          OR function EQ '&LOCAL&COPY'
          OR function EQ '&LOCAL&PASTE'
          OR function EQ '&LOCAL&UNDO'
          OR function EQ '&&SEP02'
          OR function EQ '&LOCAL&APPEND'
          OR function EQ '&LOCAL&INSERT_ROW'
          OR function EQ '&LOCAL&DELETE_ROW'
          OR function EQ '&LOCAL&COPY_ROW'
          OR function EQ '&&SEP03'
          OR function EQ '&&SEP06'
          OR function EQ '&GRAPH'
          OR function EQ '&&SEP07'
          OR function EQ '&INFO'.

  ENDMETHOD.                    "handle_toolbar_set


  METHOD handle_user_command.
    DATA: index_rows TYPE lvc_t_row,
          index      LIKE LINE OF index_rows.

    CLEAR index_rows. REFRESH index_rows.

    CALL METHOD mo_grid->get_selected_rows
      IMPORTING
        et_index_rows = index_rows.

    DATA: wr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
    DATA: lt_rows  TYPE lvc_t_row,
          lt_index TYPE  lvc_s_row-index.

    DATA : gi_index_rows TYPE lvc_t_row .
    DATA : wa_index_rows TYPE LINE OF lvc_t_row .

    CASE e_ucomm.
      WHEN 'EKLE'.
        me->ekle( index_rows = index_rows ).
      WHEN 'SIL'.
        me->sil( index_rows = index_rows ).
    ENDCASE.

    me->refresh_alv( ).

  ENDMETHOD.                    "handle_user_command



  METHOD handle_data_changed.
    DATA: ls_good      TYPE lvc_s_modi,
          ls_sflight   TYPE sflight,
          ls_good_cost TYPE lvc_s_modi.

    DATA: ls_modi TYPE lvc_s_modi.
    DATA: ls_stbl TYPE lvc_s_stbl.

    FIELD-SYMBOLS : <fs_table> TYPE ANY TABLE.


    ASSIGN er_data_changed->mp_mod_rows->* TO FIELD-SYMBOL(<fs_mod_rows>).
    ASSIGN er_data_changed->mt_mod_cells TO FIELD-SYMBOL(<fs_cells>).

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      DATA(lv_name) = '<FS_MOD_ROWS>'.
      ASSIGN (lv_name) TO <fs_table>.
      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_tab>).
*        ls_table = <fs_tab>.
        READ TABLE mt_out ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX ls_good-row_id.
        CASE ls_good-fieldname.
          WHEN 'DANISMAN'.
            SELECT SINGLE * FROM znfl_prj_m_003
                            INTO @DATA(ls_003)
                           WHERE danisman = @ls_good-value.
            MOVE-CORRESPONDING ls_003 TO <fs_out>.
          WHEN 'URUN'.

          WHEN 'VADE_AYI'.

        ENDCASE.

      ENDLOOP.
    ENDLOOP.

    me->refresh_alv( ).


  ENDMETHOD. "handle_data_changed

  METHOD set_handler.

    CREATE OBJECT go_ctrl.

    SET HANDLER go_ctrl->double_click         FOR ir_grid.
    SET HANDLER go_ctrl->handle_data_changed  FOR ir_grid.
    SET HANDLER go_ctrl->handle_hotspot_click FOR ir_grid.
    SET HANDLER go_ctrl->handle_toolbar_set   FOR ir_grid.
    SET HANDLER go_ctrl->handle_user_command  FOR ir_grid.


  ENDMETHOD.

  METHOD initialization.
    gs_t_001-bukrs = '1023'.
*    gs_t_001-
  ENDMETHOD.

  METHOD refresh_alv.
    DATA : ls_stable TYPE lvc_s_stbl .
    ls_stable-col = abap_true.
    ls_stable-row = abap_true.

    CALL METHOD mo_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ENDMETHOD.

  METHOD ekle.

    DATA : ls_002 TYPE znfl_prj_s_002.

    MOVE-CORRESPONDING gs_t_001 TO ls_002.
    APPEND ls_002 TO mt_out.

  ENDMETHOD.

  METHOD sil.

    LOOP AT index_rows INTO DATA(ls_row).
      DELETE mt_out INDEX ls_row-index.
    ENDLOOP.

  ENDMETHOD.

  METHOD proje_getir.
    DATA : returncode.
    DATA: ivals TYPE TABLE OF sval,
          xvals TYPE sval.
    DATA : lv_prj_kod TYPE zfnl_de_prj_kod.

    CLEAR: xvals, ivals[].
    xvals-tabname   = 'ZNFL_PRJ_T_001'.
    xvals-fieldname = 'PRJ_KOD'.
    APPEND xvals TO ivals.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Proje seçiniz'
      IMPORTING
        returncode      = returncode
      TABLES
        fields          = ivals
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK returncode IS INITIAL.

    READ TABLE ivals INTO xvals WITH KEY fieldname = 'PRJ_KOD'.
    IF sy-subrc  = 0.
      CONDENSE xvals-value.
      lv_prj_kod = xvals-value.
      lv_prj_kod = |{ lv_prj_kod ALPHA = IN }|.
    ELSE.
      MESSAGE 'Proje seçiniz' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM znfl_prj_t_001
                    INTO gs_t_001
                   WHERE prj_kod = lv_prj_kod.
    IF sy-subrc <> 0.
      MESSAGE 'Lütfen doğru proje kodu seçiniz' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT * FROM znfl_prj_t_002
       INTO TABLE mt_out
            WHERE prj_kod = gs_t_001-prj_kod.

    SELECT * FROM znfl_prj_m_003
             INTO TABLE @DATA(lt_003).

    LOOP AT mt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      READ TABLE lt_003 INTO DATA(ls_003) WITH KEY danisman = <fs_out>-danisman.
      MOVE-CORRESPONDING ls_003 TO <fs_out>.
      CLEAR ls_003.
    ENDLOOP.

    me->refresh_alv( ).

  ENDMETHOD.

  METHOD kaydet.
    DATA : ls_t_001 TYPE znfl_prj_t_001.
    DATA : lt_t_002 TYPE TABLE OF znfl_prj_t_002.

    DATA(lv_return) = check_proje( ).
    CHECK lv_return NE 'X'.

    DATA(lv_qust) = |Proje kaydedilecek, onaylıyor musunuz?|.
    DATA : ans(1).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Proje Kayıt '
        text_question         = lv_qust
        text_button_1         = 'Evet'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Hayır'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
        popup_type            = 'ICON_MESSAGE_ERROR'
      IMPORTING
        answer                = ans.
    IF ans = 2.
      LEAVE PROGRAM.
    ELSEIF ans = 1.

*      DATA(lv_err) = me->check_data( ).

*      CHECK lv_err IS INITIAL.
      IF gs_t_001-prj_kod = |$        1|.
        gs_t_001-prj_kod = me->get_proje_kod( ).
      ENDIF.

      MOVE-CORRESPONDING gs_t_001 TO ls_t_001.
      MOVE-CORRESPONDING me->mt_out TO lt_t_002.


      LOOP AT lt_t_002 ASSIGNING FIELD-SYMBOL(<fs_out>).
        <fs_out>-prj_kod = gs_t_001-prj_kod.
      ENDLOOP.

      MODIFY znfl_prj_t_001 FROM ls_t_001.
      MODIFY znfl_prj_t_002 FROM TABLE lt_t_002.

    ENDIF.


  ENDMETHOD.

  METHOD check_proje.

    IF gs_t_001-bukrs IS INITIAL.
      MESSAGE 'Lütfen şirket kodu giriniz' TYPE 'I' DISPLAY LIKE 'E'.
      ev_return = 'X'.
      EXIT.
    ENDIF.

    IF gs_t_001-musteri IS INITIAL.
      MESSAGE 'Lütfen müşteri giriniz' TYPE 'I' DISPLAY LIKE 'E'.
      ev_return = 'X'.
      EXIT.
    ENDIF.

    IF gs_t_001-fat_musteri IS INITIAL.
      MESSAGE 'Lütfen fatura müşterisi giriniz' TYPE 'I' DISPLAY LIKE 'E'.
      ev_return = 'X'.
      EXIT.
    ENDIF.

    IF gs_t_001-fat_tip IS INITIAL.
      MESSAGE 'Lütfen fatura tipi giriniz' TYPE 'I' DISPLAY LIKE 'E'.
      ev_return = 'X'.
      EXIT.
    ENDIF.

    IF gs_t_001-pspnr IS INITIAL.
      MESSAGE 'Lütfen proje numarasını giriniz' TYPE 'I' DISPLAY LIKE 'E'.
      ev_return = 'X'.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD check_data.

    IF gs_t_001-bukrs IS INITIAL.
      MESSAGE 'Lütfen şirket kodunu doldurunuz' TYPE 'I' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    IF gs_t_001-musteri IS INITIAL.
      MESSAGE 'Lütfen SAP Müşterisini doldurunuz' TYPE 'I' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    IF gs_t_001-fat_musteri IS INITIAL.
      MESSAGE 'Lütfen Fatura Müşterisini doldurunuz' TYPE 'I' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    IF gs_t_001-tur IS INITIAL.
      MESSAGE 'Lütfen Tür doldurunuz' TYPE 'I' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    IF gs_t_001-fat_tip IS INITIAL.
      MESSAGE 'Lütfen Fatura Tipini doldurunuz' TYPE 'I' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

*    IF gs_t_001-fat_tip IS INITIAL.
*      MESSAGE 'Lütfen Fatura Tipini doldurunuz' TYPE 'I' DISPLAY LIKE 'E'.
*      rv_error = abap_true.
*    ENDIF.

  ENDMETHOD.

  METHOD get_proje_kod.
    DATA: lv_rtn TYPE inri-returncode.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZPROJE_KOD'
      IMPORTING
        number                  = rv_pkodu
        returncode              = lv_rtn
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

  ENDMETHOD.

  METHOD calculate_efor.
    DATA : lv_trh1 TYPE datum.
    DATA : lv_trh2 TYPE datum.
    DATA : lv_months TYPE vtbbewe-atage.
*    READ TABLE mt_out INTO DATA(ls_out) INDEX 1.
*    DATA(lv_tarih) = ls_out-don_bit - ls_out-don_bas.
*
*    gv_efor = REDUCE netpr( INIT x = CONV netpr( 0 ) FOR ms_out IN mt_out NEXT x = x + ms_out-efor * ( ms_out-don_bit - ms_out-don_bas ) ).
    CLEAR gv_efor.
    LOOP AT mt_out INTO DATA(ls_out).
      lv_trh1 = |{ ls_out-don_bas(04) }{ ls_out-don_bas+5(02) }01|.
      lv_trh2 = |{ ls_out-don_bit(04) }{ ls_out-don_bit+5(02) }01|.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = lv_trh1
*         I_KEY_DAY_FROM       =
          i_date_to   = lv_trh2
*         I_KEY_DAY_TO         =
*         I_FLG_SEPARATE       = ' '
*         I_FLG_ROUND_UP       = 'X'
        IMPORTING
*         E_DAYS      =
          e_months    = lv_months
*         E_YEARS     =
        .

      gv_efor = gv_efor + ( ls_out-efor * lv_months ).

    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_maliyet.
    DATA : lv_trh1 TYPE datum.
    DATA : lv_trh2 TYPE datum.
    DATA : lv_months TYPE vtbbewe-atage.

*    gv_maliyet = REDUCE netpr( IN  IT x = CONV netpr( 0 ) FOR ms_out IN mt_out NEXT x = x + ( ms_out-efor * ( ms_out-don_bit - ms_out-don_bas ) * ms_out-rate ) ).
    CLEAR gv_maliyet.
    LOOP AT mt_out INTO DATA(ls_out).
      lv_trh1 = |{ ls_out-don_bas(04) }{ ls_out-don_bas+5(02) }01|.
      lv_trh2 = |{ ls_out-don_bit(04) }{ ls_out-don_bit+5(02) }01|.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = lv_trh1
*         I_KEY_DAY_FROM       =
          i_date_to   = lv_trh2
*         I_KEY_DAY_TO         =
*         I_FLG_SEPARATE       = ' '
*         I_FLG_ROUND_UP       = 'X'
        IMPORTING
*         E_DAYS      =
          e_months    = lv_months
*         E_YEARS     =
        .

      gv_maliyet = gv_efor + ( ls_out-efor * lv_months * ls_out-rate ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
