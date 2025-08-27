class ZFNL_AKTV_02 definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods IF_REST_RESOURCE~GET
    redefinition .
  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZFNL_AKTV_02 IMPLEMENTATION.


  method IF_REST_RESOURCE~GET.
*CALL METHOD SUPER->IF_REST_RESOURCE~GET
*    .
  endmethod.


  method if_rest_resource~post.
*CALL METHOD SUPER->IF_REST_RESOURCE~POST
*  EXPORTING
*    IO_ENTITY =
*    .
    data: lv_json          type string,
          ls_data          type zfnl_aktvt_01,
          lv_statu         type string value '200',
          lv_json_response type string.

    lv_json  = io_entity->get_string_data( ).
    check lv_json is not initial.

    /ui2/cl_json=>deserialize( exporting json = lv_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                changing data = ls_data ).

    call function 'ZFNL_AKTIVITE_01'
      exporting
        is_structure = ls_data.

    lv_json_response = /ui2/cl_json=>serialize( data          = lv_statu
                                        compress      = abap_false
                                       ).

    mo_response->create_entity( )->set_string_data( iv_data = lv_json_response ).

  endmethod.
ENDCLASS.
