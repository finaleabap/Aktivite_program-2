class ZFNL_AKTV_01 definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  final
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.

  methods HANDLE_CSRF_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZFNL_AKTV_01 IMPLEMENTATION.


  method HANDLE_CSRF_TOKEN.
*CALL METHOD SUPER->HANDLE_CSRF_TOKEN
*  EXPORTING
*    IO_CSRF_HANDLER =
*    IO_REQUEST      =
*    IO_RESPONSE     =
*    .
  endmethod.


  method if_rest_application~get_root_handler.
*CALL METHOD SUPER->IF_REST_APPLICATION~GET_ROOT_HANDLER
*  RECEIVING
*    RO_ROOT_HANDLER =
*    .

    data(io_router) = new cl_rest_router( ).

    io_router->attach(
      exporting
        iv_template      = '/aktivite' "name
        iv_handler_class = 'ZFNL_AKTV_02' "yeni oluşturulan class yazılıcak.
*        it_parameter     = it_parameter
    ).

    ro_root_handler = io_router.

  endmethod.
ENDCLASS.
