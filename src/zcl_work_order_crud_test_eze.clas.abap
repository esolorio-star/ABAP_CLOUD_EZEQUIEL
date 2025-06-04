CLASS zcl_work_order_crud_test_eze DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS:
      ensure_master_data_exists.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_work_order_crud_test_eze IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    ensure_master_data_exists( ).
    DATA(lo_crud) = NEW zcl_work_order_crud_handler_ez( ).
    DATA(logs) = VALUE string_table( ).


    DATA(ls_order) = VALUE zstr_work_order(
      work_order_id  = '1000000003'
*      customer_id    = '00000002'
*      technician_id  = 'TECH0002'
*      creation_date  = cl_abap_context_info=>get_system_date( )
*      status         = 'PE'
*      priority       = 'A'
*      description    = 'Orden  demo 5'
    ).

    TRY.
        " Crear
         lo_crud->create_work_order( is_order = ls_order ).
         APPEND '✔ Orden creada exitosamente' TO logs.


*       " Leer
        DATA(ls_result) = VALUE zstr_work_order( ).
        lo_crud->read_work_order(
          EXPORTING
            iv_work_order_id = ls_order-work_order_id
          IMPORTING
            es_order = ls_result ).
          APPEND |✔ Orden leída: { ls_result-description }| TO logs.
*
*       " Actualizar
        lo_crud->update_work_order( is_order = ls_order ).
        APPEND '✔ Orden actualizada correctamente' TO logs.
*
*        " Eliminar

        lo_crud->delete_work_order( iv_work_order_id = ls_order-work_order_id ).
        APPEND '✔ Orden eliminada correctamente' TO logs.

      CATCH zcx_work_order_validation INTO DATA(lx).
        APPEND |❌ ERROR: { lx->msg }| TO logs.
    ENDTRY.

    " Mostrar logs en consola
    out->write( |Resultado de prueba de CRUD:{ cl_abap_char_utilities=>newline }| ).
    LOOP AT logs INTO DATA(line).
      out->write( line ).
    ENDLOOP.
  ENDMETHOD.

  METHOD ensure_master_data_exists.

    " Cliente
    SELECT SINGLE customer_id
      FROM ztcustomer_eze
      WHERE customer_id = '00000002'
      INTO @DATA(lv_dummy).
    IF sy-subrc <> 0.
      DATA: ls_customer TYPE ztcustomer_eze.

      ls_customer = VALUE ztcustomer_eze(
        customer_id = '00000002'
        name        = 'Cliente Demo 2'
        address     = 'Calle Ficticia 2'
        phone       = '555-4321'
      ).

      INSERT ztcustomer_eze FROM @ls_customer.
    ENDIF.

    " Técnico
    SELECT SINGLE technician_id
      FROM zttechnician_eze
      WHERE technician_id = 'TECH0002'
      INTO @DATA(lv_technician_id).

    IF sy-subrc <> 0.
       DATA: ls_tech TYPE zttechnician_eze.
  ls_tech = VALUE zttechnician_eze(
    technician_id = 'TECH0002'
    name          = 'Técnico Demo 2'
  ).
  INSERT zttechnician_eze FROM @ls_tech.
    ENDIF.

    " Prioridad
    SELECT SINGLE priority_code
      FROM ztpriority_eze
      WHERE priority_code = 'B'
      INTO @DATA(lv_priority_code).
    IF sy-subrc <> 0.
      DATA: ls_prio TYPE ztpriority_eze.
  ls_prio = VALUE ztpriority_eze(
    priority_code = 'B'
    priority_desc   = 'Baja'
  ).
  INSERT ztpriority_eze FROM @ls_prio.
    ENDIF.

    " Estado
    SELECT SINGLE status_code
      FROM ztstatus_eze
      WHERE status_code = 'CO'
      INTO @DATA(lv_status_code).
    IF sy-subrc <> 0.
    data: ls_status type ztstatus_eze.
    ls_status = VALUE ztstatus_eze(
        status_code = 'CO'
        status_desc = 'Completada'
     ).
      insert ztstatus_eze from @ls_status.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
