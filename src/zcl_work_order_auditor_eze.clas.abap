CLASS zcl_work_order_auditor_eze DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  TYPES: ty_order TYPE ztwork_order_eze.

    METHODS:
      log_history
        IMPORTING
          is_old_order TYPE ty_order.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_auditor_eze IMPLEMENTATION.

 METHOD log_history.

    " Obtener timestamp
    DATA: lv_date TYPE dats,
          lv_time TYPE t.
    GET TIME STAMP FIELD DATA(lv_ts).
    lv_date = cl_abap_context_info=>get_system_date( ).
    lv_time = cl_abap_context_info=>get_system_time( ).

    " Generar ID único para el historial
    DATA: lv_history_id TYPE ztwork_order_hez-history_id.
    lv_history_id = |{ lv_date }{ lv_time }|.

    DATA: ls_hist TYPE ztwork_order_hez.
    ls_hist = VALUE ztwork_order_hez(
      history_id        = lv_history_id
      work_order_id     = is_old_order-work_order_id
      modification_date = lv_date
      change_time       = lv_time
      change_description = 'Actualización de orden'
      customer_id       = is_old_order-customer_id
      technician_id     = is_old_order-technician_id
      status            = is_old_order-status
      priority          = is_old_order-priority
      description       = is_old_order-description
      changed_by        = sy-uname
    ).

    INSERT ztwork_order_hez FROM @ls_hist.

  ENDMETHOD.

ENDCLASS.
