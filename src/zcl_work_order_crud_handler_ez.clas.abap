CLASS zcl_work_order_crud_handler_ez DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  TYPES: BEGIN OF ty_work_order,
             work_order_id TYPE n LENGTH 10,
             customer_id   TYPE n LENGTH 8,
             technician_id TYPE c LENGTH 8,
             creation_date TYPE dats,
             status        TYPE c LENGTH 2,
             priority      TYPE c LENGTH 1,
             description   TYPE c LENGTH 50,
           END OF ty_work_order.

    METHODS:
      create_work_order
        IMPORTING !is_order TYPE zstr_work_order
        RAISING   zcx_work_order_validation,

      read_work_order
        IMPORTING iv_work_order_id TYPE ty_work_order-work_order_id
        EXPORTING es_order TYPE ty_work_order
        RAISING zcx_work_order_validation,

      update_work_order
        IMPORTING is_order TYPE ty_work_order
        RAISING   zcx_work_order_validation,

      delete_work_order
        IMPORTING iv_work_order_id TYPE ty_work_order-work_order_id
        RAISING   zcx_work_order_validation.
  PROTECTED SECTION.
  PRIVATE SECTION.
  DATA mo_validator TYPE REF TO zcl_work_order_validator_eze.
ENDCLASS.



CLASS zcl_work_order_crud_handler_ez IMPLEMENTATION.

METHOD create_work_order.
    " Validaciones
    CREATE OBJECT mo_validator.
    mo_validator->validate_customer_id( is_order-customer_id ).
    mo_validator->validate_technician_id( is_order-technician_id ).
    mo_validator->validate_priority( is_order-priority ).
    mo_validator->validate_status( is_order-status ).


    " Insertar orden
    INSERT INTO ztwork_order_eze VALUES @is_order.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING iv_msg = 'Error al crear la orden de trabajo'.
    ENDIF.
  ENDMETHOD.

  METHOD read_work_order.
    SELECT SINGLE *
      FROM ztwork_order_eze
      WHERE work_order_id = @iv_work_order_id
      INTO @es_order.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING iv_msg = 'Orden no encontrada'.
    ENDIF.
  ENDMETHOD.

  METHOD update_work_order.
    " Validar existencia
    SELECT SINGLE status
      FROM ztwork_order_eze
      WHERE work_order_id = @is_order-work_order_id
      INTO @DATA(lv_status).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING
          iv_msg = 'La orden no existe'.
    ENDIF.

    IF lv_status <> 'PE'.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING
          iv_msg = 'Solo se puede modificar órdenes en estado Pendiente'.
    ENDIF.

    " Leer datos antiguos
    SELECT SINGLE * FROM ztwork_order_eze
      WHERE work_order_id = @is_order-work_order_id
      INTO @DATA(ls_old_order).

    IF sy-subrc = 0.
      " Registrar historial
      DATA(lo_auditor) = NEW zcl_work_order_auditor_eze( ).
      lo_auditor->log_history( is_old_order = ls_old_order ).
    ENDIF.

    " Validar nuevos datos
    CREATE OBJECT mo_validator.
    mo_validator->validate_priority( is_order-priority ).
    mo_validator->validate_status( is_order-status ).

    " Actualizar
    UPDATE ztwork_order_eze SET
        status     = @is_order-status,
        priority   = @is_order-priority,
        description = @is_order-description
      WHERE work_order_id = @is_order-work_order_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING
          iv_msg = 'Error al actualizar la orden de trabajo'.
    ENDIF.
  ENDMETHOD.

  METHOD delete_work_order.
    " Validar existencia y estado
    SELECT SINGLE status FROM ztwork_order_eze
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(lv_status).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING iv_msg = 'Orden no encontrada'.
    ENDIF.

    IF lv_status <> 'PE'.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING iv_msg = 'Solo se pueden eliminar órdenes pendientes'.
    ENDIF.


    " Verificar historial
    SELECT SINGLE work_order_id FROM ztwork_order_hez
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(lv_exists).
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING iv_msg = 'No se puede eliminar una orden con historial'.
    ENDIF.

    DELETE FROM ztwork_order_eze WHERE work_order_id = @iv_work_order_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_work_order_validation
        EXPORTING iv_msg = 'Error al eliminar la orden de trabajo'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
