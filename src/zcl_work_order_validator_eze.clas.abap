CLASS zcl_work_order_validator_eze DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  TYPES: ty_customer_id   TYPE n LENGTH 8,
           ty_technician_id TYPE c LENGTH 8,
           ty_priority      TYPE c LENGTH 1,
           ty_status        TYPE c LENGTH 2.
  METHODS:

      validate_customer_id
        IMPORTING i_customer_id TYPE ty_customer_id
        RAISING   cx_static_check,

      validate_technician_id
        IMPORTING i_technician_id TYPE ty_technician_id
        RAISING   cx_static_check,

      validate_priority
        IMPORTING i_priority TYPE ty_priority
        RAISING   cx_static_check,

      validate_status
        IMPORTING i_status TYPE ty_status
        RAISING   cx_static_check.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WORK_ORDER_VALIDATOR_EZE IMPLEMENTATION.


METHOD validate_customer_id.
    DATA lv_customer_id TYPE ty_customer_id.
    SELECT SINGLE customer_id
       FROM ztcustomer_eze
       WHERE customer_id = @i_customer_id
       INTO @lv_customer_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_WORK_ORDER_VALIDATION
        EXPORTING iv_msg = 'Cliente no válido'.
    ENDIF.
  ENDMETHOD.


  METHOD validate_priority.
    DATA lv_priority TYPE ty_priority.
    SELECT SINGLE priority_code
    FROM ztpriority_eze
      WHERE priority_code = @i_priority
      INTO @lv_priority.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_WORK_ORDER_VALIDATION
        EXPORTING iv_msg = 'Prioridad no válida'.
    ENDIF.
  ENDMETHOD.


  METHOD validate_status.

   DATA lv_status TYPE ty_status.
    SELECT SINGLE status_code
    FROM ztstatus_eze
    WHERE status_code = @i_status
      INTO @lv_status.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_WORK_ORDER_VALIDATION
        EXPORTING iv_msg = 'Estado no válido'.
    ENDIF.
  ENDMETHOD.


  METHOD validate_technician_id.
    DATA lv_technician_id TYPE ty_technician_id.
    SELECT SINGLE technician_id
    FROM zttechnician_eze
      WHERE technician_id = @i_technician_id
      INTO @lv_technician_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_WORK_ORDER_VALIDATION
        EXPORTING iv_msg = 'Técnico no válido'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
