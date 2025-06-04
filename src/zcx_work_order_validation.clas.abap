CLASS zcx_work_order_validation DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   DATA msg TYPE string.
    METHODS constructor
      IMPORTING iv_msg TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_WORK_ORDER_VALIDATION IMPLEMENTATION.


METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    msg = iv_msg.
  ENDMETHOD.
ENDCLASS.
