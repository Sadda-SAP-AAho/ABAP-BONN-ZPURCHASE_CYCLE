CLASS zcl_purchase_cycle_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PURCHASE_CYCLE_TEST IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.


*    MODIFY ENTITIES OF i_purchaseordertp_2
*     ENTITY PurchaseOrder
*         CREATE FIELDS ( PurchaseOrderType CompanyCode PurchasingOrganization PurchasingGroup Supplier )
*         WITH VALUE #( (
*             %cid = 'PO'
*             %data = VALUE #(
*             PurchaseOrderType = 'ZRAW'
*             CompanyCode = 'BBPL'
*             PurchasingOrganization = 'BB00'
*             PurchasingGroup = '101'
*             Supplier = '0021000081'
*             ) ) )
*
*
*CREATE BY \_PurchaseOrderItem
*FIELDS ( Material Plant OrderQuantity )
* WITH VALUE #( ( %cid_ref = 'PO'
*            PurchaseOrder = space
*             %target  = VALUE #( ( %cid  = 'POI'
*             Material = 'AB195'
*             Plant = 'BB02'
*             OrderQuantity = 1
*                  )
*                  ( %cid  = 'POI2'
*             Material = 'AB195'
*             Plant = 'BB02'
*             OrderQuantity = 3
*                  )
*                   ) ) )
*
*    MAPPED DATA(ls_po_mapped)
*    FAILED DATA(ls_po_failed)
*    REPORTED DATA(ls_po_reported).

 MODIFY ENTITIES OF i_purchaseordertp_2
     ENTITY PurchaseOrder
         CREATE FIELDS ( PurchaseOrderType CompanyCode PurchasingOrganization PurchasingGroup SupplyingPlant  )
         WITH VALUE #( (
             %cid = 'PO'
             %data = VALUE #(
             PurchaseOrderType = 'ZINB'
             CompanyCode = 'BBPL'
             PurchasingOrganization = 'BB00'
             PurchasingGroup = '105'
             SupplyingPlant  = 'BB02'
             ) ) )


CREATE BY \_PurchaseOrderItem
FIELDS ( Material Plant OrderQuantity PurchaseOrderItemCategory )
 WITH VALUE #( ( %cid_ref = 'PO'
            PurchaseOrder = space
             %target  = VALUE #( ( %cid  = 'POI'
             Material = 'TW35'
             Plant = 'BB04'
             OrderQuantity = 3
*             PurchaseOrderItem = '10'
*             PurchaseOrderItemCategory = '7'
                  )
*                  ( %cid  = 'POI2'
*             Material = 'TW3'
*             Plant = 'BB02'
*             OrderQuantity = 3
*                  )
                   ) ) )

    MAPPED DATA(ls_po_mapped)
    FAILED DATA(ls_po_failed)
    REPORTED DATA(ls_po_reported).

    COMMIT ENTITIES BEGIN
        RESPONSE OF I_PurchaseOrderTP_2
        FAILED DATA(ls_save_failed)
        REPORTED DATA(ls_save_reported).

    LOOP AT ls_po_mapped-purchaseorder ASSIGNING FIELD-SYMBOL(<fs_header>).
      CONVERT KEY OF i_purchaseordertp_2 FROM <fs_header>-%pid TO <fs_header>-%key.
        DATA(lv_po_number) = <fs_header>-%key-purchaseorder.
    ENDLOOP.

* TYPES:  BEGIN OF ty_purchaseorderitem_key,
*    purchaseorder     TYPE I_PurchaseOrderItemTP_2-purchaseorder,
*    purchaseorderitem TYPE I_PurchaseOrderItemTP_2-purchaseorderitem,
*  END OF ty_purchaseorderitem_key.
*
*      DATA: lt_so_item_temp_keys  TYPE TABLE OF ty_purchaseorderitem_key,
*            lt_so_item_final_keys TYPE TABLE OF ty_purchaseorderitem_key,
*            ls_so_item_temp_key   TYPE ty_purchaseorderitem_key,
*            ls_so_item_final_key  TYPE ty_purchaseorderitem_key.
*
*      LOOP AT ls_po_mapped-purchaseorderitem ASSIGNING FIELD-SYMBOL(<ls_mapped_item>).
*        MOVE-CORRESPONDING <ls_mapped_item> TO ls_so_item_temp_key.
*        APPEND ls_so_item_temp_key TO lt_so_item_temp_keys.
*      ENDLOOP.
*
*      LOOP AT lt_so_item_temp_keys INTO ls_so_item_temp_key.
*        CONVERT KEY OF I_PurchaseOrderItemTP_2 FROM ls_so_item_temp_key TO ls_so_item_final_key.
*        APPEND ls_so_item_final_key TO lt_so_item_final_keys.
*      ENDLOOP.

*    DATA: ls_so_temp_key              TYPE STRUCTURE FOR KEY OF i_purchaseordertp_2.
*
*    CONVERT KEY OF i_purchaseordertp_2 FROM ls_so_temp_key TO DATA(ls_so_final_key).

    COMMIT ENTITIES END.

*CREATE BY \_purchaseorderitem


*    TYPES: tt_purorder_items_create TYPE TABLE FOR CREATE i_purchaseordertp_2\_purchaseorderitem,
*           ty_purorder_items_create TYPE LINE OF tt_purorder_items_create.
*
*        DATA(lt_item) = VALUE tt_purorder_items_create( ( %cid_ref = 'PO'
*                                                      %target  = VALUE #( ( %cid       = 'POI'
*                                                                            Material = 'AB195'
*                                                                            Plant = 'BB02'
*                                                                            OrderQuantity = 10
**                                                                           PurchaseContract     = ls_pr_key-purchasecontract
**                                                                           PurchasecontractItem = '00010'
*                                                       %control = VALUE #( plant                = cl_abap_behv=>flag_changed
*                                                                           orderquantity        = cl_abap_behv=>flag_changed
*                                                                           purchaseorderitem    = cl_abap_behv=>flag_changed
**                                                                           PurchaseContract     = cl_abap_behv=>flag_changed
**                                                                           AccountAssignmentCategory = cl_abap_behv=>flag_changed
**                                                                           PurchaseContractItem  = cl_abap_behv=>flag_changed )
*                                                                          ) ) ) ) ).



*            CREATE BY \_purchaseorderitem
*                FROM lt_item

* " Check if process is not failed
*    cl_abap_unit_assert=>assert_initial( ls_po_failed-purchaseorder ).
*    cl_abap_unit_assert=>assert_initial( ls_po_reported-purchaseorder ).

*    ls_mapped_root_late-%pre = VALUE #( %tmp = ls_mapped-purchaseorder[ 1 ]-%key ).
*    COMMIT ENTITIES BEGIN RESPONSE OF I_PurchaseOrderTP_2 FAILED DATA(lt_po_res_failed) REPORTED DATA(lt_po_res_reported).
*    "Special processing for Late numbering to determine the generated document number.
*    LOOP AT ls_po_mapped-purchaseorder ASSIGNING FIELD-SYMBOL(<fs_po_mapped>).
*      CONVERT KEY OF I_PurchaseOrderTP_2 FROM <fs_po_mapped>-%key TO DATA(ls_po_key).
*      <fs_po_mapped>-PurchaseOrder = ls_po_key-PurchaseOrder.
*    ENDLOOP.
*    COMMIT ENTITIES END.
  ENDMETHOD.
ENDCLASS.
