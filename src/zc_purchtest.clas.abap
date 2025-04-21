CLASS zc_purchtest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZC_PURCHTEST IMPLEMENTATION.


    METHOD if_oo_adt_classrun~main.

        update zinv_mst set po_tobe_created = 1, po_no = '' , po_processed = 0,migo_no = '', migo_processed = 0 where imno = '000178'.

*           NORMAL ORDER
*           MODIFY ENTITIES OF I_PURCHASEORDERTP_2
*            ENTITY PurchaseOrder
*                CREATE FIELDS ( PurchaseOrderType CompanyCode PurchasingOrganization PurchasingGroup Supplier )
*                WITH VALUE #( (
*                    %cid = 'PO'
*                    %data = Value #(
*                        PurchaseOrderType = 'ZRAW'
*                        CompanyCode = 'BBPL'
*                        PurchasingOrganization = 'BB00'
*                        PurchasingGroup = '001'
*                        Supplier = '0021000081'
*                    )
*                ) )
*
*            CREATE BY \_purchaseorderitem
*                FROM VALUE #( ( %cid_ref = 'PO'
*                                                      PurchaseOrder = space
*                                                      %target  = VALUE #( ( %cid       = 'POI'
*                                                                            Material = 'AB195'
*                                                                            Plant = 'BB02'
*                                                                            OrderQuantity = 100
*                                                       %control = VALUE #( plant                = cl_abap_behv=>flag_changed
*                                                                           orderquantity        = cl_abap_behv=>flag_changed
*                                                                           purchaseorderitem    = cl_abap_behv=>flag_changed
*                                                                          ) ) ) ) )
*            REPORTED DATA(ls_po_reported)
*              FAILED   DATA(ls_po_failed)
*              MAPPED   DATA(ls_po_mapped).

*                MODIFY ENTITIES OF I_PurchaseOrderTP_2
*                    ENTITY PurchaseOrder
*                    CREATE FIELDS ( PurchaseOrderType CompanyCode PurchasingOrganization PurchasingGroup Supplier )
*                    WITH VALUE #( (
*                        %cid = 'PO2-ZINB2'  " Unique CID to avoid MISSING_CID
*                        %data = VALUE #(
*                            PurchaseOrderType = 'ZSTO'
*                            CompanyCode = 'BNPL'
*                            PurchasingOrganization = 'BN00'
*                            PurchasingGroup = '106'
*                            Supplier = 'CVBN02'
*                        )
*                          %control = VALUE #(
*                           purchaseordertype      = cl_abap_behv=>flag_changed
*                           companycode            = cl_abap_behv=>flag_changed
*                           purchasingorganization = cl_abap_behv=>flag_changed
*                           purchasinggroup        = cl_abap_behv=>flag_changed
*                           supplier               = cl_abap_behv=>flag_changed
*                    )
*                    ) )
*                CREATE BY \_purchaseorderitem
*                    FIELDS ( Material Plant OrderQuantity PurchaseOrderItemCategory )
*                    WITH VALUE #( (
*                    %cid_ref = 'PO2-ZINB2'
*                                      %target = VALUE #( (
*                                        %cid = 'PO2-ZINB2-ITEM1'
*                                        Material = 'AOB'
*                                        Plant = 'BN03'
*                                        OrderQuantity = 100
*                                       %control = VALUE #(
*                                            material = cl_abap_behv=>flag_changed
*                                            plant = cl_abap_behv=>flag_changed
*                                            orderquantity = cl_abap_behv=>flag_changed
*                                            purchaseorderitem = cl_abap_behv=>flag_changed
*                            )
*                                      ) )
*                                   ) )
*
*                REPORTED DATA(ls_po_reported)
*                FAILED DATA(ls_po_failed)
*                MAPPED DATA(ls_po_mapped).
*
*
*
*                COMMIT ENTITIES BEGIN
*                    RESPONSE OF I_PurchaseOrderTP_2
*                    FAILED DATA(ls_save_failed)
*                    REPORTED DATA(ls_save_reported).
*
*         TYPES:  BEGIN OF ty_purchaseorderitem_key,
*            purchaseorder     TYPE I_PurchaseOrderItemTP_2-purchaseorder,
*            purchaseorderitem TYPE I_PurchaseOrderItemTP_2-purchaseorderitem,
*          END OF ty_purchaseorderitem_key.
*
*
*
*        IF lines( ls_po_mapped-purchaseorder ) > 0.
*            LOOP AT ls_po_mapped-purchaseorder ASSIGNING FIELD-SYMBOL(<fs_header>).
*              CONVERT KEY OF i_purchaseordertp_2 FROM <fs_header>-%pid TO <fs_header>-%key.
*              DATA(lv_po_number) = <fs_header>-%key-purchaseorder.
*            ENDLOOP.
*        ENDIF.
**
**      DATA: lt_so_item_temp_keys  TYPE TABLE OF ty_purchaseorderitem_key,
**            lt_so_item_final_keys TYPE TABLE OF ty_purchaseorderitem_key,
**            ls_so_item_temp_key   TYPE ty_purchaseorderitem_key,
**            ls_so_item_final_key  TYPE ty_purchaseorderitem_key.
**
**            LOOP AT ls_po_mapped-purchaseorderitem ASSIGNING FIELD-SYMBOL(<ls_mapped_item>).
**              CLEAR ls_so_item_temp_key.
**              MOVE-CORRESPONDING <ls_mapped_item> TO ls_so_item_temp_key.
**              ls_so_item_temp_key-purchaseorder = lv_po_number.
**              APPEND ls_so_item_temp_key TO lt_so_item_temp_keys.
**            ENDLOOP.
**
**
**        LOOP AT lt_so_item_temp_keys INTO ls_so_item_temp_key.
**            CONVERT KEY OF I_PurchaseOrderItemTP_2 FROM ls_so_item_temp_key TO ls_so_item_final_key.
**            APPEND ls_so_item_final_key TO lt_so_item_final_keys.
**        ENDLOOP.
*
*
**    DATA: ls_so_temp_key              TYPE STRUCTURE FOR KEY OF i_purchaseordertp_2.
**
**    CONVERT KEY OF i_purchaseordertp_2 FROM ls_so_temp_key TO DATA(ls_so_final_key).
*
*    COMMIT ENTITIES END.


*    out->write( 'Purchase order :' && lv_po_number ).
*
*        DATA(Header_data) = VALUE #( ( %cid = CID_PO
*            purchaseordertype = 'NB'
*            companycode = 1010
*            purchasingorganization = 1010
*            purchasinggroup = 001
*            supplier = 0010300001
*            %control = VALUE #( purchaseordertype = cl_abap_behv=>flag_changed
*            companycode = cl_abap_behv=>flag_changed
*            purchasingorganization = cl_abap_behv=>flag_changed
*            purchasinggroup = cl_abap_behv=>flag_changed
*            supplier = cl_abap_behv=>flag_changed ) ) ).
*
*        Item_data = VALUE #( ( %cid_ref = CID_PO
*                %target = VALUE #( ( %cid = CID_PO_ITEM
*                    material = TG0011
*                    manufacturermaterial = TG0011
*                    plant = 1010
*                    orderquantity = 4
*                    purchaseorderitem = '00010'
*                    netpriceamount = 12
*                        %control = VALUE #( material = cl_abap_behv=>flag_changed
*                        manufacturermaterial = cl_abap_behv=>flag_changed
*                        plant = cl_abap_behv=>flag_changed
*                        orderquantity = cl_abap_behv=>flag_changed
*                        purchaseorderitem = cl_abap_behv=>flag_changed
*                        netpriceamount = cl_abap_behv=>flag_changed ) )
*                        ) ) ).


*                test_data = VALUE #( ( %cid_ref = CID_PO_ITEM
*                PurchaseOrderItem = '00010'
*                %target = VALUE #( ( %cid = CID_ACCOUNT_ASSIGN
*                purchaseorderitem = '00010'
*                profitcenter = YB101
*                controllingarea = A000
*                accountassignmentnumber = '01'
*                costcenter = 0010101101
*                glaccount = 0065008500
*                %control = VALUE #( purchaseorderitem = cl_abap_behv=>flag_changed
*                profitcenter = cl_abap_behv=>flag_changed
*                controllingarea = cl_abap_behv=>flag_changed
*                accountassignmentnumber = cl_abap_behv=>flag_changed
*                costcenter = cl_abap_behv=>flag_changed
*                glaccount = cl_abap_behv=>flag_changed ) ) ) ) ).
*
*        rt_result = VALUE #( ( %cid_ref = CID_PO_ITEM
*            PurchaseOrderItem = '00010'
*            %target = VALUE #( ( %cid = CID_SCHEDULELINE
*            schedulelineorderquantity = 1
*            schedulelinedeliverydate = sy-datum + 7
*            scheduleline = '0001'
*            purchaseorderitem = '00010'
*            %control = VALUE #( schedulelineorderquantity = cl_abap_behv=>flag_changed
*            schedulelinedeliverydate = cl_abap_behv=>flag_changed
*            scheduleline = cl_abap_behv=>flag_changed
*            purchaseorderitem = cl_abap_behv=>flag_changed ) ) ) ) ).
*
*        rt_result = VALUE #( ( %cid_ref = mc_cid_purchaseorder
*        %target = VALUE #( ( %cid = mc_cid_supplieraddress
*        CityName = 'Waldorf'
*        StreetName = 'Baker'
*        Country = 'DE'
*        Region = 'BW'
*        PostalCode = '69190'
*        %control = VALUE #( CityName = cl_abap_behv=>flag_changed
*        StreetName = cl_abap_behv=>flag_changed
*        Country = cl_abap_behv=>flag_changed
*        Region = cl_abap_behv=>flag_changed ) ) ) ) ).
*
*        " Sub-Contracting Components
*        rt_result = VALUE #( ( %cid_ref = mc_cid_purorderscheduleline
*        PurchaseOrderItem = '00010'
*        %target = VALUE #( ( %cid = mc_cid_subcontractcomp
*        purchaseorderitem = '00010'
*        scheduleline = '0001'
*        plant = '1010'
*        material = 'TG0011'
*        reservationitem = '0001'
*        %control = VALUE #( purchaseorderitem = cl_abap_behv=>flag_changed
*        scheduleline = cl_abap_behv=>flag_changed
*        plant = cl_abap_behv=>flag_changed
*        material = cl_abap_behv=>flag_changed
*        reservationitem = cl_abap_behv=>flag_changed ) ) ) ) ).
*        rt_result = VALUE #( ( %cid_ref = mc_cid_purorderitem
*        purchaseorderitem = '00010'
*        %target = VALUE #( ( %cid = mc_cid_delivery_address
*        purchaseorderitem = '00010'
*        CityName = 'Waldorf'
*        StreetName = 'Baker'
*        Country = 'DE'
*        Region = 'BW'
*        PostalCode = '69190'
*        %control = VALUE #( PurchaseOrderItem = cl_abap_behv=>flag_changed
*        CityName = cl_abap_behv=>flag_changed
*        StreetName = cl_abap_behv=>flag_changed
*        Country = cl_abap_behv=>flag_changed
*        Region = cl_abap_behv=>flag_changed ) ) ) ) ).

    ENDMETHOD.
ENDCLASS.
