CLASS zc_stockpurchase DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .


    INTERFACES if_oo_adt_classrun .

   CLASS-METHODS getCID RETURNING VALUE(cid) TYPE abp_behv_cid.

   CLASS-METHODS runJob  .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zc_stockpurchase IMPLEMENTATION.

    METHOD if_apj_dt_exec_object~get_parameters.
        " Return the supported selection parameters here
        et_parameter_def = VALUE #(
          ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'Create Interbranch PO'   lowercase_ind = abap_true changeable_ind = abap_true )
        ).

        " Return the default parameters values here
        et_parameter_val = VALUE #(
          ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = 'Create Interbranch PO' )
        ).

    ENDMETHOD.

    METHOD if_apj_rt_exec_object~execute.
        runJob(  ).
     ENDMETHOD.


      METHOD if_oo_adt_classrun~main .
        runJob(  ).
      ENDMETHOD.


      METHOD runJob.

*        DATA POList TYPE TABLE OF zinv_mst.

        SELECT SINGLE FROM zintegration_tab
        FIELDS intgpath
        WHERE intgmodule  = 'SALESFILTER'
        INTO  @data(it_integration).

        IF it_integration IS NOT INITIAL AND it_integration NE ''.
            SELECT a~imno,a~comp_code,a~plant,a~imfyear,a~imtype,a~po_no,a~cust_code FROM zinv_mst as a
               INNER JOIN zinv_mst_filter AS b
               ON a~comp_code  = b~comp_code
                AND a~plant      = b~plant
                AND a~imfyear    = b~imfyear
                AND a~imtype     = b~imtype
                AND a~imno       = b~imno
                where a~po_tobe_created = 1 and a~po_processed = 0
                INTO TABLE @DATA(POList).

        ELSE.
            SELECT imno,comp_code,plant,imfyear,imtype,po_no,cust_code FROM zinv_mst
                where po_tobe_created = 1 and po_processed = 0
                INTO TABLE @POList.
        ENDIF.

        LOOP AT POList INTO DATA(PODetails).

*            CREATING RANDOMCID
             DATA(POcid) = getCID(  ).

*            Getting Line Details
             SELECT FROM zinvoicedatatab1
             FIELDS idqtybag, remarks, idcat, idid, idno, idpartycode, idprdasgncode, idprdqty,idprdrate, idtdiscamt, idtotaldiscamt, idprdbatch
                WHERE comp_code = @PODetails-comp_code and plant = @PODetails-plant and idfyear = @PODetails-imfyear and  idtype = @PODetails-imtype
                and idno = @PODetails-imno and idprdasgncode NE '000000001400000023'
                INTO TABLE @DATA(po_lines).

*            Getting Plant
             REPLACE ALL OCCURRENCES OF 'CV' IN PODetails-cust_code WITH ''.

             select single from I_PLANTPURCHASINGORGANIZATION
             fields PurchasingOrganization
             where Plant = @PODetails-cust_code
             INTO @DATA(PurchasingOrganization).

             select single from ZI_PlantTable
             fields compcode
             where plantcode = @PODetails-cust_code
             into @DATA(CompCode).

             CONCATENATE 'CV' PODetails-plant INTO DATA(Supplier).

            MODIFY ENTITIES OF I_PURCHASEORDERTP_2
            ENTITY PurchaseOrder
                CREATE FIELDS ( PurchaseOrderType CompanyCode PurchasingOrganization PurchasingGroup Supplier )
                WITH VALUE #( (
                    %cid = POcid
                    %data = Value #(
                        PurchaseOrderType = 'ZSTO'
                        CompanyCode = CompCode
                        PurchasingOrganization = PurchasingOrganization
                        PurchasingGroup = '106'
                        Supplier = Supplier
                    )
                    %control = VALUE #(
                           purchaseordertype      = cl_abap_behv=>flag_changed
                           companycode            = cl_abap_behv=>flag_changed
                           purchasingorganization = cl_abap_behv=>flag_changed
                           purchasinggroup        = cl_abap_behv=>flag_changed
                           supplier               = cl_abap_behv=>flag_changed
                    )
                ) )

            CREATE BY \_purchaseorderitem
                 FROM VALUE #( ( %cid_ref = POcid
                      PurchaseOrder = space
                      %target = VALUE #( FOR po_line IN po_lines INDEX INTO i (
                            %cid =  |{ POcid }{ i WIDTH = 3 ALIGN = RIGHT PAD = '0' }|
                            Material = po_line-idprdasgncode
                            OrderQuantity = po_line-idprdqty
                            Plant = PODetails-cust_code
                            Batch = po_line-idprdbatch
*                            NetPriceAmount = po_line-idprdrate
                            %control = VALUE #(
                                    plant                = cl_abap_behv=>flag_changed
                                    orderquantity        = cl_abap_behv=>flag_changed
                                    purchaseorderitem    = cl_abap_behv=>flag_changed
                                    Batch    = cl_abap_behv=>flag_changed
*                                    NetPriceAmount       = cl_abap_behv=>flag_changed
                            )
                        ) )
                     ) )
             ENTITY PurchaseOrderItem
            CREATE BY \_PurOrdPricingElement
            FIELDS ( conditiontype conditionrateamount conditioncurrency conditionquantity )
            WITH VALUE #(
              FOR po_line IN po_lines INDEX INTO j
              (
                %cid_ref = |{ POcid }{ j WIDTH = 3 ALIGN = RIGHT PAD = '0' }|
                PurchaseOrder = space
                PurchaseOrderItem = space
                %target = VALUE #(
                ( %cid =  |{ POcid }{ j }_01|
                    conditiontype = 'PMP0'
                    conditionrateamount = ( po_line-idprdrate - ( po_line-idtotaldiscamt / po_line-idprdqty ) ) * 100
                    conditioncurrency = 'INR'
                    conditionquantity = 100
                  )
                )
              )
            )


            REPORTED DATA(ls_po_reported)
            FAILED   DATA(ls_po_failed)
            MAPPED   DATA(ls_po_mapped).

           COMMIT ENTITIES BEGIN
              RESPONSE OF I_PurchaseOrderTP_2
              FAILED DATA(ls_save_failed)
              REPORTED DATA(ls_save_reported).


                IF lines( ls_po_mapped-purchaseorder ) > 0.
                    LOOP AT ls_po_mapped-purchaseorder ASSIGNING FIELD-SYMBOL(<fs_header>).
                      CONVERT KEY OF i_purchaseordertp_2 FROM <fs_header>-%pid TO <fs_header>-%key.
                      DATA(lv_po_number) = <fs_header>-%key-purchaseorder.
                    ENDLOOP.
                ELSE.

*                    To store error

                ENDIF.
        COMMIT ENTITIES END.

            if lv_po_number = ''.
                CONTINUE.
            ENDIF.

            UPDATE zinv_mst SET po_processed = 1, po_no = @lv_po_number WHERE imno = @PODetails-imno.

        CLEAR: po_lines.
        ENDLOOP.

      ENDMETHOD.

      METHOD getCID.
            TRY.
                cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
            CATCH cx_uuid_error.
                ASSERT 1 = 0.
            ENDTRY.
      ENDMETHOD.
ENDCLASS.
