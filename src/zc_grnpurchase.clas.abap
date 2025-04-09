CLASS zc_grnpurchase DEFINITION
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



CLASS zc_grnpurchase IMPLEMENTATION.
     METHOD if_apj_dt_exec_object~get_parameters.
        " Return the supported selection parameters here
        et_parameter_def = VALUE #(
          ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'Purchase Order GRN'   lowercase_ind = abap_true changeable_ind = abap_true )
        ).

        " Return the default parameters values here
        et_parameter_val = VALUE #(
          ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = 'Purchase Order GRN' )
        ).

    ENDMETHOD.

    METHOD if_apj_rt_exec_object~execute.
        runJob(  ).
     ENDMETHOD.


      METHOD if_oo_adt_classrun~main .
        runJob(  ).
      ENDMETHOD.

      METHOD runJob.

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
                where a~po_processed = 1 and a~migo_processed = 0
                INTO TABLE @DATA(GRNList).

        ELSE.
            SELECT imno,comp_code,plant,imfyear,imtype,po_no,cust_code FROM zinv_mst
                where po_processed = 1 and migo_processed = 0
                INTO TABLE @GRNList.
        ENDIF.

            LOOP AT GRNList INTO DATA(GRNDetails).

*                CREATING RANDOMCID
                 DATA(GRNcid) = getCID(  ).

*                   Getting Line Details
                SELECT FROM zinvoicedatatab1 as a
                    join I_PurchaseOrderItemAPI01 as b on a~idprdasgncode = b~Material
                    Fields a~idqtybag, a~remarks, a~idcat, a~idid, a~idno, a~idpartycode, a~idprdbatch, a~idprdasgncode, a~idprdqty,a~idprdrate, a~idtdiscamt, b~PurchaseOrderItem, b~PurchaseOrderQuantityUnit
                    WHERE a~comp_code = @GRNDetails-comp_code and a~plant = @GRNDetails-plant and a~idfyear = @GRNDetails-imfyear and  a~idtype = @GRNDetails-imtype
                    and a~idno = @GRNDetails-imno AND b~PurchaseOrder = @GRNDetails-po_no
                    INTO TABLE @DATA(grn_lines).


*                Getting Plant
                 REPLACE ALL OCCURRENCES OF 'CV' IN GRNDetails-cust_code WITH ''.
                 CONCATENATE 'CV' GRNDetails-plant INTO DATA(Supplier).
*                Creating MIGO

                DATA(MIGOcid) = getCID(  ).
                MODIFY ENTITIES OF i_materialdocumenttp
                ENTITY materialdocument
                CREATE FROM VALUE #( (
                    %cid                          =  MIGOcid
                    postingdate                   =  cl_abap_context_info=>get_system_date(  )
                    documentdate                  =  cl_abap_context_info=>get_system_date(  )
                     GoodsMovementCode             =  '01'
*                    ReferenceDocument             =  GRNDetails-po_no
                    %control = VALUE #(
                        postingdate                         = cl_abap_behv=>flag_changed
                        documentdate                        = cl_abap_behv=>flag_changed
                        ReferenceDocument                   = cl_abap_behv=>flag_changed
                        GoodsMovementCode                   = cl_abap_behv=>flag_changed
                        )
                    ) )
                    CREATE BY \_materialdocumentitem
                    FROM VALUE #( (
                            %cid_ref = MIGOcid
                            %target = VALUE #( FOR po_line IN GRN_lines INDEX INTO i (
                                %cid =  |{ MIGOcid }{ i WIDTH = 3 ALIGN = RIGHT PAD = '0' }|
                                 plant                              =  GRNDetails-cust_code
                                 Material                           =  po_line-idprdasgncode
                                 goodsmovementtype                  =  '101'
                                 storagelocation                    =  'FG01'
                                 PurchaseOrder                      =  GRNDetails-po_no
                                 GoodsMovementRefDocType            =  'B'
                                 PurchaseOrderItem                  =  po_line-PurchaseOrderItem
                                 Supplier                           =  Supplier
                                 Batch                              =  po_line-idprdbatch
                                 Quantityinentryunit                =  po_line-idprdqty
                                 entryunit                          =  po_line-PurchaseOrderQuantityUnit
                                 %control = VALUE #(
                                        plant                   = cl_abap_behv=>flag_changed
                                        Material                = cl_abap_behv=>flag_changed
                                        storagelocation         = cl_abap_behv=>flag_changed
                                        GoodsMovementType       = cl_abap_behv=>flag_changed
                                        PurchaseOrder           = cl_abap_behv=>flag_changed
                                        purchaseorderitem       = cl_abap_behv=>flag_changed
                                        Supplier                = cl_abap_behv=>flag_changed
                                        Quantityinentryunit     = cl_abap_behv=>flag_changed
                                        EntryUnit               = cl_abap_behv=>flag_changed
                                        GoodsMovementRefDocType = cl_abap_behv=>flag_changed
                                )
                            ) )
                         ) )
                    MAPPED   DATA(ls_create_mappedi2)
                    FAILED   DATA(ls_create_failedi2)
                    REPORTED DATA(ls_create_reportedi2).

                COMMIT ENTITIES BEGIN
                  RESPONSE OF i_materialdocumenttp
                  FAILED DATA(commit_failedi2)
                  REPORTED DATA(commit_reportedi2).

                    IF lines( ls_create_mappedi2-materialdocument ) > 0.
                        LOOP AT ls_create_mappedi2-materialdocument ASSIGNING FIELD-SYMBOL(<fs_migo>).
                          CONVERT KEY OF i_materialdocumenttp FROM <fs_migo>-%pid TO <fs_migo>-%key.
                          DATA(migo_no) = <fs_migo>-%key-MaterialDocument.
                        ENDLOOP.
                    ENDIF.

                COMMIT ENTITIES END.

                IF commit_failedi2 is INITIAL.
                    UPDATE zinv_mst SET migo_processed = 1, migo_no = @migo_no WHERE imno = @GRNDetails-imno.
                ENDIF.


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
