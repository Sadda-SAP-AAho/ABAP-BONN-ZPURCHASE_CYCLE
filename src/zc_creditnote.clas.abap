CLASS zc_creditnote DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .

    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS runJobBillOfExchangePayment.
    CLASS-METHODS runJobCreditNote.
    CLASS-METHODS runJobGSTReversal.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZC_CREDITNOTE IMPLEMENTATION.


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
        runJobGSTReversal(  ).
    ENDMETHOD.


    METHOD if_oo_adt_classrun~main .
        runJobGSTReversal(  ).
    ENDMETHOD.


     METHOD runJobBillOfExchangePayment.
       DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
        lv_cid TYPE abp_behv_cid.

        TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
        CATCH cx_uuid_error.
        ASSERT 1 = 0.
        ENDTRY.

        APPEND INITIAL LINE TO lt_je_deep ASSIGNING FIELD-SYMBOL(<je_deep>).
        <je_deep>-%cid = lv_cid.
        <je_deep>-%param = VALUE #(
        companycode = 'BNPL'
        businesstransactiontype = 'RFBU'
        accountingdocumenttype = 'DZ'
        CreatedByUser = SY-uname
        documentdate = cl_abap_context_info=>get_system_date( )
        postingdate = cl_abap_context_info=>get_system_date( )
        accountingdocumentheadertext = 'RAP rules'

        _aritems = VALUE #( ( glaccountlineitem = |001|
                              glaccount = '12100000'
                                Customer = '11001112'
                                BusinessPlace = 'BN02'
                              _currencyamount = VALUE #( (
                                                currencyrole = '00'
                                                journalentryitemamount = '-10000'
                                                currency = 'INR' ) ) )
                           )
        _glitems = VALUE #(
                            ( glaccountlineitem = |002|
                            glaccount = '11031203'
                            HouseBank = 'HDF02'
                            HouseBankAccount = '5838'
                              ProfitCenter = 'BN02LDHHO'
                            _currencyamount = VALUE #( (
                                                currencyrole = '00'
                                                journalentryitemamount = '10000'
                                                currency = 'INR' ) ) ) )
        ).

        MODIFY ENTITIES OF i_journalentrytp PRIVILEGED
        ENTITY journalentry
        EXECUTE post FROM lt_je_deep
        FAILED DATA(ls_failed_deep)
        REPORTED DATA(ls_reported_deep)
        MAPPED DATA(ls_mapped_deep).

        IF ls_failed_deep IS NOT INITIAL.

        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
         DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
        ...
        ENDLOOP.
        ELSE.

        COMMIT ENTITIES BEGIN
        RESPONSE OF i_journalentrytp
        FAILED DATA(lt_commit_failed)
        REPORTED DATA(lt_commit_reported).
        ...
        COMMIT ENTITIES END.
        ENDIF.

    ENDMETHOD.


    METHOD runJobCreditNote.
       DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
        lv_cid TYPE abp_behv_cid.

        TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
        CATCH cx_uuid_error.
        ASSERT 1 = 0.
        ENDTRY.

        APPEND INITIAL LINE TO lt_je_deep ASSIGNING FIELD-SYMBOL(<je_deep>).
        <je_deep>-%cid = lv_cid.
        <je_deep>-%param = VALUE #(
        companycode = 'BNPL'
        businesstransactiontype = 'RFBU'
        accountingdocumenttype = 'DG'
        CreatedByUser = SY-uname
        documentdate = cl_abap_context_info=>get_system_date( )
        postingdate = cl_abap_context_info=>get_system_date( )
        accountingdocumentheadertext = 'RAP rules'
        _aritems = VALUE #( ( glaccountlineitem = |001|
                              glaccount = '12213000'
                                Customer = '11000628'
                                BusinessPlace = 'BN02'
                              _currencyamount = VALUE #( (
                                                currencyrole = '00'
                                                journalentryitemamount = '-10000'
                                                currency = 'INR' ) ) )
                           )
        _glitems = VALUE #(
                            ( glaccountlineitem = |002|
                            glaccount = '65900000'
                              CostCenter = 'BN02LDHO13'
                            _currencyamount = VALUE #( (
                                                currencyrole = '00'
                                                journalentryitemamount = '10000'
                                                currency = 'INR' ) ) ) )
        ).

        MODIFY ENTITIES OF i_journalentrytp PRIVILEGED
        ENTITY journalentry
        EXECUTE post FROM lt_je_deep
        FAILED DATA(ls_failed_deep)
        REPORTED DATA(ls_reported_deep)
        MAPPED DATA(ls_mapped_deep).

        IF ls_failed_deep IS NOT INITIAL.

        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
         DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
        ...
        ENDLOOP.
        ELSE.

        COMMIT ENTITIES BEGIN
        RESPONSE OF i_journalentrytp
        FAILED DATA(lt_commit_failed)
        REPORTED DATA(lt_commit_reported).
        ...
        COMMIT ENTITIES END.
        ENDIF.

    ENDMETHOD.


    METHOD runJobGSTReversal.
       DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
        lv_cid TYPE abp_behv_cid.

        TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
        CATCH cx_uuid_error.
        ASSERT 1 = 0.
        ENDTRY.

        APPEND INITIAL LINE TO lt_je_deep ASSIGNING FIELD-SYMBOL(<je_deep>).
        <je_deep>-%cid = lv_cid.
        <je_deep>-%param = VALUE #(
        companycode = 'BBPL'
        businesstransactiontype = 'RFBU'
        accountingdocumenttype = 'SA'
        CreatedByUser = SY-uname
        documentdate = cl_abap_context_info=>get_system_date( )
        postingdate = cl_abap_context_info=>get_system_date( )
        accountingdocumentheadertext = 'RAP rules'



        _glitems = VALUE #(
                            ( glaccountlineitem = |001|
                            glaccount = '80001060'
                            BusinessPlace = 'BB02'
                            CostCenter = 'BB01COR03'
                            _currencyamount = VALUE #( (
                                                currencyrole = '00'
                                                journalentryitemamount = 110
                                                currency = 'INR' ) ) )

                            ( glaccountlineitem = |002|
                            glaccount = '12605900'
                            _currencyamount = VALUE #( (
                                                currencyrole = '00'
                                                journalentryitemamount = -110
                                                currency = 'INR' ) ) ) )


*                            ( glaccountlineitem = |003|
*                            glaccount = '12605902'
*                            _currencyamount = VALUE #( (
*                                                currencyrole = '00'
*                                                journalentryitemamount = -55
*                                                currency = 'INR' ) ) ) )


        ).

        MODIFY ENTITIES OF i_journalentrytp PRIVILEGED
        ENTITY journalentry
        EXECUTE post FROM lt_je_deep
        FAILED DATA(ls_failed_deep)
        REPORTED DATA(ls_reported_deep)
        MAPPED DATA(ls_mapped_deep).

        IF ls_failed_deep IS NOT INITIAL.

        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
         DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
        ...
        ENDLOOP.
        ELSE.

        COMMIT ENTITIES BEGIN
        RESPONSE OF i_journalentrytp
        FAILED DATA(lt_commit_failed)
        REPORTED DATA(lt_commit_reported).
        ...
        COMMIT ENTITIES END.
        ENDIF.

    ENDMETHOD.
ENDCLASS.
