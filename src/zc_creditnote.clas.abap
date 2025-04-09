CLASS zc_creditnote DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zc_creditnote IMPLEMENTATION.

      METHOD if_oo_adt_classrun~main.
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
        documentdate = sy-datlo
        postingdate = sy-datlo
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
ENDCLASS.
