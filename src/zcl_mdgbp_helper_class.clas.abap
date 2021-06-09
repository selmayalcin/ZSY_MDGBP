class ZCL_MDGBP_HELPER_CLASS definition
  public
  final
  create public .

public section.

  class-methods CREATE_CREQUEST
    importing
      !IV_CREQUEST_TYPE type USMD_CREQUEST_TYPE
      !IV_DESCRIPTION type USMD_TXTLG
      !IV_PARTNER type PARTNER
    exporting
      !ET_MESSAGES type USMD_T_MESSAGE
      !EV_CREQUEST_ID type USMD_CREQUEST .
  class-methods REJECT_CREQUEST
    importing
      !IV_CREQUEST type USMD_CREQUEST
      !IV_NOTE type USMD_NOTE
    exporting
      !ET_MESSAGE type USMD_T_MESSAGE .
  class-methods CANCEL_WI_AGENT_CHANGE
    importing
      !IV_CREQUEST type USMD_CREQUEST
      !IV_UNAME type SY-UNAME
    exporting
      !ET_MESSAGE type USMD_T_MESSAGE .
  class-methods READ_CREQUEST_BY_BP_NUMBER
    importing
      !IV_BP_NUMBER type BU_PARTNER
    exporting
      !ET_CREQUEST_BY_MATNR type USMD_CR_TS_ROOT_NODE_ID .
  class-methods CHECK_ENTITY_LOCKED
    importing
      !IV_BPHEADER type BU_BUSINESSPARTNER
    exporting
      !ET_MESSAGE type USMD_T_MESSAGE .
  class-methods GET_BLOCK
    importing
      !IV_PARTNER type PARTNER optional
      !IV_LIFNR type LIFNR optional
      !IV_KUNNR type KUNNR optional
    exporting
      !ET_MAIL_LIST type STRING_T .
  class-methods TAXNUM_MEMMORY_ID
    importing
      !IV_PARTNER type PARTNER .
  class-methods CHECK_CHANGED_CR_CONTENT
    importing
      !IV_CREQUEST type USMD_CREQUEST
    exporting
      !ET_MESSAGE type USMD_T_MESSAGE
      !EV_RESULT type BOOLEAN .
  class-methods CR_CHANGE_DOC .
  class-methods GET_EMAIL_FOR_USER_ID
    importing
      !IV_USER type BAPIBNAME-BAPIBNAME
    returning
      value(RV_EMAIL) type AD_SMTPADR .
  class-methods GET_LAST_CR_EDITOR_BY_CR_NUM
    importing
      !IV_USMD_CREQUEST type USMD_CREQUEST
    returning
      value(RV_LAST_CR_EDITOR) type SWW_AAGENT .
  class-methods GET_LINK_FOR_CR
    importing
      !IV_CR_NUMBER type USMD_CREQUEST
    returning
      value(RV_CR_LINK) type STRING .
  class-methods GET_ACCOUNT_GROUPS_BY_CR_NUM
    importing
      !IV_USMD_CREQUEST type USMD_CREQUEST
      !I_READMODE type USMD_READMODE_EXT
    exporting
      !ET_SUPPLIIER_ACCOUNT_GROUP type ZMDGA_BP_WF_T_VEN_ACC_GRP
      !ET_CUSTOMER_ACCOUNT_GROUP type ZMDGA_BP_WF_S_CUS_ACC_GRP3 .
  class-methods GET_MAIL_RECIPIENTS
    changing
      !RT_EMAILS type STRING_TABLE .
  class-methods GET_USERS_FOR_ROLE
    importing
      !IV_ROLE type AGR_NAME
      !IV_USER_EXCLUDE type XUBNAME
      !IV_USER_EXCLUDE2 type XUBNAME
    returning
      value(RT_USERS) type USMD_T_USER_AGENT .
  class-methods IS_SEGREGATION_NOT_WANTED4USER
    importing
      !IV_USER type XUBNAME
    returning
      value(RV_IS_SEGREGATION_NOT_WANTED) type BOOLEAN .
  class-methods GET_BP_TYPE
    importing
      !IV_CR type USMD_CREQUEST
    exporting
      !EV_BP_TYPE type BU_TYPE .
  PROTECTED SECTION.
private section.

  constants GC_FIELDNAME_BP_HEADER type USMD_FIELDNAME value 'BP_HEADER' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_MDGBP_HELPER_CLASS IMPLEMENTATION.


  METHOD cancel_wi_agent_change.
***************************************************************************
*   This method changes the work item agent to the current user.
*   That is mandatory if a cancel process is triggered, e.g.
*   from PTC which uses the interface user to reject/cancel the change request.
*   -> PLM2-1392
*   ES
***************************************************************************

*  Local variables
    DATA lt_obj     TYPE tswhactor.
    DATA ls_obj     TYPE swhactor.
    DATA ls_message TYPE usmd_s_message.


    CLEAR: et_message, ls_message.

    IF iv_crequest IS INITIAL.
      ls_message-msgty = 'E'.
      ls_message-msgv1 = 'Change request missing'.
      APPEND ls_message TO et_message.
      RETURN.
    ENDIF.

*   Take current user
    ls_obj-otype = 'US'.
    ls_obj-objid = iv_uname.
    APPEND ls_obj TO lt_obj.

*  Get all dialog work items of the change request
    CALL METHOD cl_usmd_wf_service=>get_cr_wis
      EXPORTING
        id_crequest = iv_crequest
*       it_top_wiid =
*       iv_translate_wi_text = ABAP_FALSE
      IMPORTING
        et_workitem = DATA(lt_workitem).

    LOOP AT lt_workitem ASSIGNING FIELD-SYMBOL(<wi>).
*     Change the agents of the work item with interface user: new entry replaces old entries in SWWUSERWI
      CALL FUNCTION 'SWW_WI_AGENTS_CHANGE'
        EXPORTING
          wi_id            = <wi>-wi_id
*         DO_COMMIT        = 'X'
*         AUTHORIZATION_CHECKED            = ' '
        TABLES
          agents           = lt_obj
        EXCEPTIONS
          no_authorization = 1
          update_failed    = 2
          invalid_type     = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
*     Implement suitable error handling here
        ls_message-msgty = 'E'.
        ls_message-msgv1 = 'Work item agent cannot be changed'.
        APPEND ls_message TO et_message.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_changed_cr_content.


    DATA lt_entity_fieldname  TYPE usmd_ts_entity_fieldname.
    DATA ls_message           TYPE LINE OF usmd_t_message.
    DATA lo_change_document   TYPE REF TO cl_usmd_mdf_change_document.
    DATA lo_if_model TYPE REF TO if_usmd_model.
    DATA lt_message TYPE usmd_t_message.
    DATA lt_crequest           TYPE cl_usmd_mdf_change_document=>t_change_request.
    DATA lo_usmd_chg_doc TYPE REF TO cl_usmd_mdf_change_document.
    DATA lc_model TYPE usmd_model VALUE 'BP'.
    DATA lt_document_header   TYPE cl_usmd_mdf_change_document=>t_document_header.
    DATA lo_model             TYPE REF TO if_usmd_model.
    DATA lv_cd_found          TYPE boolean VALUE abap_false.
    DATA lt_doc_hdr TYPE cl_usmd_mdf_change_document=>t_document_header.
    DATA lt_cg_doc_val TYPE usmd_ts_change_document_value.
    DATA lt_cg_doc_chng TYPE usmd_th_change_document_change.
    DATA lt_cg_doc_val_comp TYPE usmd_ts_change_document_value.
    DATA ls_doc_hdr TYPE cl_usmd_mdf_change_document=>s_document_header.
    DATA lv_change_found      TYPE boolean VALUE abap_false.

    FIELD-SYMBOLS <ls_document_header>   TYPE LINE OF cl_usmd_mdf_change_document=>t_document_header.


    APPEND iv_crequest TO lt_crequest.

    CALL METHOD cl_usmd_mdf_change_document=>get_instance
      RECEIVING
        eo_instance = lo_usmd_chg_doc.


    CALL METHOD cl_usmd_model=>get_instance
      EXPORTING
        i_usmd_model = lc_model
      IMPORTING
        eo_instance  = lo_if_model
        et_message   = lt_message.


    CALL METHOD lo_usmd_chg_doc->read_document_header
      EXPORTING
        it_crequest        = lt_crequest
        io_model           = lo_if_model
      IMPORTING
        et_document_header = lt_doc_hdr.


    LOOP AT lt_doc_hdr INTO ls_doc_hdr.
      CALL METHOD lo_usmd_chg_doc->read_document_lines
        EXPORTING
          is_document_header = ls_doc_hdr
          io_model           = lo_if_model
        IMPORTING
          et_changed_value   = lt_cg_doc_val
          et_changed_detail  = lt_cg_doc_chng
          et_compound_value  = lt_cg_doc_val_comp
          et_message         = lt_message.
    ENDLOOP.

    READ TABLE lt_cg_doc_chng  INTO DATA(lt_tax) WITH KEY fieldname = 'TAXNUM'.
    LOOP AT lt_cg_doc_chng ASSIGNING FIELD-SYMBOL(<ls_cg_doc_chng>).
      IF <ls_cg_doc_chng>-value_old IS NOT INITIAL.
        ev_result = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_entity_locked.
**************************************************************************************************************************
*   This method checks whether a given entity can be maintained. This is the case if the entity is not assigned to a CR,
*   i.e. ET_MESSAGE has to be empty.
*   PLM2-1639
*   ES
*
*   June 10, 2019
*   Change request type information added to the ET_MESSAGE output.
*   ES
**************************************************************************************************************************


    DATA: lo_gov_api                TYPE REF TO if_usmd_gov_api,
          lo_sp_api                 TYPE REF TO if_usmd_cr_sp_utilities,
          lt_message_obj            TYPE usmd_cr_t_root_message,
          ls_message_obj            TYPE usmd_cr_s_root_message,
          lo_message_entity_locked  TYPE REF TO cm_usmd_cr_root_entity_in_cr,
          lv_msg_crequest           TYPE usmd_crequest,
          lv_cr_type                TYPE usmd_crequest_type,
          lo_message_entity_blocked TYPE REF TO cm_usmd_cr_root_blocklist,
          ls_message                TYPE usmd_s_message.

*   1 - create an instance of the governance api
    TRY.
        lo_gov_api = cl_usmd_gov_api=>get_instance( iv_model_name = 'BP' ).
        lo_gov_api->refresh_buffers( ).
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api.
        et_message = lo_gov_api->get_messages( ).
        RETURN.
    ENDTRY.

*   CR service provider utility
    lo_sp_api = cl_usmd_cr_factory=>get_cr_utilities( ).

*   2 - Get reference of entity BP_HEADER and check whether it is maintainable
    TRY.

        lo_gov_api->create_data_reference( EXPORTING iv_entity_name = 'BP_HEADER'
                                                         iv_struct     = lo_gov_api->gc_struct_key
                                               IMPORTING er_structure  = DATA(lr_bp_key_str)
                                                         er_table      = DATA(lr_bp_key_tab) ).

        ASSIGN lr_bp_key_str->* TO FIELD-SYMBOL(<ls_bp_key>).
        FIELD-SYMBOLS <lt_bp_key> TYPE ANY TABLE.
        ASSIGN lr_bp_key_tab->* TO <lt_bp_key>.
        ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_bp_key> TO FIELD-SYMBOL(<value>).

        <value> = iv_bpheader.
        INSERT <ls_bp_key> INTO TABLE <lt_bp_key>.

*    Is entity maintainable?
        lo_sp_api->check_entity_maintainable(
                    EXPORTING
                      iv_entity_type            = 'BP_HEADER'
                      is_entity_key             = <ls_bp_key>
                      iv_model                  = 'BP'
*           iv_ignore_obj_list_buffer = ABAP_TRUE
                    RECEIVING
                      rt_message                = lt_message_obj ).

        LOOP AT lt_message_obj INTO ls_message_obj.

          TRY. "first check change list lock
              lo_message_entity_locked ?= ls_message_obj-message.
              lv_msg_crequest = lo_message_entity_locked->crequest_id.
              lv_cr_type = cl_usmd_crequest_util=>get_cr_type_by_cr( i_crequest = lv_msg_crequest ).

            CATCH cx_sy_move_cast_error.
              "Then try blocklist block
              TRY .
                  lo_message_entity_blocked ?= ls_message_obj-message.
                  lv_msg_crequest = lo_message_entity_blocked->crequest_id.
                CATCH cx_sy_move_cast_error.            "#EC NO_HANDLER
              ENDTRY.

          ENDTRY.

          ls_message = ls_message_obj-message->get_usmd_message( ).
          ls_message-msgv4 = lv_cr_type.
          INSERT ls_message INTO TABLE et_message.

        ENDLOOP.

      CATCH cx_usmd_cr_root_no_model       ##NO_HANDLER.
      CATCH cx_usmd_cr_root_bl_pp_error    ##NO_HANDLER.
      CATCH cx_usmd_cr_root_no_model_insta ##NO_HANDLER.
      CATCH cx_usmd_cr_root_bad_entity_typ ##NO_HANDLER.
      CATCH cx_usmd_cr_root_emp_entity_key ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD create_crequest.

    DATA:
      ls_entity      TYPE usmd_gov_api_s_ent_tabl,
      lt_entity      TYPE usmd_gov_api_ts_ent_tabl,
      lv_description TYPE usmd_txtlg.
    FIELD-SYMBOLS:
      <ls_bp_key> TYPE any,
      <lv_value>  TYPE any,
      <lt_bp_key> TYPE ANY TABLE.
* Create an instance of the governance api
    TRY.
        DATA(lo_gov_api) = cl_usmd_gov_api=>get_instance( iv_model_name = if_mdg_bp_constants=>gc_bp_model ).
        lo_gov_api->refresh_buffers( ).
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api.
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_messages.
        RETURN.
    ENDTRY.
*        Create all the data references needed to maintain the bp  entity
*  Create a data reference of the key structure
    TRY .
        lo_gov_api->create_data_reference( EXPORTING iv_entity_name = if_mdg_bp_constants=>gc_bp_model_entity-bp  "
                                                    iv_struct      = lo_gov_api->gc_struct_key
                                           IMPORTING er_structure   = DATA(lr_bp_key_str)
                                                     er_table       = DATA(lr_bp_key_tab) ).
*     Assign the created data references for partner key to field symbols
        ASSIGN lr_bp_key_str->*  TO <ls_bp_key>.
        ASSIGN lr_bp_key_tab->*  TO <lt_bp_key>.
        ASSIGN COMPONENT if_mdg_bp_constants=>gc_bp_model_entity-bp OF STRUCTURE <ls_bp_key> TO <lv_value>.
        IF sy-subrc EQ 0. <lv_value> = iv_partner. ENDIF.
        INSERT <ls_bp_key> INTO TABLE <lt_bp_key>.
      CATCH cx_usmd_gov_api_entity_lock cx_usmd_gov_api cx_usmd_cr_root_no_cr_typ_4_cr.
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_messages.
        RETURN.
    ENDTRY.

*       Create & Enqueue new change request/entity using change request type and a description (required)
    TRY .
        DATA(lv_new_crequest_id) = lo_gov_api->create_crequest( EXPORTING iv_crequest_type = iv_crequest_type iv_description = iv_description  ).
        lo_gov_api->enqueue_crequest( EXPORTING iv_crequest_id = lv_new_crequest_id ).
        lo_gov_api->enqueue_entity( EXPORTING iv_crequest_id = lv_new_crequest_id
                                           iv_entity_name = if_mdg_bp_constants=>gc_bp_model_entity-bp
                                              it_data        =  <lt_bp_key> ).
        ev_crequest_id = lv_new_crequest_id.
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api.
        lo_gov_api->refresh_buffers( ).
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_messages.
        RETURN.
    ENDTRY.

*         Write the changes for the partner
    TRY .
        lo_gov_api->write_entity( EXPORTING iv_crequest_id = lv_new_crequest_id
                                            iv_entity_name = if_mdg_bp_constants=>gc_bp_model_entity-bp
                                             it_data        = <lt_bp_key> ).
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api_entity_lock cx_usmd_gov_api_entity_write cx_usmd_gov_api.




        lo_gov_api->refresh_buffers( ).

        APPEND LINES OF lo_gov_api->get_messages( ) TO et_messages.
        RETURN.
    ENDTRY.

*       The complete change request should be checked before it is saved
    TRY.
        lo_gov_api->check_crequest_data( iv_crequest_id = lv_new_crequest_id ).
        "Collect the entities to be checked
        ls_entity-entity = if_mdg_bp_constants=>gc_bp_model_entity-bp.
        ls_entity-tabl   = lr_bp_key_tab.
        INSERT ls_entity INTO TABLE lt_entity.

      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api.
        lo_gov_api->refresh_buffers( ).
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_messages.
        RETURN.
    ENDTRY.

*         Save & dequeue & start the change request (and the entity DATA, of course)
    TRY.
        lo_gov_api->save( i_mode = if_usmd_ui_services=>gc_save_mode_draft_no_check ).
        " Save is done in draft mode by default so it is possible to save the change request
        " even if the change request data or the entity data is not consistent.
        lo_gov_api->dequeue_crequest( EXPORTING iv_crequest_id = lv_new_crequest_id ).
        lo_gov_api->if_usmd_gov_api_process~start_workflow( EXPORTING iv_crequest_id = lv_new_crequest_id ).
        COMMIT WORK AND WAIT.
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api.
        lo_gov_api->refresh_buffers( ).
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_messages.
        EXIT.
        "Adequate exception handling
    ENDTRY.

  ENDMETHOD.


  METHOD cr_change_doc.
    DATA : lr_model        TYPE REF TO if_usmd_model,
           lt_ent          TYPE usmd_t_value,
           ls_ent          TYPE usmd_s_value,
           lo_usmd_chg_doc TYPE REF TO cl_usmd_mdf_change_document.

    CALL METHOD cl_usmd_model=>get_instance
      EXPORTING
        i_usmd_model = 'BP'
      IMPORTING
        eo_instance  = lr_model
        et_message   = DATA(lt_msg).

    CALL METHOD cl_usmd_mdf_change_document=>get_instance
      RECEIVING
        eo_instance = lo_usmd_chg_doc.

    ls_ent-fieldname = 'BP_HEADER'.
    ls_ent-value = '23'.
    APPEND ls_ent TO lt_ent.

    CALL METHOD lo_usmd_chg_doc->read_document_header
      EXPORTING
*       IT_CREQUEST        =
        i_entity           = 'BP_HEADER'
        it_entity_value    = lt_ent
        io_model           = lr_model
      IMPORTING
        et_document_header = DATA(lt_header).
  ENDMETHOD.


  METHOD get_account_groups_by_cr_num.

    DATA: lo_model_api TYPE REF TO if_usmd_model_ext.
    DATA: lt_selection_criteria TYPE usmd_ts_sel,
          ls_selection_criteria TYPE usmd_s_sel.

    DATA: lt_creq_attribute TYPE usmd_t_crequest_entity,
          ls_creq_attribute TYPE usmd_s_crequest_entity.

    DATA: lr_bp_vengen_data TYPE REF TO data,
          lr_bp_cusgen_data TYPE REF TO data.

    DATA: ls_ven_acc_grp TYPE zmdga_bp_wf_s_ven_acc_grp,
          ls_cus_acc_grp TYPE zmdga_bp_wf_s_cus_acc_grpp.

    FIELD-SYMBOLS:
      <t_customer> TYPE SORTED TABLE, " of mdg_bs_cust_bp_cusgen  with unique key bp_header assgnm_id,
      <t_supplier> TYPE SORTED TABLE, " of mdg_bs_suppl_bp_vengen with unique key bp_header assgnm_id.
      <acc_group>  TYPE any,
      <assignm_id> TYPE any.


    cl_usmd_model_ext=>get_instance(
    EXPORTING
      i_usmd_model = 'BP'
    IMPORTING
      eo_instance = lo_model_api
    et_message  = DATA(lt_msg)
    ).

    "***************BP_HEADER*********************************************
    ls_selection_criteria-fieldname = usmd0_cs_fld-crequest.    " USMD_CREQUEST
    ls_selection_criteria-sign      = 'I'.
    ls_selection_criteria-option    = 'EQ'.
    ls_selection_criteria-low       = iv_usmd_crequest.

    INSERT ls_selection_criteria INTO TABLE lt_selection_criteria.

    CALL METHOD lo_model_api->read_char_value
      EXPORTING
        i_fieldname       = usmd0_cs_fld-crequest
        it_sel            = lt_selection_criteria
        if_edition_logic  = abap_false
        i_readmode        = lo_model_api->gc_readmode_default
        if_use_edtn_slice = abap_false
        if_no_flush       = abap_false
      IMPORTING
        et_data           = lt_creq_attribute
        et_message        = DATA(lt_msg_from_model).

    READ TABLE lt_creq_attribute
    INTO DATA(ls_creq_entity)
          WITH KEY usmd_entity = 'BP_HEADER'.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    DATA(lv_bupa_number) = ls_creq_entity-usmd_value.

    CLEAR: ls_selection_criteria, lt_selection_criteria.
    ls_selection_criteria-fieldname = 'BP_HEADER'.
    ls_selection_criteria-sign      = 'I'.
    ls_selection_criteria-option    = 'EQ'.
    ls_selection_criteria-low       = lv_bupa_number.
    INSERT ls_selection_criteria INTO TABLE lt_selection_criteria.

    "***************VENDOR*********************************************
    CALL METHOD lo_model_api->create_data_reference
      EXPORTING
        i_fieldname = 'BP_VENGEN'
        i_struct    = lo_model_api->gc_struct_key_attr
        if_table    = abap_true
        i_tabtype   = lo_model_api->gc_tabtype_sorted
      IMPORTING
        er_data     = lr_bp_vengen_data
        et_message  = DATA(lt_msg_create_data).

    ASSIGN lr_bp_vengen_data->* TO <t_supplier>.

    CALL METHOD lo_model_api->read_char_value
      EXPORTING
        i_fieldname       = 'BP_VENGEN'
        it_sel            = lt_selection_criteria
        if_edition_logic  = abap_false
        i_readmode        = i_readmode
        if_use_edtn_slice = abap_false
        if_no_flush       = abap_true
      IMPORTING
        et_data           = <t_supplier>
        et_message        = lt_msg_from_model.

    "***************CUSTOMER*********************************************
    CALL METHOD lo_model_api->create_data_reference
      EXPORTING
        i_fieldname = 'BP_CUSGEN'
        i_struct    = lo_model_api->gc_struct_key_attr
        if_table    = abap_true
        i_tabtype   = lo_model_api->gc_tabtype_sorted
      IMPORTING
        er_data     = lr_bp_cusgen_data
        et_message  = lt_msg_create_data.

    ASSIGN lr_bp_cusgen_data->* TO <t_customer>.

    CALL METHOD lo_model_api->read_char_value
      EXPORTING
        i_fieldname       = 'BP_CUSGEN'
        it_sel            = lt_selection_criteria
        if_edition_logic  = abap_false
        i_readmode        = i_readmode
        if_use_edtn_slice = abap_false
        if_no_flush       = abap_true
      IMPORTING
        et_data           = <t_customer>
        et_message        = lt_msg_from_model.

    "***************Map data to exporting parameters*********************************************
    LOOP AT <t_customer> ASSIGNING FIELD-SYMBOL(<s_customer>).
      ASSIGN COMPONENT 'KTOKD' OF STRUCTURE <s_customer> TO <acc_group>.
      IF <acc_group> IS ASSIGNED.
        ls_cus_acc_grp-ktokd = <acc_group>.
        UNASSIGN <acc_group>.
      ENDIF.
      ASSIGN COMPONENT 'ASSGNM_ID' OF STRUCTURE <s_customer> TO <assignm_id>.
      IF <assignm_id> IS ASSIGNED.
        ls_cus_acc_grp-assignm_id = <assignm_id>.
        UNASSIGN <assignm_id>.
      ENDIF.
      APPEND ls_cus_acc_grp TO et_customer_account_group.
    ENDLOOP.

    LOOP AT <t_supplier> ASSIGNING FIELD-SYMBOL(<s_supplier>).
      ASSIGN COMPONENT 'KTOKK' OF STRUCTURE <s_supplier> TO <acc_group>.
      IF <acc_group> IS ASSIGNED.
        ls_ven_acc_grp-ktokk = <acc_group>.
        UNASSIGN <acc_group>.
      ENDIF.
      ASSIGN COMPONENT 'ASSGNM_ID' OF STRUCTURE <s_supplier> TO <assignm_id>.
      IF <assignm_id> IS ASSIGNED.
        ls_ven_acc_grp-assignm_id = <assignm_id>.
        UNASSIGN <assignm_id>.
      ENDIF.
      APPEND ls_ven_acc_grp TO et_suppliier_account_group.
      CLEAR ls_ven_acc_grp.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_block.

    DATA : lv_xblck   TYPE bu_xblck,
           lv_sperb_x TYPE sperb_x,
           lv_sperb_b TYPE sperb_b.


**********************************************************************
*    IF iv_partner IS NOT INITIAL.
    SELECT SINGLE FROM but000 FIELDS xblck
     WHERE partner = @iv_partner
     INTO @lv_xblck.

    SELECT SINGLE FROM lfb1 FIELDS sperr
    WHERE lifnr = @iv_lifnr
    INTO @lv_sperb_b.
    SELECT SINGLE FROM lfa1 FIELDS sperr
     WHERE lifnr = @iv_lifnr
    INTO @lv_sperb_x.

    IF lv_xblck EQ 'X' OR  lv_sperb_x EQ 'X' OR lv_sperb_b EQ 'X'.
      APPEND 'raceparts@porsche.de' TO et_mail_list.
      APPEND 'regress@porsche.de' TO et_mail_list.
      APPEND 'umb@porsche.de' TO et_mail_list.
      APPEND 'lieferantenstamm@porsche.de' TO et_mail_list.
      APPEND 'kathrin.koerbl@porsche.de' TO et_mail_list.
    ENDIF.
*    ENDIF.
**********************************************************************
*    IF iv_lifnr IS NOT INITIAL.
*    SELECT SINGLE FROM lfa1 FIELDS sperr
*    WHERE lifnr = @iv_lifnr
*    INTO @lv_sperb_x.
*
*    IF lv_sperb_x EQ 'X'.
*      APPEND 'lieferantenstamm@porsche.de' TO et_mail_list.
*    ENDIF.
*    ENDIF.
**********************************************************************
*    IF iv_lifnr IS NOT INITIAL.
*      SELECT SINGLE FROM lfb1 FIELDS sperr
*      WHERE lifnr = @iv_lifnr
*      INTO @lv_sperb_b.

*      IF lv_sperb_b EQ 'X'.
*        APPEND 'kathrin.koerbl@porsche.de' TO et_mail_list.
*      ENDIF.
*    ENDIF.
* **********************************************************************


  ENDMETHOD.


  METHOD get_bp_type.
    DATA : lo_model     TYPE REF TO if_usmd_model_ext,
           lt_message   TYPE usmd_t_message,

           lts_entities TYPE usmd_ts_entities,
           lt_entities  TYPE usmd_t_entity,
           ls_sel       TYPE usmd_s_sel,
           lt_sel       TYPE usmd_ts_sel.

    FIELD-SYMBOLS  :
      <lt_data_ent> TYPE ANY TABLE,
      <ls_data_ent> TYPE any,
      <lv_bu_type>  TYPE any.
    "RETURS   Return intance of  ıf_usmd_model_ext

    cl_usmd_model_ext=>get_instance(
       EXPORTING
        i_usmd_model =  'BP'   " Data model
      IMPORTING
          eo_instance  =    lo_model
        et_message   =    lt_message " Messages
     ).
    CHECK lo_model IS NOT INITIAL.

    lo_model->read_entity_data_all(
       EXPORTING
          i_fieldname      =   gc_fieldname_bp_header  "
        if_active        =   abap_false
         i_crequest       =   iv_cr " Change Request
*        it_sel           =     " Sorted Table: Selection Condition (Range per Field)
            it_entity_filter =   lt_entities  " Ent.Types for Which Data Is Expected; Default: All Ent.Types
       IMPORTING
           et_message       =   lt_message   " Messages
           et_data_entity   =   DATA(lt_data_entity)  " Data for Entity Types
      ).

    READ TABLE lt_data_entity INTO DATA(ls_data_entity) WITH KEY usmd_entity = 'BP_HEADER'.

    IF sy-subrc EQ 0.

      ASSIGN ls_data_entity-r_t_data->* TO <lt_data_ent>.

      CHECK <lt_data_ent> IS ASSIGNED.

      LOOP AT <lt_data_ent> ASSIGNING <ls_data_ent>.

        ASSIGN COMPONENT 'BU_TYPE' OF STRUCTURE <ls_data_ent> TO <lv_bu_type>.
        IF sy-subrc = 0.
          ev_bp_type = <lv_bu_type>.
        ENDIF.

      ENDLOOP.

    ENDIF.
*    ENDMETHOD.
  ENDMETHOD.


  METHOD get_email_for_user_id.
    DATA: lt_return  TYPE bapiret2_t,
          lt_addsmtp TYPE bapiadsmtp_t,
          ls_addsmtp LIKE LINE OF lt_addsmtp.


    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      TABLES
        return   = lt_return
        addsmtp  = lt_addsmtp.

    READ TABLE lt_addsmtp INTO ls_addsmtp INDEX 1.
    IF sy-subrc = 0.
      rv_email = ls_addsmtp-e_mail.
    ENDIF.

  ENDMETHOD.


  METHOD get_last_cr_editor_by_cr_num.
    DATA: lt_work_items              TYPE STANDARD TABLE OF swr_wihdr,
          ls_work_item_with_end_date TYPE swwwiret,
          lt_work_item_with_end_date TYPE TABLE OF swwwiret,
          l_ret_code                 LIKE sy-subrc.

    CALL METHOD cl_usmd_wf_service=>get_cr_top_wf
      EXPORTING
        id_crequest = iv_usmd_crequest
*       id_selection_variant = 0001
      IMPORTING
        et_workflow = DATA(lt_workflow)
        ed_rtcode   = l_ret_code.

    CHECK l_ret_code = 0.

    DATA(l_top_work_item) = cl_usmd_wf_crequest_mapper=>get_top_wi_by_crequest_single( iv_crequest       = iv_usmd_crequest
                                                                                        if_refresh_buffer = abap_true ).
    READ TABLE lt_workflow WITH KEY wi_id = l_top_work_item ASSIGNING FIELD-SYMBOL(<s_workflow>).
    CHECK sy-subrc = 0.

    IF    <s_workflow>-wi_stat NE swfco_wi_status_completed
      AND <s_workflow>-wi_stat NE swfco_wi_status_cancelled.
      " Der Workflow ist noch in Arbeit
      " --> Wir wollen nur die beendeten Workitems lesen
      CALL FUNCTION 'SAP_WAPI_GET_DEPENDENT_WIS'
        EXPORTING
          workitem_id            = <s_workflow>-wi_id
          direct_dependants_only = ''
        IMPORTING
          return_code            = l_ret_code
        TABLES
          items                  = lt_work_items.

      CHECK l_ret_code = 0.
    ENDIF.

    " Jetzt zu den Workitems mit dem WI_TYPE = swfco_wi_normal (Dialogbearbeitung) noch den Zeitstempel der Beendigung ermitteln.
    LOOP AT lt_work_items ASSIGNING FIELD-SYMBOL(<s_workitem>)
      WHERE wi_type = swfco_wi_normal
      AND   wi_stat = swfco_wi_status_completed
      AND   wi_aagent IS NOT INITIAL.

      CALL FUNCTION 'SWW_WI_RETURN_READ'
        EXPORTING
          wi_id       = <s_workitem>-wi_id
          wi_status   = swfco_wi_status_completed
        IMPORTING
          wi_return   = ls_work_item_with_end_date
        EXCEPTIONS
          read_failed = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        " Kein Workitemin SWWWIRET persistiert!!! Was ist da los?
        " Implement suitable error handling here
        ASSERT 1 = 0.
      ENDIF.

      APPEND ls_work_item_with_end_date TO lt_work_item_with_end_date.

    ENDLOOP.

    " Jetzt noch nach dem Zeitstempel die Workitems absteigend sortieren:
    SORT lt_work_item_with_end_date BY wi_aets DESCENDING.

    " Den obersten Eintrag ermitteln --> das aktuellste Workitem
    READ TABLE lt_work_item_with_end_date INDEX 1 ASSIGNING FIELD-SYMBOL(<s_latest_work_item>).
    CHECK sy-subrc EQ 0.

    " Jetzt noch den Bearbeiter für das jüngst beendete Workitem ermitteln:
    READ TABLE lt_work_items WITH KEY wi_id = <s_latest_work_item>-wi_id ASSIGNING FIELD-SYMBOL(<s_user_work_item>).
    CHECK sy-subrc EQ 0.

    rv_last_cr_editor = <s_user_work_item>-wi_aagent.

  ENDMETHOD.


  METHOD get_link_for_cr.

    DATA: lo_navigation_handler       TYPE REF TO if_usmd_wf_navigation_handler,
          ls_navigation_parameters    TYPE usmd_s_wf_obn_params,
          ls_navigation_parameter     TYPE usmd_s_namevalue,
          lv_application_id           TYPE string,
          lv_action                   TYPE usmd_action,
          key_value_pairs_for_mapping TYPE usmd_t_namevalue,
          url_parameters              TYPE usmd_t_value,
          lo_ui_service               TYPE REF TO if_usmd_ui_services,
          id_wi_id                    TYPE sww_wiid,
          lv_main_component           TYPE REF TO if_wd_component,
          lv_value_list               TYPE usmd_ts_seqnr_value,
          lv_url                      TYPE string,
          lv_WDY_KEY_VALUE_TABLE      TYPE wdy_key_value_table,
          lv_APB_LPD_T_PARAMS         TYPE apb_lpd_t_params,
          lt_para                     TYPE tihttpnvp,
          lv_para                     TYPE ihttpnvp.

    DATA:     fieldname_value_pair TYPE usmd_s_value.


    DATA: lv_accessibility TYPE abap_bool,
          lv_langu(2)      TYPE c,
          lo_application   TYPE REF TO if_wd_application,
          lv_shm_instance  TYPE shm_inst_name,
          lv_guid          TYPE guid_32,
          ls_rfc           TYPE rfcsi.


    CONSTANTS : lc_client   TYPE string VALUE 'SAP-CLIENT',
                lc_language TYPE string VALUE 'SAP-LANGUAGE'.

    FIELD-SYMBOLS: <key_value_pair> TYPE powl_namevalue_sty.


    lo_navigation_handler = cl_usmd_wf_navigation_handler=>get_instance(
            iv_workitem_id    = id_wi_id
            iv_crequest_id    = iv_cr_number ).


    ls_navigation_parameters = lo_navigation_handler->get_navigation_parameters( ).

    lv_application_id = ls_navigation_parameters-ui_appl.


    READ TABLE ls_navigation_parameters-parameters INTO ls_navigation_parameter
      WITH KEY key = cl_usmd_wf_navigation_handler=>gc_par_logical_action .


    IF sy-subrc = 0.
      lv_action = ls_navigation_parameter-value.
    ELSE.
      CLEAR lv_action.
    ENDIF.

    key_value_pairs_for_mapping = ls_navigation_parameters-parameters.

*--------------------------------------------------------------------*

    LOOP AT key_value_pairs_for_mapping ASSIGNING <key_value_pair>.
      fieldname_value_pair-fieldname = <key_value_pair>-key.
      fieldname_value_pair-value     = <key_value_pair>-value.
      APPEND fieldname_value_pair TO url_parameters.
    ENDLOOP.


    fieldname_value_pair-fieldname  = lc_client.
    fieldname_value_pair-value = sy-mandt.

    INSERT fieldname_value_pair INTO TABLE url_parameters .

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = sy-langu
      IMPORTING
        output = lv_langu.


    fieldname_value_pair-fieldname  = lc_language.

    fieldname_value_pair-value = lv_langu.

    INSERT fieldname_value_pair INTO TABLE url_parameters.


    LOOP AT url_parameters INTO fieldname_value_pair ##INTO_OK.
      IF fieldname_value_pair-fieldname NE 'WDAPPLICATIONID'.
        lv_para-name = fieldname_value_pair-fieldname.
        lv_para-value = fieldname_value_pair-value.
        INSERT lv_para INTO TABLE lt_para.
      ENDIF.
    ENDLOOP.

    cl_wd_utilities=>construct_wd_url(
    EXPORTING
      application_name = lv_application_id
      in_parameters    = lt_para
    IMPORTING
      out_absolute_url = lv_url ).


    IF lv_url IS NOT INITIAL.
      MOVE lv_url TO rv_cr_link.
    ELSE.
      "RAISE URL_NOT_FOUND. "TODO ?
    ENDIF.


  ENDMETHOD.


  METHOD get_mail_recipients.










  ENDMETHOD.


  METHOD get_users_for_role.

    DATA: lv_uname TYPE xubname.

    FIELD-SYMBOLS: <fs_user> LIKE LINE OF rt_users.

    SELECT uname FROM agr_users INTO lv_uname
      WHERE agr_name = iv_role
        AND from_dat <= sy-datum
        AND to_dat   >= sy-datum.

      IF lv_uname <> iv_user_exclude AND
         lv_uname <> iv_user_exclude2.
        APPEND INITIAL LINE TO rt_users ASSIGNING <fs_user>.
        <fs_user>-user_type  = 'US'.
        <fs_user>-user_value = lv_uname.
      ELSE.
        IF NOT iv_user_exclude IS INITIAL.
          IF is_segregation_not_wanted4user( EXPORTING iv_user = iv_user_exclude ) = abap_true.
            APPEND INITIAL LINE TO rt_users ASSIGNING <fs_user>.
            <fs_user>-user_type  = 'US'.
            <fs_user>-user_value = lv_uname.
          ENDIF.
        ENDIF.
        IF NOT iv_user_exclude2 IS INITIAL.
          IF is_segregation_not_wanted4user( EXPORTING iv_user = iv_user_exclude2 ) = abap_true.
            APPEND INITIAL LINE TO rt_users ASSIGNING <fs_user>.
            <fs_user>-user_type  = 'US'.
            <fs_user>-user_value = lv_uname.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDSELECT.


    SORT rt_users.
    DELETE ADJACENT DUPLICATES FROM rt_users.


  ENDMETHOD.


  METHOD is_segregation_not_wanted4user.

    DATA : lt_parameter TYPE TABLE OF bapiparam,
           is_parameter LIKE LINE OF lt_parameter,
           lt_return    TYPE TABLE OF bapiret2,
           lv_user      TYPE xubname.

    rv_is_segregation_not_wanted = abap_false.

    IF NOT iv_user IS INITIAL.
      lv_user = iv_user.
    ELSE.
      lv_user = sy-uname.
    ENDIF.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username  = lv_user
      TABLES
        parameter = lt_parameter
        return    = lt_return.

*
    READ TABLE lt_parameter WITH KEY parid = 'YMDGA_BP_WF_NO_SEGRA' INTO is_parameter.

    IF sy-subrc = 0 .
      IF NOT is_parameter-parva IS INITIAL.
        rv_is_segregation_not_wanted = abap_true.
      ENDIF.

    ENDIF.




  ENDMETHOD.


  METHOD read_crequest_by_bp_number.

    DATA lv_crequest_id TYPE usmd_crequest.
    CLEAR et_crequest_by_matnr.

    "Get Model Instance for Entity Key Structure
    cl_usmd_model=>get_instance( EXPORTING i_usmd_model = 'BP'
                                 IMPORTING eo_instance = DATA(lo_model)
                                           et_message = DATA(lt_model_message) ) ##NEEDED.
    "Set Entity MATERIAL Key Structure
    DATA(lr_s_bp_key) = lo_model->create_data_reference( i_fieldname  = 'BP_HEADER'
                                                                i_struct     = 'KEY'
                                                                if_table     = abap_false ).
    ASSIGN lr_s_bp_key->* TO FIELD-SYMBOL(<ls_bp_key>).
    "Set Material Number
    FIELD-SYMBOLS <lv_attribute> TYPE any.
    ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_bp_key> TO <lv_attribute>.
    <lv_attribute> = iv_bp_number.
    "Get Change Request Service Provider
    DATA(lo_cr_service_provider) = cl_usmd_cr_factory=>get_cr_service_provider( ).
    "Retrieve CR by Entity Key
    DATA(lt_crequest) = lo_cr_service_provider->if_usmd_cr_sp_query~retrieve_cr_by_entity_key( iv_entity_type = 'BP_HEADER'
                                                                                               is_entity_key  = <ls_bp_key>
                                                                                               iv_model       = 'BP' ).
    "Set Change Requests for each Plant
    LOOP AT lt_crequest INTO DATA(ls_crequest).
      lv_crequest_id = ls_crequest-usmd_crequest.
      SELECT SINGLE * FROM usmd120c INTO @DATA(ls_usmd120c) WHERE usmd_crequest EQ @lv_crequest_id. "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc EQ 0.
*        APPEND INITIAL LINE TO et_crequest_by_matnr ASSIGNING FIELD-SYMBOL(<ls_crequest_by_matnr>).
*        MOVE-CORRESPONDING ls_usmd120c TO <ls_crequest_by_matnr>.
        APPEND   ls_usmd120c TO et_crequest_by_matnr.
      ENDIF.
    ENDLOOP.

*--------------------------------------------------------------------*
* Material with Plant
*--------------------------------------------------------------------*

    CLEAR: lo_model, lt_model_message.

*    "Get Plant Data for Material
*    IF iv_werks IS INITIAL.
*      SELECT matnr, werks INTO TABLE @DATA(lt_marc) FROM marc WHERE matnr EQ @iv_matnr. "#EC CI_SEL_NESTED
*                                                          "#EC CI_SUBRC
*    ELSE.
*      SELECT matnr werks INTO TABLE lt_marc FROM marc WHERE matnr EQ iv_matnr AND werks EQ iv_werks. "#EC CI_SEL_NESTED
*                                                          "#EC CI_SUBRC
*    ENDIF.
*    LOOP AT lt_marc INTO DATA(ls_marc).
*
*      CLEAR: lo_cr_service_provider, lt_crequest.
*
*      "Get Model Instance for Entity Key Structure
*      cl_usmd_model=>get_instance( EXPORTING i_usmd_model = 'MM'
*                                   IMPORTING eo_instance  = lo_model
*                                             et_message   = lt_model_message ).
*      "Set Entity MARCBASIC Key Structure
*      DATA(lr_s_marcbasic_key) = lo_model->create_data_reference( i_fieldname  = 'MARCBASIC'
*                                                                  i_struct     = 'KEY'
*                                                                  if_table     = abap_false ).
*      ASSIGN lr_s_marcbasic_key->* TO FIELD-SYMBOL(<ls_marcbasic_key>).
*      "Set Material Number
*      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <ls_marcbasic_key> TO <lv_attribute>.
*      <lv_attribute> = ls_marc-matnr.
*      "Set Plant Number
*      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_marcbasic_key> TO <lv_attribute>.
*      <lv_attribute> = ls_marc-werks.
*      "Get Change Request Service Provider
*      lo_cr_service_provider = cl_usmd_cr_factory=>get_cr_service_provider( ).
*      "Retrieve CR by Entity Key
*      lt_crequest = lo_cr_service_provider->if_usmd_cr_sp_query~retrieve_cr_by_entity_key( iv_entity_type = 'MARCBASIC'
*                                                                                           is_entity_key  = <ls_marcbasic_key>
*                                                                                           iv_model       = 'MM' ).
*      "Set Change Requests for each Plant
*      LOOP AT lt_crequest INTO ls_crequest.              "#EC CI_NESTED
*        lv_crequest_id = ls_crequest-usmd_crequest.
*        SELECT SINGLE * FROM usmd120c INTO ls_usmd120c WHERE usmd_crequest EQ lv_crequest_id. "#EC CI_ALL_FIELDS_NEEDED
*        IF sy-subrc EQ 0.
*          APPEND INITIAL LINE TO et_crequest_by_matnr ASSIGNING <ls_crequest_by_matnr>.
*          <ls_crequest_by_matnr>-plant = ls_marc-werks.
*          MOVE-CORRESPONDING ls_usmd120c TO <ls_crequest_by_matnr>.
*        ENDIF.
*      ENDLOOP.
*
*    ENDLOOP.


  ENDMETHOD.


  METHOD reject_crequest.
*&----------------------------------------------------------------------------------------------*
*&  This method rolls back the CR. In case of parallel work items it checks on all the work items
*&  available for the CR. Please note that this method is connected to following other logical
*&  components:
*&  1 - BRFplus workflow tables (-> tcode: usmd_ssw_rule) and corresponding cr-types
*&  2 - Corresponding  HANDLE_PARALLEL_RESULT method to handle parallel WF merge
*&      (enhancement spot: USMD_SSW_SERVICE_PROCESSOR)
*&
*&  This logic covers also change requests that were forwarded to new processors.
*&

*&----------------------------------------------------------------------------------------------*
    DATA:
      ls_value TYPE usmd_s_value,
      lt_value TYPE usmd_ts_value,
      ls_msg   TYPE usmd_s_message.

* Work item agent change to cancel the change request
    CALL METHOD cancel_wi_agent_change
      EXPORTING
        iv_crequest = iv_crequest
        iv_uname    = sy-uname
      IMPORTING
        et_message  = et_message.

* Get Gov API instance
    TRY.
        CALL METHOD cl_usmd_gov_api=>get_instance
          EXPORTING
            iv_model_name = if_mdg_bp_constants=>gc_bp_model
*           iv_classname  = 'CL_USMD_GOV_API'
          RECEIVING
            ro_gov_api    = DATA(lo_gov_api).
      CATCH cx_usmd_gov_api .
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
        RETURN.
    ENDTRY.

* Check authorization for the rollback
*    TRY .
*        ls_value-fieldname = usmd0_cs_fld-crequest. ls_value-value = iv_crequest. APPEND ls_value TO lt_value. "#EC CI_APPEND_OK
*        lo_gov_api->mo_model->check_authority(
*        EXPORTING
*          i_fieldname = usmd0_cs_fld-crequest
*          i_actvt     = usmd0_cs_crequest_actvt-rollback
*          it_key      = lt_value
*        IMPORTING
*          et_message  = et_message ).
*      CATCH cx_usmd_gov_api .
*        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
*        RETURN.
*    ENDTRY.

* Enqueue the change request
    TRY.
        lo_gov_api->if_usmd_gov_api_cr_data~enqueue_crequest( iv_crequest_id = iv_crequest ).
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api .
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
        RETURN.
    ENDTRY.

* Add a note to the CR header
    TRY.
        lo_gov_api->if_usmd_gov_api_cr_data~write_note(
          EXPORTING
            iv_crequest_id = iv_crequest
            iv_note        = iv_note ).
      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api .
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
        RETURN.
    ENDTRY.

* Get all work item IDs for the change request.
* Please note that work items with status = 'Completed' won't be listed anymore - so,
* the CR provided must not be completed (e.g. finalized/rejected).
    TRY.
        CALL METHOD cl_usmd_wf_service=>get_wi_id_by_cr_id
          EXPORTING
            iv_cr_id = iv_crequest
*           iv_user  = SY-UNAME
          IMPORTING
            et_wi_id = DATA(lt_wi_id).
*   Check whether any work item id exists for the CR
        DESCRIBE TABLE lt_wi_id LINES DATA(lv_lines).
        IF NOT lv_lines GT 0.
          ls_msg-msgid = 'ZSY_MDGM'.
          ls_msg-msgty = 'W'.
          ls_msg-msgno = '031'.
          ls_msg-msgv1 = iv_crequest.
          APPEND ls_msg TO et_message.
          RETURN.
        ENDIF.
      CATCH cx_usmd_cust_bo_event_wf .
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
        RETURN.
    ENDTRY.

* Finalize change request and every work item id
    LOOP AT lt_wi_id INTO DATA(ls_wi_id).

      TRY.
          lo_gov_api->if_usmd_gov_api_services~get_allowed_changes(
            EXPORTING
              iv_crequest_id        = iv_crequest
              iv_wi_id              = ls_wi_id
*        is_entity_key         =
            IMPORTING
              ef_change_object_list = DATA(lf_change_object_list)
              ef_change_object      = DATA(lf_change_object)
              ef_change_notes       = DATA(lf_change_notes)
              ef_change_attachments = DATA(lf_change_attachments) ) ##NEEDED. "lg200515
        CATCH cx_usmd_gov_api_mult_witems cx_usmd_gov_api .
          APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
          RETURN.
      ENDTRY.

*   Finalize / Reject the change request
      TRY.
          lo_gov_api->if_usmd_gov_api_process~finalize_process_step(
            EXPORTING
              iv_crequest_id = iv_crequest
              iv_wi_id       = ls_wi_id
              iv_action      = cl_usmd_wf_service=>gc_action-cancel ).         .
        CATCH cx_usmd_gov_api_core_error .
          APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
          RETURN.
      ENDTRY.

    ENDLOOP.

* Save the change request
    TRY.
        lo_gov_api->if_usmd_gov_api_trans~save( if_usmd_ui_services=>gc_save_mode_crequest_reject ).
      CATCH cx_usmd_gov_api_core_error .
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
        RETURN.
    ENDTRY.

* Now...tidy up the change request and then commit
    TRY.
        lo_gov_api->if_usmd_gov_api_cr_data~dequeue_crequest( iv_crequest_id = iv_crequest ).
      CATCH cx_usmd_gov_api .
        APPEND LINES OF lo_gov_api->get_messages( ) TO et_message.
        RETURN.
    ENDTRY.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'
*    IMPORTING
*       RETURN    =
      .

  ENDMETHOD.


  METHOD taxnum_memmory_id.

    DATA : c_memory_id TYPE sstring,
           lv_taxnum   TYPE bptaxnum.

    SELECT SINGLE FROM dfkkbptaxnum FIELDS taxnum
      WHERE partner = @iv_partner
      INTO (@lv_taxnum ).

    EXPORT lv_taxnum FROM lv_taxnum TO MEMORY ID c_memory_id.





  ENDMETHOD.
ENDCLASS.
