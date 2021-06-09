class ZCL_MDG_DYN_AGNT_MANAGER definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_DYNAMIC_AGT_SELECT .
*  class-methods GET_MANAGER
*    importing
*      !IV_CREQUEST type USMD_CREQUEST
*    changing
*      !CT_AGENT_GROUP type TY_T_AGENT_GROUP .
protected section.
private section.

 types:
    BEGIN OF ty_s_agent_group,
      otype   TYPE otype,
      hrobjid TYPE hrobjid,
    END OF ty_s_agent_group .
  types:
    ty_t_agent_group TYPE STANDARD TABLE OF ty_s_agent_group
    WITH DEFAULT KEY.
ENDCLASS.



CLASS ZCL_MDG_DYN_AGNT_MANAGER IMPLEMENTATION.


  method IF_USMD_SSW_DYNAMIC_AGT_SELECT~GET_DYNAMIC_AGENTS.
  "dummmy
  endmethod.
ENDCLASS.
