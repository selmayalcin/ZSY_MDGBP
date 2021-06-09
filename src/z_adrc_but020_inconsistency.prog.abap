*&---------------------------------------------------------------------*
*& Report  Z_ADRC_BUT020_INCONSISTENCY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_ADRC_BUT020_INCONSISTENCY.

Tables : but000.

SELECTION-SCREEN
          BEGIN OF BLOCK b_partners
          WITH FRAME TITLE b_text1.

SELECT-OPTIONS: BP_Num FOR but000-partner.

SELECTION-SCREEN
          END OF BLOCK b_partners.

SELECTION-SCREEN

          BEGIN OF BLOCK b_testrun
          WITH FRAME TITLE b_text3.

 PARAMETERS: testrun TYPE c AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN
          END OF BLOCK b_testrun.


 DATA: lt_but020 TYPE TABLE OF but020,
       ls_ADRC TYPE ADRC,
       lt_but020_del TYPE TABLE OF but020,
       lt_ADRC TYPE TABLE OF  ADRC,
       wa TYPE but020.

 SELECT * FROM but020 INTO TABLE lt_but020 where partner in BP_Num.

 SELECT * FROM ADRC INTO TABLE lt_ADRC for ALL ENTRIES IN lt_but020 where addrnumber = lt_but020-addrnumber.
sort lt_adrc by addrnumber.
 WRITE : / 'Inconsistent partners are listed below'.
 WRITE : /, /  'BP Number', 'Address Number'.
     loop at lt_but020 into wa.
       READ TABLE lt_ADRC INTO ls_ADRC
          WITH KEY addrnumber = wa-addrnumber BINARY SEARCH.

   IF sy-subrc <> 0.
     APPEND wa to lt_but020_del.
     WRITE: / wa-partner, wa-addrnumber.
   ENDIF.
   ENDLOOP.

   CLEAR: wa.

    IF  testrun <> 'X'.
       DELETE but020 from table lt_but020_del.
      WRITE: /, / ' THE DATABASE IS UPDATED'.
    ELSE.
      WRITE: /, / ' THE DATABASE IS NOT UPDATED'.
    ENDIF.
