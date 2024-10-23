# Vendor-Profile
Vendor Profile Integration
  METHOD if_rest_resource~get.
*&---------------------------------------------------------------------*
*& Class Name : ZCL_VENDOR_PROFILE_PROVIDER
*&---------------------------------------------------------------------*
*&Class for Extraction of Vendor Profile From SAP to Supplier Portal
*&Date : 05-05-2023.
*&---------------------------------------------------------------------*

*----------------------------Types Declaration For LFA1 Table--------------------------------
    TYPES : BEGIN OF ty_lfa1,
              lifnr TYPE lifnr,
              name1 TYPE name1_gp,
              adrnr TYPE adrnr,
              erdat TYPE erdat_rf,
              ktokk TYPE ktokk,
              telf1 TYPE telf1,
              sperq TYPE qsperrfkt,
            END OF ty_lfa1,

            BEGIN OF ty_lfa1_n,
              lifnr TYPE lifnr,
              name1 TYPE name1_gp,
              adrnr TYPE c LENGTH 90,
              ktokk TYPE ktokk,
              telf1 TYPE telf1,
              sperq TYPE qsperrfkt,
            END OF ty_lfa1_n,
*----------------------------Types Declaration For LFM1 Table--------------------------------
            BEGIN OF ty_lfm1,
              lifnr TYPE lifnr,
              ekorg TYPE ekorg,
              erdat TYPE erdat,
            END OF ty_lfm1,

*----------------------------Types Declaration For ADRC Table--------------------------------

            BEGIN OF ty_adrc,
              addrnumber TYPE ad_addrnum,
              name1      TYPE ad_name1,
              name2      TYPE ad_name2,
              name3      TYPE ad_name3,
              city1      TYPE ad_city1,
              country    TYPE land1,
              region     TYPE regio,
              post_code1 TYPE ad_pstcd1,
              house_num1 TYPE ad_hsnm1,
              street     TYPE ad_street,
              str_suppl1 TYPE ad_strspp1,
              str_suppl2 TYPE ad_strspp2,
              str_suppl3 TYPE	ad_strspp3,
              location   TYPE	ad_lctn,
            END OF ty_adrc,

*----------------------------Types Declaration For ADR6 Table--------------------------------
            BEGIN OF ty_adr6,
              addrnumber TYPE ad_addrnum,
              consnumber TYPE ad_consnum,
              smtp_addr  TYPE ad_smtpadr,
            END OF ty_adr6,

*----------------------------Types Declaration For T005T Table--------------------------------
            BEGIN OF ty_t005t,
              spras TYPE spras,
              land1 TYPE land1,
              landx TYPE landx,
            END OF ty_t005t,

*----------------------------Types Declaration For T005U Table--------------------------------
            BEGIN OF ty_t005u,
              spras TYPE spras,
              land1 TYPE land1,
              bland TYPE regio,
              bezei TYPE bezei20,
            END OF ty_t005u,

*----------------------------Types Declaration For IBPSUPPLIER Table--------------------------------
            BEGIN OF ty_ibpsupplier,
              supplier        TYPE lifnr,
              businesspartner TYPE c LENGTH 10,
            END  OF ty_ibpsupplier,

*----------------------------Types Declaration For DFKKBPTAXNUM Table--------------------------------
            BEGIN OF ty_dfkkbptaxnum,
              partner TYPE c LENGTH 10,
              taxtype TYPE bptaxtype,
              taxnum  TYPE bptaxnum,
            END OF ty_dfkkbptaxnum,

*----------------------------Types Declaration For CDHDR Table--------------------------------
            BEGIN OF ty_cdhdr,
              objectclas TYPE cdobjectcl,
              objectid   TYPE c LENGTH 90,
              udate      TYPE cddatum,
              change_ind TYPE cdchngindh,
              tcode      TYPE cdtcode,
            END OF ty_cdhdr,

            BEGIN OF ty_cdhdr1,
              objectclas TYPE cdobjectcl,
              objectid   TYPE c LENGTH 10,
              udate      TYPE cddatum,
              change_ind TYPE cdchngindh,
              tcode      TYPE cdtcode,
            END OF ty_cdhdr1,

            BEGIN OF ty_ibpsupplier1,
              supplier        TYPE lifnr,
              businesspartner TYPE c LENGTH 90,
            END  OF ty_ibpsupplier1,

*----------------------------Types Declaration For Final Internal Table--------------------------------

            BEGIN OF ty_final,
              recordtype     TYPE c LENGTH 20,         "Record Type
              vendorcategory TYPE c LENGTH 20,         "Vendor account group
              vendorcode     TYPE lfa1-lifnr,          "Vendor Number
              vendorname     TYPE lfa1-name1,          "Vendor Name
              orgdata        TYPE lfm1-ekorg,          "Purchasing organization
              emailid        TYPE adr6-smtp_addr,      "Email Address
              mobileno       TYPE lfa1-telf1,          "Telephone number
              address        TYPE string,              "Name 1
              city           TYPE adrc-city1,          "City
              country        TYPE t005t-landx50,       "Country
              state          TYPE t005u-bezei,         "Region (State)
              postalcode     TYPE adrc-post_code1,     "Postal Code
              msmenumber     TYPE dfkkbptaxnum-taxnum, "Business Partner Tax Number - MSME No
              gstno          TYPE dfkkbptaxnum-taxnum, "Business Partner Tax Number - GST No
              is_deleted     TYPE c LENGTH 3,          "Deletion Indicator for Supplier at Purchasing Level
              modifiedon     TYPE lfa1-aedat,          "Changed On
            END OF ty_final.

*----------------------------Data Declaration-------------------------------

    DATA : ls_lfa1         TYPE ty_lfa1,
           lt_lfa1         TYPE TABLE OF ty_lfa1,
           ls_lfa1_n       TYPE ty_lfa1_n,
           lt_lfa1_n       TYPE TABLE OF ty_lfa1_n,
           ls_lfm1         TYPE ty_lfm1,
           lt_lfm1         TYPE TABLE OF ty_lfm1,
           ls_adrc         TYPE ty_adrc,
           lt_adrc         TYPE TABLE OF ty_adrc,
           ls_adr6         TYPE ty_adr6,
           lt_adr6         TYPE TABLE OF ty_adr6,
           ls_t005t        TYPE ty_t005t,
           lt_t005t        TYPE TABLE OF ty_t005t,
           ls_t005u        TYPE ty_t005u,
           lt_t005u        TYPE TABLE OF ty_t005u,
           lt_ibpsupplier  TYPE TABLE OF ty_ibpsupplier,
           ls_ibpsupplier  TYPE ty_ibpsupplier,
           lt_ibpsupplier1 TYPE TABLE OF ty_ibpsupplier1,
           ls_ibpsupplier1 TYPE ty_ibpsupplier1,
           lt_dfkkbptaxnum TYPE TABLE OF ty_dfkkbptaxnum,
           ls_dfkkbptaxnum TYPE ty_dfkkbptaxnum,
           ls_cdhdr        TYPE ty_cdhdr,
           lt_cdhdr        TYPE TABLE OF ty_cdhdr,
           ls_cdhdr1       TYPE ty_cdhdr1,
           lt_cdhdr1       TYPE TABLE OF ty_cdhdr1,
           ls_cdhdr2       TYPE ty_cdhdr1,
           lt_cdhdr2       TYPE TABLE OF ty_cdhdr1,
           ls_final        TYPE ty_final,
           lt_final        TYPE TABLE OF ty_final,
           ls_json         TYPE string.
*--------------------------------------Import Paramater For Delta Load---------------------------------
    DATA(l_value) = mo_request->get_uri_query_parameter( 'AEDAT' ).


    IF l_value IS NOT INITIAL.   "Delta Load
      SELECT lifnr
             name1
             adrnr
             erdat
             ktokk
             telf1
             sperq
             FROM lfa1
             INTO TABLE lt_lfa1
             WHERE aedat = l_value.

      SELECT objectclas       "Changes Record Table
             objectid
             udate
             change_ind
             tcode
             FROM cdhdr
             INTO TABLE lt_cdhdr
             WHERE udate  = l_value
             AND ( objectclas   = 'BUPA_ADR'
             OR   objectclas   = 'ADRESSE' ).

      IF lt_cdhdr IS NOT INITIAL.

        LOOP AT lt_cdhdr INTO ls_cdhdr.
          IF ls_cdhdr-objectclas = 'ADRESSE'.
            ls_cdhdr2-objectid = ls_cdhdr-objectid+4(10).
            ls_cdhdr2-objectclas = ls_cdhdr-objectclas.
            ls_cdhdr2-change_ind = ls_cdhdr-change_ind.
            ls_cdhdr2-udate = ls_cdhdr-udate.
            APPEND ls_cdhdr2 TO lt_cdhdr2.
            CLEAR ls_cdhdr2.
            CLEAR ls_cdhdr.
          ELSE.
            ls_cdhdr1-objectid = ls_cdhdr-objectid.
            ls_cdhdr1-objectclas = ls_cdhdr-objectclas.
            ls_cdhdr1-change_ind = ls_cdhdr-change_ind.
            ls_cdhdr1-udate = ls_cdhdr-udate.
            APPEND ls_cdhdr1 TO lt_cdhdr1.
            CLEAR ls_cdhdr1.
            CLEAR ls_cdhdr.
          ENDIF.
        ENDLOOP.

        IF lt_cdhdr2 IS NOT INITIAL.
          SELECT lifnr
                 name1
                 adrnr
                 erdat
                 ktokk
                 telf1
                 sperq
                 FROM lfa1
                 APPENDING TABLE lt_lfa1
                 FOR ALL ENTRIES IN lt_cdhdr2
                 WHERE adrnr = lt_cdhdr2-objectid.


        ENDIF.
        IF lt_cdhdr1 IS NOT INITIAL.

          SELECT supplier
                 businesspartner
                 FROM ibpsupplier
                 INTO TABLE lt_ibpsupplier1
                 FOR ALL ENTRIES IN lt_cdhdr1
                 WHERE businesspartner = lt_cdhdr1-objectid.

          IF  NOT lt_ibpsupplier1  IS INITIAL.
            SELECT lifnr
                   name1
                   adrnr
                   erdat
                   ktokk
                   telf1
                   sperq
                   FROM lfa1
                   APPENDING TABLE lt_lfa1
                   FOR ALL ENTRIES IN lt_ibpsupplier1
                   WHERE lifnr = lt_ibpsupplier1-supplier. "#EC CI_NO_TRANSFORM
          ENDIF.
        ENDIF.
      ENDIF.

*-----------------------------------------Full Load Data--------------------------
    ELSEIF l_value IS INITIAL.                         "Full Load
      SELECT lifnr
             name1
             adrnr
             erdat
             ktokk
             telf1
             sperq
             FROM lfa1
             INTO TABLE
             lt_lfa1 WHERE lifnr IS NOT NULL.
    ENDIF.

    IF lt_lfa1 IS NOT INITIAL.
      SELECT lifnr                  "Organisation
             ekorg
             erdat
             FROM lfm1
             INTO TABLE lt_lfm1
             FOR ALL ENTRIES IN lt_lfa1
             WHERE lifnr = lt_lfa1-lifnr.          "#EC CI_NO_TRANSFORM
      SORT lt_lfm1 BY lifnr erdat DESCENDING.

      CHECK lt_lfa1[] IS NOT INITIAL.

      SELECT addrnumber           "Address
             name1
             name2
             name3
             city1
             country
             region
             post_code1
             house_num1
             street
             str_suppl1
             str_suppl2
             str_suppl3
             location
             FROM adrc
             INTO TABLE lt_adrc
             FOR ALL ENTRIES IN lt_lfa1
             WHERE addrnumber = lt_lfa1-adrnr.     "#EC CI_NO_TRANSFORM

      CHECK lt_lfa1[] IS NOT INITIAL.
      SELECT addrnumber            "Emailid
             consnumber
             smtp_addr
             FROM adr6
             INTO TABLE lt_adr6
             FOR ALL ENTRIES IN lt_lfa1
             WHERE addrnumber = lt_lfa1-adrnr.     "#EC CI_NO_TRANSFORM

      CHECK lt_adrc[] IS NOT INITIAL.
      SELECT spras               "Country
             land1
             landx
             FROM t005t
             INTO TABLE lt_t005t
             FOR ALL ENTRIES IN lt_adrc
             WHERE  spras     EQ sy-langu
             AND    land1     = lt_adrc-country.   "#EC CI_NO_TRANSFORM

      CHECK lt_adrc[] IS NOT INITIAL.
      SELECT spras           "Region
             land1
             bland
             bezei
             FROM t005u
             INTO TABLE lt_t005u
             FOR ALL ENTRIES IN lt_adrc
             WHERE  spras     EQ sy-langu
             AND    land1     = lt_adrc-country
             AND    bland     = lt_adrc-region.    "#EC CI_NO_TRANSFORM

      CHECK lt_lfa1[] IS NOT INITIAL.
      SELECT supplier            "Business Partner
             businesspartner
             FROM ibpsupplier
             INTO TABLE lt_ibpsupplier
             FOR ALL ENTRIES IN lt_lfa1
             WHERE supplier = lt_lfa1-lifnr.       "#EC CI_NO_TRANSFORM

      CHECK lt_ibpsupplier[] IS NOT INITIAL.
      SELECT partner
             taxtype
             taxnum
             FROM dfkkbptaxnum
             INTO TABLE lt_dfkkbptaxnum
             FOR ALL ENTRIES IN lt_ibpsupplier
             WHERE partner = lt_ibpsupplier-businesspartner
             AND ( taxtype   = 'IN0' OR taxtype = 'IN3' ). "#EC CI_NO_TRANSFORM

      MOVE lt_ibpsupplier TO lt_ibpsupplier1.

      CHECK lt_ibpsupplier1[] IS NOT INITIAL.
      SELECT objectclas
             objectid
             udate
             change_ind
             tcode
             FROM cdhdr
             INTO TABLE lt_cdhdr
             FOR ALL ENTRIES IN lt_ibpsupplier1
             WHERE objectclas   = 'BUPA_ADR'
             AND   objectid     = lt_ibpsupplier1-businesspartner. "#EC CI_NO_TRANSFORM


      CLEAR ls_lfa1_n.
      REFRESH lt_lfa1_n.

      LOOP AT lt_lfa1 INTO ls_lfa1.
        DATA(lv_adrnr) = 'BP'.
        CONCATENATE lv_adrnr ls_lfa1-adrnr INTO ls_lfa1_n-adrnr SEPARATED BY '  '.
        ls_lfa1_n-lifnr = ls_lfa1-lifnr.
        APPEND ls_lfa1_n TO lt_lfa1_n.
        CLEAR : ls_lfa1, ls_lfa1_n.
      ENDLOOP.

      SELECT objectclas
             objectid
             udate
             change_ind
             tcode
             FROM cdhdr
             APPENDING TABLE lt_cdhdr
             FOR ALL ENTRIES IN lt_lfa1_n
             WHERE objectclas   = 'ADRESSE'
             AND   objectid     = lt_lfa1_n-adrnr.
    ENDIF.
*-------------------------- Sorting The Internal Table------------------------------
    IF lt_lfa1 IS NOT INITIAL.
      SORT lt_lfa1          BY lifnr.
      SORT lt_lfm1          BY lifnr.
      SORT lt_adrc          BY addrnumber.
      SORT lt_adr6          BY addrnumber consnumber ASCENDING.
      SORT lt_t005t         BY spras land1.
      SORT lt_t005u         BY spras land1 bland.
      SORT lt_ibpsupplier   BY supplier.
      SORT lt_ibpsupplier1  BY supplier.
      SORT lt_dfkkbptaxnum  BY partner.
      SORT lt_cdhdr         BY objectclas objectid udate DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_cdhdr COMPARING objectclas objectid.
      DELETE ADJACENT DUPLICATES FROM lt_adr6 COMPARING addrnumber.

*-------------------------- Looping The Internal Table To get Output Data------------------------------
      LOOP AT lt_lfa1 INTO ls_lfa1.
        ls_final-vendorcode      = ls_lfa1-lifnr.
        ls_final-vendorname      = ls_lfa1-name1.
        ls_final-mobileno        = ls_lfa1-telf1.

        IF l_value IS NOT INITIAL.
          IF ls_lfa1-erdat = l_value.
            ls_final-recordtype = TEXT-005."'New'.
          ELSE.
            ls_final-recordtype = TEXT-006."'Exist'.
          ENDIF.
        ENDIF.

        IF ls_lfa1-ktokk = 'IVEN'.
          ls_final-vendorcategory = TEXT-001. "'Supplier'.
        ELSEIF ls_lfa1-ktokk = 'DVEN'.
          ls_final-vendorcategory = TEXT-001. "'Supplier'.
        ELSEIF ls_lfa1-ktokk = 'SERV'.
          ls_final-vendorcategory =  TEXT-002."'Service Provider'.
        ELSEIF ls_lfa1-ktokk = 'SUBC'.
          ls_final-vendorcategory = TEXT-003. "'Subcontracter'.
        ELSEIF ls_lfa1-ktokk = 'TRAN'.
          ls_final-vendorcategory = TEXT-004. "'Transporter'.
        ELSE.
          ls_final-vendorcategory = ls_lfa1-ktokk.
        ENDIF.

        IF ls_lfa1-sperq = '99'.
          ls_final-is_deleted        ='Yes'.
        ELSE.
          ls_final-is_deleted        ='No'.
        ENDIF.


        READ TABLE lt_lfm1 INTO ls_lfm1 WITH KEY lifnr = ls_lfa1-lifnr
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-orgdata = ls_lfm1-ekorg.
        ENDIF.

        READ TABLE lt_adrc INTO ls_adrc WITH KEY addrnumber = ls_lfa1-adrnr
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE ls_adrc-house_num1 ls_adrc-street ls_adrc-str_suppl1 ls_adrc-str_suppl2 ls_adrc-str_suppl3 ls_adrc-location
                      INTO ls_final-address SEPARATED BY ''.
          ls_final-city = ls_adrc-city1.
          ls_final-postalcode = ls_adrc-post_code1.
        ENDIF.
        READ TABLE lt_adr6 INTO ls_adr6 WITH KEY addrnumber = ls_lfa1-adrnr
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-emailid = ls_adr6-smtp_addr.
        ENDIF.

        READ TABLE lt_t005t INTO ls_t005t WITH KEY spras     = sy-langu
                                                   land1     = ls_adrc-country
                                                   BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-country = ls_t005t-landx.
        ENDIF.


        READ TABLE lt_t005u INTO ls_t005u WITH KEY spras     = sy-langu
                                                   land1     = ls_adrc-country
                                                   bland     = ls_adrc-region
                                                   BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-state = ls_t005u-bezei.
        ENDIF.

        READ TABLE lt_ibpsupplier INTO ls_ibpsupplier WITH KEY supplier = ls_lfa1-lifnr
                                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_dfkkbptaxnum  INTO ls_dfkkbptaxnum WITH KEY partner = ls_ibpsupplier-businesspartner
                                                                    taxtype   = 'IN0'
                                                                    BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_final-msmenumber = ls_dfkkbptaxnum-taxnum.
          ENDIF.
          READ TABLE lt_dfkkbptaxnum  INTO ls_dfkkbptaxnum WITH KEY partner = ls_ibpsupplier-businesspartner
                                                                    taxtype   = 'IN3'
                                                                    BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_final-gstno = ls_dfkkbptaxnum-taxnum.
          ENDIF.
        ENDIF.
        READ TABLE lt_ibpsupplier1 INTO ls_ibpsupplier1 WITH KEY supplier = ls_lfa1-lifnr
                                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_cdhdr INTO ls_cdhdr WITH KEY objectclas   = 'BUPA_ADR'
                                                     objectid     = ls_ibpsupplier1-businesspartner
                                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_final-modifiedon = ls_cdhdr-udate.
          ENDIF.
        ENDIF.

        READ TABLE lt_lfa1_n INTO ls_lfa1_n WITH KEY lifnr = ls_lfa1-lifnr.
        IF sy-subrc IS INITIAL.
          CONCATENATE lv_adrnr ls_lfa1-adrnr INTO ls_lfa1_n-adrnr SEPARATED BY '  '.
        ENDIF.

        READ TABLE lt_cdhdr INTO ls_cdhdr WITH KEY objectclas   = 'ADRESSE'
                                                  objectid     = ls_lfa1_n-adrnr
                                                  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-modifiedon = ls_cdhdr-udate.
        ENDIF.

        IF l_value IS NOT INITIAL.
          ls_final-modifiedon = l_value.
        ELSE.
          ls_final-modifiedon = 00000000.
        ENDIF.

        APPEND ls_final TO lt_final.
        CLEAR ls_lfa1_n.
        CLEAR ls_lfa1.
        CLEAR ls_adrc.
        CLEAR ls_adr6.
        CLEAR ls_dfkkbptaxnum.
        CLEAR ls_ibpsupplier.
        CLEAR ls_lfm1.
        CLEAR ls_t005t.
        CLEAR ls_t005u.
        CLEAR ls_cdhdr.
        CLEAR ls_final.
        CLEAR ls_cdhdr.


      ENDLOOP.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM lt_final COMPARING vendorcode.

*---------------------------Sending The Data in JSON Format -----------------------------
    IF lt_final IS NOT INITIAL.
      CALL METHOD /ui2/cl_json=>serialize(
        EXPORTING
          data   = lt_final
        RECEIVING
          r_json = ls_json
      ).
      mo_response->create_entity( )->set_string_data( iv_data = ls_json ).
    ELSE.
      WRITE  TEXT-007. "' Data Extraction Failed'.
    ENDIF.

  ENDMETHOD.
