class ZCL_PUB_TVARV definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_s_param,
             mandt TYPE tvarvc-mandt,
             name  TYPE tvarvc-name,
             type  TYPE tvarvc-type,
             low   TYPE tvarvc-low,
           END OF ty_s_param .
  types:
    ty_t_param TYPE SORTED TABLE OF ty_s_param WITH UNIQUE KEY mandt name type .
  types:
    BEGIN OF ty_s_selopt,
             mandt  TYPE tvarvc-mandt,
             name   TYPE tvarvc-name,
             type   TYPE tvarvc-type,
             numb   TYPE tvarvc-numb,
             sign   TYPE tvarvc-sign,
             option TYPE tvarvc-opti,
             low    TYPE tvarvc-low,
             high   TYPE tvarvc-high,
           END OF ty_s_selopt .
  types:
    ty_t_selopt TYPE SORTED TABLE OF  ty_s_selopt WITH UNIQUE KEY mandt name type numb .

  constants:
    BEGIN OF mcs_type,
        param  TYPE tvarvc-type VALUE 'P',
        selopt TYPE tvarvc-type VALUE 'S',
      END OF mcs_type .

  methods GET_PARAM
    importing
      !IV_NAME type TVARVC-NAME
      !IV_CLIENT type SYST_MANDT default SY-MANDT
    returning
      value(RV_PARAM) type TVARVC-LOW .
  methods IS_EXISTS
    importing
      !IV_NAME type TVARVC-NAME
      !IV_TYPE type TVARVC-TYPE optional
      !IV_CLIENT type SYST_MANDT default SY-MANDT
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods GET_SELOPT
    importing
      !IV_NAME type TVARVC-NAME
      !IV_CLIENT type SYST_MANDT default SY-MANDT
    returning
      value(RT_SELOPT) type EFG_TAB_RANGES .
  PROTECTED SECTION.
private section.

  data MT_PARAM type TY_T_PARAM .
  data MT_SELOPT type TY_T_SELOPT .
ENDCLASS.



CLASS ZCL_PUB_TVARV IMPLEMENTATION.


  METHOD get_param.

    CLEAR rv_param.

    IF iv_name IS INITIAL.
      RETURN.
    ENDIF.

* get parameter from buffer
    READ TABLE mt_param[] ASSIGNING FIELD-SYMBOL(<ls_param>)
                          WITH TABLE KEY mandt = iv_client
                                         name  = iv_name
                                         type  = mcs_type-param.
    IF sy-subrc = 0.
      rv_param = <ls_param>-low.
      RETURN.
    ENDIF.


* if not in buffer, then read from db
    INSERT VALUE #( mandt = iv_client
                    name  = iv_name
                    type  = mcs_type-param
                    low   = space
                  ) INTO TABLE mt_param[] ASSIGNING <ls_param>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT low UP TO 1 ROWS
      FROM tvarvc CLIENT SPECIFIED
      INTO @rv_param
      WHERE mandt = @iv_client
        AND type  = @mcs_type-param
        AND name  = @iv_name
    ORDER BY mandt, name, type, numb. "#EC CI_BYPASS.
    ENDSELECT.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    <ls_param>-low = rv_param.

  ENDMETHOD.


  METHOD get_selopt.
    DATA: lt_selopt TYPE ty_t_selopt.

    CLEAR rt_selopt[].

    IF iv_name IS INITIAL.
      RETURN.
    ENDIF.

* get sel.opt. from buffer
    LOOP AT mt_selopt[] ASSIGNING FIELD-SYMBOL(<ls_selopt>)
                        WHERE mandt = iv_client
                          AND name  = iv_name
                          AND type  = mcs_type-selopt.
      IF <ls_selopt>-sign IS INITIAL
        OR <ls_selopt>-option IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND CORRESPONDING #( <ls_selopt> ) TO rt_selopt[].
    ENDLOOP.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

* if not in buffer, then read from db
    SELECT mandt, name, type, numb,
      sign, opti AS option, low, high
      FROM tvarvc CLIENT SPECIFIED
      INTO TABLE @lt_selopt
      WHERE mandt = @iv_client
        AND type  = @mcs_type-selopt
        AND name  = @iv_name.
    IF sy-subrc <> 0.
      INSERT VALUE #( mandt = iv_client
                      name  = iv_name
                      type  = mcs_type-selopt
                    ) INTO TABLE mt_selopt[].
      RETURN.
    ENDIF.

    INSERT LINES OF lt_selopt[] INTO TABLE mt_selopt[].
    rt_selopt[] = CORRESPONDING #( lt_selopt[] ).
  ENDMETHOD.


  METHOD is_exists.
    DATA: lr_type TYPE RANGE OF tvarvc-type.

    rv_exists = abap_false.

    IF iv_name IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_type IS NOT INITIAL.
      lr_type = VALUE #( ( sign = 'I' option = 'EQ' low = iv_type high = space ) ).
    ELSE.
      lr_type = VALUE #( ( sign = 'I' option = 'EQ' low = mcs_type-param high = space )
                         ( sign = 'I' option = 'EQ' low = mcs_type-selopt high = space )
                        ).
    ENDIF.

    SELECT @abap_true  ##NEEDED
      FROM tvarvc CLIENT SPECIFIED
      INTO @rv_exists
      WHERE mandt = @iv_client
        AND type IN @lr_type[]
        AND name  = @iv_name.
    ENDSELECT.

  ENDMETHOD.
ENDCLASS.
