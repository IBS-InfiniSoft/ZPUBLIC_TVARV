*&---------------------------------------------------------------------*
*& Include          ZPUB_TVARV_C01
*&---------------------------------------------------------------------*
CLASS lcl_demo DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF mcs_tvarv_name,
        par    TYPE tvarvc-name VALUE 'ZTEST_PAR',
        selopt TYPE tvarvc-name VALUE 'ZTEST_SO',
      END OF mcs_tvarv_name.

    CLASS-METHODS:
      main.

ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD main.

    DATA(lo_tvarv) = NEW zcl_pub_tvarv( ).

* Get parameter
    DATA(lv_par) = lo_tvarv->get_param( mcs_tvarv_name-par ).
    IF lv_par IS INITIAL.
      IF lo_tvarv->is_exists( mcs_tvarv_name-par ) = abap_true.
        MESSAGE 'The value is empty, but the parameter exists'(w01) TYPE 'I'.
      ELSE.
        MESSAGE 'Parameter does not exist'(w02) TYPE 'I'.
      ENDIF.
    ELSE.
      cl_demo_output=>display_data( lv_par ).
    ENDIF.

* Get sel.opt.
    DATA(lr_so) = lo_tvarv->get_selopt( mcs_tvarv_name-selopt ).
    cl_demo_output=>display_data( lr_so ).

  ENDMETHOD.
ENDCLASS.
