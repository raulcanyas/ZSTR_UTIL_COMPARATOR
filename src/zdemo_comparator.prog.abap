*&---------------------------------------------------------------------*
*& Report ZDEMO_COMPARATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_comparator.

DATA: gt_diff     TYPE ztt_comparator_diff,
      gs_sflight1 TYPE sflight,
      gs_sflight2 TYPE sflight.


**********************************************************************
*  -> Pantalla de Selección
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.
SELECTION-SCREEN COMMENT /01(72) gv_info.
SELECTION-SCREEN SKIP.
PARAMETERS: p_carrid TYPE s_carr_id DEFAULT 'AA',
            p_connid TYPE s_conn_id DEFAULT '0017',
            p_fldat1 TYPE s_date,
            p_fldat2 TYPE s_date.

SELECTION-SCREEN END OF BLOCK b0.

INITIALIZATION.
  gv_info = TEXT-i00.


**********************************************************************
*  -> Lógica de programa
**********************************************************************
START-OF-SELECTION.

  PERFORM select_flights.
  PERFORM compare_flights.


**********************************************************************
*  -> Lógica de resultados
**********************************************************************
END-OF-SELECTION.

  PERFORM show_diff.


*&---------------------------------------------------------------------*
*&      Form  SELECT_FLIGHTS
*&---------------------------------------------------------------------*
FORM select_flights .

  CLEAR: gs_sflight1, gs_sflight2.

  SELECT SINGLE * INTO gs_sflight1 FROM sflight
    WHERE carrid EQ p_carrid
      AND connid EQ p_connid
      AND fldate EQ p_fldat1.

  SELECT SINGLE * INTO gs_sflight2 FROM sflight
    WHERE carrid EQ p_carrid
      AND connid EQ p_connid
      AND fldate EQ p_fldat2.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  COMPARE_FLIGHTS
*&---------------------------------------------------------------------*
FORM compare_flights .

  CLEAR: gt_diff[].
  gt_diff = zcl_util_comparator=>structures(
      is_struc_1 = gs_sflight1
      is_struc_2 = gs_sflight2 ).

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SHOW_DIFF
*&---------------------------------------------------------------------*
FORM show_diff .

  DATA: ls_diff  TYPE zst_comparator_diff,
        lv_date1 TYPE char10,
        lv_date2 TYPE char10.

  WRITE p_fldat1 TO lv_date1.
  WRITE p_fldat2 TO lv_date2.

  SKIP.
  WRITE:/ '@0S@', 'Comparación de Estructuras'.
  SKIP.

  IF gt_diff[] IS INITIAL.
    " No hay diferencias
    WRITE:/ 'No hay diferencias en los vuelos de los días ', lv_date1, ' y ', lv_date2.
  ELSE.
    WRITE:/ 'Diferencias detectadas en los vuelos de los días ', lv_date1, ' y ', lv_date2.
    SKIP.

    WRITE:/2 sy-uline(51).
    WRITE:/2 sy-vline, (15)'Campo' COLOR COL_HEADING, (15) 'Estructura 1' COLOR COL_HEADING, (15) 'Estructura 2' COLOR COL_HEADING, sy-vline.
    WRITE:/2 sy-uline(51).

    LOOP AT gt_diff INTO ls_diff.

      CONDENSE: ls_diff-value_1 NO-GAPS,
                ls_diff-value_2 NO-GAPS.

      WRITE:/2 sy-vline, (15)ls_diff-fieldname LEFT-JUSTIFIED, (15) ls_diff-value_1 LEFT-JUSTIFIED, (15) ls_diff-value_2 LEFT-JUSTIFIED, sy-vline.

    ENDLOOP.
    WRITE:/2 sy-uline(51).

  ENDIF.

ENDFORM.
