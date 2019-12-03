class ZCL_UTIL_COMPARATOR definition
  public
  final
  create private .

public section.

  class-methods STRUCTURES
    importing
      !IS_STRUC_1 type ANY
      !IS_STRUC_2 type ANY
    returning
      value(RT_DIFF) type ZTT_COMPARATOR_DIFF .
protected section.
private section.

  types:
    ty_objects TYPE HASHED TABLE OF REF TO object WITH UNIQUE KEY table_line .

  data F_COUNT_STEPS type I .
  data F_OBJECTS_VISITED type TY_OBJECTS .
  type-pools ABAP .
  data F_IGNORE_HTAB_SEQ type ABAP_BOOL .
  data MO_RESULT type ref to CL_AUNIT_FAILURE_ASSERT .

  methods CONSTRUCTOR
    importing
      !IGNORE_HTAB_SEQ type ABAP_BOOL .
  methods DIFF_HASH_TABLES
    importing
      !EXPECTED_TABLE type ANY TABLE
      !ACTUAL_TABLE type ANY TABLE
      !MASTER_MESSAGE type ref to CL_AUNIT_FAILURE_ASSERT
      !EXPECTED_LINE_LAYOUT type ANY
      !ACTUAL_LINE_LAYOUT type ANY
      !INDENT type TAUNIT_INDENT .
  methods DIFF_INDEX_TABLES
    importing
      !EXPECTED_LINE_LAYOUT type ANY
      !ACTUAL_LINE_LAYOUT type ANY
      !ACTUAL_TABLE type INDEX TABLE
      !MASTER_MESSAGE type ref to CL_AUNIT_FAILURE_ASSERT
      !EXPECTED_TABLE type INDEX TABLE
      !INDENT type TAUNIT_INDENT .
  methods DIFF_STRUCTS
    importing
      !EXPECTED_STRUCT type DATA
      !ACTUAL_STRUCT type DATA
      !MASTER_MESSAGE type ref to CL_AUNIT_FAILURE_ASSERT
      !INDENT type TAUNIT_INDENT .
  methods DIFF_TABLES
    importing
      !EXPECTED_TABLE type ANY TABLE
      !ACTUAL_TABLE type ANY TABLE
      !MASTER_MESSAGE type ref to CL_AUNIT_FAILURE_ASSERT
      !INDENT type TAUNIT_INDENT .
  methods IS_KEY_SAME_AT_INDEX
    importing
      !ACT_ENTRY type DATA
      !ACT_INDEX type I
      !EXP_TABLE type INDEX TABLE
    returning
      value(RESULT) type ABAP_BOOL .
  methods READ_DIFF_STRUCTS
    importing
      !IO_MASTER_MESSAGE type ref to CL_AUNIT_FAILURE_ASSERT
    returning
      value(RT_DIFF) type ZTT_COMPARATOR_DIFF .
ENDCLASS.



CLASS ZCL_UTIL_COMPARATOR IMPLEMENTATION.


METHOD CONSTRUCTOR.

    me->f_ignore_htab_seq = ignore_htab_seq.

  ENDMETHOD.


METHOD DIFF_HASH_TABLES.

    TYPES:
      BEGIN OF ty_ndx_reference,
        index TYPE i,
        dref  TYPE REF TO data,
        equal TYPE abap_bool,
      END OF ty_ndx_reference,
      ty_ndx_reference_table
        TYPE SORTED TABLE OF ty_ndx_reference
        WITH UNIQUE KEY dref.
    DATA:
      text_buf1 TYPE string,    " temp. text buffer
      text_buf2 TYPE string,    " temp. text buffer
      text_buf3 TYPE string.    " temp. text buffer
    DATA:
      lines_are_structs TYPE abap_bool, " 2* line types are structures
      lines_are_tables  TYPE abap_bool. " 2* line types are table
    DATA:
      type_actual_line   TYPE abaptype,  " see describe
      act_line_ndx       TYPE i,         " entry within Act table
      type_expected_line TYPE abaptype,  " see describe
      exp_line_ndx       TYPE i.         " entry within Exp tab
    DATA:
      count_ndx_mismatch TYPE i,
      ndx_reference      TYPE ty_ndx_reference,
      ndx_references     TYPE ty_ndx_reference_table.
    DATA:
      xpt_root     TYPE REF TO cx_root,
      ref_expected TYPE REF TO data.
    FIELD-SYMBOLS:
      <act_value>        TYPE any,    " cursor within Actual_Table
      <act_value_casted> TYPE any,    " dito but casted to exp. type
      <exp_value>        TYPE any.    " cursor within Expected_Tab
    DATA:
      new_indent               TYPE taunit_indent.

    me->f_count_steps = me->f_count_steps + 1.
    new_indent = indent  + 1.
    DESCRIBE FIELD actual_line_layout   TYPE type_actual_line.
    DESCRIBE FIELD expected_line_layout TYPE type_expected_line.

    IF ( type_actual_line = type_expected_line ).
      CASE  type_actual_line.
        WHEN cl_abap_typedescr=>typekind_struct1 OR
             cl_abap_typedescr=>typekind_struct2.
          lines_are_structs = abap_true.
        WHEN cl_abap_typedescr=>typekind_table.
          lines_are_tables =  abap_true.
        WHEN OTHERS.
          " thanks, no need
      ENDCASE.
    ENDIF.

    " compare Actual to Expected line by line,
    " recurse for line type table or structure
    TRY.
        LOOP AT actual_table[] ASSIGNING <act_value>.
          ADD 1 TO act_line_ndx.
          ASSIGN <act_value> TO <act_value_casted>
            CASTING LIKE expected_line_layout.

          " test existence
          READ TABLE expected_table[]
            FROM <act_value_casted>
            REFERENCE INTO ref_expected.
          IF ( 0 NE sy-subrc ).
            text_buf1 = act_line_ndx. CONDENSE text_buf1.
            text_buf2 = conversion_svc=>convert_to_string( <act_value> ).
            master_message->analysis_tab_value_missing_exp(
              index = text_buf1
              value = text_buf2
              indent = new_indent ).
            CONTINUE.
          ELSE.
            ASSIGN ref_expected->* TO <exp_value>.
          ENDIF.

          " test identity
          ndx_reference-dref =   ref_expected.
          ndx_reference-index =  act_line_ndx.

          IF ( <act_value_casted> = <exp_value> ).
            ndx_reference-equal = abap_true.
            INSERT ndx_reference INTO TABLE ndx_references[].

          ELSE.
            ndx_reference-equal = abap_false.
            INSERT ndx_reference INTO TABLE ndx_references[].

            text_buf1 = act_line_ndx. CONDENSE text_buf1.
            text_buf2 = conversion_svc=>convert_to_string( <act_value> ).
            text_buf3 = conversion_svc=>convert_to_string( <exp_value> ).

            IF ( abap_true         EQ lines_are_structs              AND
                 text_buf2         EQ text_buf3                      AND
              type_actual_line  EQ cl_abap_typedescr=>typekind_struct2 ).

              master_message->analysis_type_deep_structure(
                index = text_buf1
                type =  text_buf2
                indent = new_indent ).


            ELSEIF ( lines_are_tables = abap_true AND
                     text_buf2 EQ text_buf3 ).
              master_message->analysis_type_table(
                index =  text_buf1
                type =   text_buf2
                indent = new_indent ).

            ELSE.
              master_message->analysis_tab_value_different(
                index = text_buf1
                act = text_buf2
                exp = text_buf3
                indent = new_indent ).

            ENDIF.

            IF ( abap_true EQ lines_are_tables ).
              diff_tables(
                actual_table =   <act_value_casted>
                expected_table = <exp_value>
                master_message = master_message
                indent = new_indent ).

            ELSEIF ( abap_true EQ lines_are_structs ).
              diff_structs(
                actual_struct =   <act_value_casted>
                expected_struct = <exp_value>
                master_message = master_message
                indent = new_indent ).
            ENDIF.
          ENDIF.
        ENDLOOP.

      CATCH cx_root INTO xpt_root ##catch_All.
        text_buf1 = xpt_root->get_text( ).
        master_message->analysis_intern_err_exception(
          text = text_buf1
          indent = new_indent ).
    ENDTRY.

    " and now the other way round find Expected lines not included in
    " actual due indices are meaningless we take the hard and windy
    " road of references
    act_line_ndx = exp_line_ndx = 0.
    TRY.
        LOOP AT expected_table[] REFERENCE INTO ref_expected.
          ADD 1 TO exp_line_ndx.
          READ TABLE ndx_references[]
            WITH TABLE KEY dref = ref_expected INTO ndx_reference.
          IF ( 0 NE sy-subrc ).
            text_buf1 = exp_line_ndx. CONDENSE text_buf1.
            ASSIGN ref_expected->* TO <exp_value>.
            text_buf2 =
              conversion_svc=>convert_to_string( <exp_value> ).
            " Value <&1> (Index &2) of expected table not in act table
            master_message->analysis_tab_value_missing_act(
              index = text_buf1
              value = text_buf2
              indent = new_indent ).
            CONTINUE.
          ENDIF.

          " test for same position
          IF ( abap_true NE me->f_ignore_htab_seq AND 2 > count_ndx_mismatch ).
            IF ( abap_true         EQ ndx_reference-equal AND
                 exp_line_ndx NE ndx_reference-index ).
              count_ndx_mismatch = count_ndx_mismatch + 1.
              " notify of particular index mismatch
              text_buf1 = ndx_reference-index.    CONDENSE text_buf1.
              text_buf2 = exp_line_ndx. CONDENSE text_buf2.
              ASSIGN ref_expected->* TO <exp_value>.
              text_buf3 =
                conversion_svc=>convert_to_string( <exp_value> ).
              " Same Value &3 but different indices, expected &1 act &2
              master_message->analysis_tab_diff_index(
                exp_index = text_buf2
                act_index = text_buf1
                value = text_buf3
                indent = new_indent ).
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF ( 2 < count_ndx_mismatch ).
          master_message->analysis_sequence_in_hash(
            indent = new_indent ).
        ENDIF.

      CATCH cx_root INTO xpt_root ##catch_All.
        text_buf1 = xpt_root->get_text( ).
        " Internal Error: Exception &1
        master_message->analysis_intern_err_exception(
          text = text_buf1
          indent = new_indent ).

    ENDTRY.

  ENDMETHOD.


METHOD DIFF_INDEX_TABLES.

    " computes delta of 2 index based tables
    TYPES:
      BEGIN OF ty_ndx_reference,
        index TYPE i,
        equal TYPE abap_bool,
      END OF ty_ndx_reference,
      ty_ndx_reference_table
        TYPE SORTED TABLE OF ty_ndx_reference
        WITH UNIQUE KEY index.
    DATA:
      lines_are_structs TYPE abap_bool, " both line types are str
      lines_are_tables  TYPE abap_bool. " both line types are tab
    DATA:
      act_line_ndx       TYPE i,         " index of line in actual table
      act_value_text     TYPE string,    " line of actual table as text
      exp_line_ndx       TYPE i,         " index of line in exp. table
      exp_value_text     TYPE string,    " line of exp. table as text
      cnt_expected_lines TYPE i,
      is_same_key        TYPE abap_bool.
    DATA:
      type_act_line      TYPE REF TO cl_abap_datadescr,
      type_exp_line      TYPE REF TO cl_abap_datadescr,
      type_act_table     TYPE REF TO cl_abap_tabledescr,
      type_exp_table     TYPE REF TO cl_abap_tabledescr,
      primary_key_fields TYPE string_table.
    DATA:
      ndx_reference  TYPE ty_ndx_reference,
      ndx_references TYPE ty_ndx_reference_table.
    DATA:
      xpt_root                 TYPE REF TO cx_root.
    DATA:
      new_indent               TYPE taunit_indent.
    FIELD-SYMBOLS:
      <act_value>        TYPE any,    " cursor within Actual_Table
      <act_value_casted> TYPE any,    " dito but casted
      <exp_value>        TYPE any.    " cursor within Expected_Tab

    me->f_count_steps = me->f_count_steps + 1.
    new_indent = indent + 1.

    type_act_table ?= cl_abap_typedescr=>describe_by_data( actual_table ).
    type_act_line = type_act_table->get_table_line_type( ).
    type_exp_table ?= cl_abap_typedescr=>describe_by_data( expected_table ).
    type_exp_line = type_exp_table->get_table_line_type( ).

    IF ( type_act_line->kind = type_exp_line->kind ).
      CASE type_act_line->kind.
        WHEN cl_abap_typedescr=>kind_struct.
          lines_are_structs = abap_true.
        WHEN cl_abap_typedescr=>kind_table.
          lines_are_tables =  abap_true.
        WHEN OTHERS.
          " thanks, no need
      ENDCASE.
    ENDIF.

    " compare Actual to Expected line by line, recurse for line type
    " table or structure
    TRY.
        LOOP AT actual_table[] ASSIGNING <act_value>.
          ADD 1 TO act_line_ndx.
          UNASSIGN <exp_value>.

          " test existence by index
          READ TABLE expected_table[]
            INDEX act_line_ndx
            ASSIGNING <exp_value>.
          IF ( 0 EQ sy-subrc AND <act_value> EQ <exp_value> ).
            ndx_reference-index = exp_line_ndx = act_line_ndx.
            INSERT ndx_reference INTO TABLE ndx_references[].
            CONTINUE.
          ENDIF.

          ASSIGN <act_value> TO <act_value_casted>
            CASTING LIKE expected_line_layout.

          " full match at different index ?
          READ TABLE expected_table[]
            WITH KEY table_line = <act_value_casted>
            TRANSPORTING NO FIELDS.
          IF ( 0 EQ sy-subrc ).
            ndx_reference-index = exp_line_ndx = sy-tabix.
            INSERT ndx_reference INTO TABLE ndx_references[].
            " same value <&3> but different indices: Expected &1, but &2.
            act_value_text = conversion_svc=>convert_to_string( <act_value> ).
            master_message->analysis_tab_diff_index(
              exp_index = |{ exp_line_ndx }|
              act_index = |{ act_line_ndx }|
              value = act_value_text
              indent = new_indent ).
            CONTINUE.
          ENDIF.

          " key field match ?
          READ TABLE expected_table[]
            FROM <act_value_casted> ASSIGNING <exp_value>.
          IF ( 0 EQ sy-subrc ).
            exp_line_ndx = sy-tabix.
          ELSE.
            act_value_text = conversion_svc=>convert_to_string( <act_value> ).
            " value <&1> (index &2) of Actual table not within Expected
            master_message->analysis_tab_value_missing_exp(
              index = |{ act_line_ndx }|
              value = act_value_text
              indent = new_indent ).
            CONTINUE.
          ENDIF.

          " the above read statement takes the 1st match which might not be
          " ideal in case a table is not unique and the expected entry in the
          " expected table with the actual index has a key match also
          IF ( exp_line_ndx < act_line_ndx ).
            is_same_key = is_key_same_at_index(
              act_entry =   <act_value_casted>
              act_index =   act_line_ndx
              exp_table =   expected_table  ).
            IF ( abap_true EQ is_same_key ).
              READ TABLE expected_table INDEX act_line_ndx ASSIGNING <exp_value>.
              exp_line_ndx = act_line_ndx.
            ENDIF.
          ENDIF.
          ndx_reference-index = exp_line_ndx.
          INSERT ndx_reference INTO TABLE ndx_references[].

          " render difference in content
          act_value_text = conversion_svc=>convert_to_string( <act_value> ).
          exp_value_text = conversion_svc=>convert_to_string( <exp_value> ).

          IF ( act_line_ndx = exp_line_ndx ).
            IF ( act_value_text EQ exp_value_text ).
              CASE type_act_line->type_kind.
                WHEN cl_abap_typedescr=>typekind_struct2.
                  master_message->analysis_type_deep_structure(
                   index =  |{ act_line_ndx }|
                   type =   act_value_text
                   indent = new_indent ).
                WHEN cl_abap_typedescr=>typekind_table.
                  master_message->analysis_type_table(
                   index =  |{ act_line_ndx }|
                   type =   act_value_text
                   indent = new_indent ).
                WHEN OTHERS.
                  master_message->analysis_tab_value_different(
                    index = |{ act_line_ndx }|
                    act = act_value_text
                    exp = exp_value_text
                    indent = new_indent ).
              ENDCASE.
            ELSE.
              master_message->analysis_tab_value_different(
                index = |{ act_line_ndx }|
                act = act_value_text
                exp = exp_value_text
                indent = new_indent ).
            ENDIF.
          ELSE.
            IF ( type_act_line->type_kind EQ cl_abap_typedescr=>typekind_struct2 ).
              master_message->analysis_type_deep_structure(
               index =  |{ act_line_ndx }|
               index2 = |{ exp_line_ndx }|
               type =   act_value_text
               indent = new_indent ).
            ELSE.
              master_message->analysis_tab_value_diff_index(
                act_index = |{ act_line_ndx }|
                act_val =   act_value_text
                exp_index = |{ exp_line_ndx }|
                exp_val =   exp_value_text
                indent =    new_indent  ).
            ENDIF.
          ENDIF.

          " Check For More Detailed Info
          IF ( abap_true EQ lines_are_tables ).
            diff_tables(
              actual_table =   <act_value_casted>
              expected_table = <exp_value>
              master_message = master_message
              indent = new_indent ).
          ELSEIF ( abap_true EQ lines_are_structs ).
            diff_structs(
              actual_struct =   <act_value_casted>
              expected_struct = <exp_value>
              master_message = master_message
              indent = new_indent ).
          ENDIF.
        ENDLOOP.
      CATCH cx_root INTO xpt_root  ##catch_All.
        master_message->analysis_intern_err_exception(
          text =   xpt_root->get_text( )
          indent = new_indent ).
    ENDTRY.

    " warn about not available entries in Expected_Table
    TRY.
        exp_line_ndx = 0.
        cnt_expected_lines = lines( expected_table[] ).
        IF ( cnt_expected_lines NE lines( ndx_references[] ) ).
          DO cnt_expected_lines TIMES.
            ADD 1 TO exp_line_ndx.
            READ TABLE ndx_references[]
             WITH TABLE KEY index = exp_line_ndx
             TRANSPORTING NO FIELDS.

            IF ( 0 NE sy-subrc ).
              READ TABLE expected_table[]
                INDEX exp_line_ndx
                ASSIGNING <exp_value>.
              IF ( 0 EQ sy-subrc ).
                exp_value_text =
                  conversion_svc=>convert_to_string( <exp_value> ).
                master_message->analysis_tab_value_missing_act(
                  index =   |{ exp_line_ndx }|
                  value =  exp_value_text
                  indent = new_indent ).
              ENDIF.
            ENDIF.
          ENDDO.
        ENDIF.

      CATCH cx_root INTO xpt_root ##catch_All.
        master_message->analysis_intern_err_exception(
          text =   xpt_root->get_text( )
          indent = new_indent ).
    ENDTRY.

  ENDMETHOD.


METHOD DIFF_STRUCTS.

    " computes delta of 2 structures
    DATA:
      dref_actual TYPE REF TO data,
      text_buf1   TYPE string,        " temp. text buffer
      text_buf2   TYPE string.        " temp. text buffer
    DATA:
      comp_index    TYPE i,
      rtti_actual   TYPE REF TO cl_abap_structdescr,
      rtti_expected TYPE REF TO cl_abap_structdescr.
    DATA:
      new_indent               TYPE taunit_indent.
    FIELD-SYMBOLS:
      <actual_struct> TYPE data,
      <actual_comp>   TYPE data,
      <expected_comp> TYPE data,
      <rtti_comp>     TYPE abap_compdescr.

    me->f_count_steps = me->f_count_steps + 1.
    new_indent = indent + 1.
    rtti_expected ?=
      cl_abap_typedescr=>describe_by_data( expected_struct ).
    TRY.
        rtti_actual ?=
          cl_abap_typedescr=>describe_by_data( actual_struct ).
        ASSIGN actual_struct TO <actual_struct>.
      CATCH cx_sy_move_cast_error.
        " Actual result is no structure, create Expected format
        rtti_actual = rtti_expected.
        CREATE DATA dref_actual TYPE HANDLE rtti_actual.
        ASSIGN dref_actual->* TO <actual_struct>.
        <actual_struct> = actual_struct.
    ENDTRY.

    IF ( rtti_actual->components[] NE rtti_expected->components[] ).
      " Act-Structure Differs From Exp-Structure
      master_message->analysis_diff_structure( indent = new_indent ).
      RETURN.
    ENDIF.

    LOOP AT rtti_actual->components[]
      ASSIGNING <rtti_comp>.                             "#EC CI_BOX_OK
      comp_index = sy-tabix.
      ASSIGN COMPONENT comp_index OF STRUCTURE:
        <actual_struct>  TO <actual_comp>,
        expected_struct  TO <expected_comp>.
      IF ( <actual_comp>   IS NOT ASSIGNED OR
           <expected_comp> IS NOT ASSIGNED ).
        " Internal Problem with Component <&1>.
        master_message->analysis_int_problem_component(
          param1 = <rtti_comp>-name
          indent = new_indent ).
        CONTINUE.
      ELSEIF ( <actual_comp> EQ <expected_comp> ).
        CONTINUE.
      ENDIF.

      text_buf1 = conversion_svc=>convert_to_string( <actual_comp> ).
      text_buf2 = conversion_svc=>convert_to_string( <expected_comp> ).

      IF <rtti_comp>-type_kind = cl_abap_typedescr=>typekind_struct2
            AND ( text_buf1 EQ text_buf2 ).
        master_message->analysis_type_deep_structure(
          component = <rtti_comp>-name
          type = text_buf1 indent = new_indent ).

      ELSEIF <rtti_comp>-type_kind = cl_abap_typedescr=>typekind_table
      AND ( text_buf1 EQ text_buf2 ).
        master_message->analysis_type_table(
          component = <rtti_comp>-name
          type = text_buf1 indent = new_indent ).

      ELSE.
        master_message->analysis_diff_struct_component(
          component = <rtti_comp>-name
          act = text_buf1
          exp = text_buf2
          indent = new_indent ).

      ENDIF.

      " decent recursive
      CASE <rtti_comp>-type_kind.
        WHEN cl_abap_typedescr=>typekind_struct1 OR
             cl_abap_typedescr=>typekind_struct2.
          diff_structs(
            expected_struct = <expected_comp>
            actual_struct =   <actual_comp>
            master_message = master_message
            indent = new_indent ).

        WHEN cl_abap_typedescr=>typekind_table.
          diff_tables(
            expected_table = <expected_comp>
            actual_table =   <actual_comp>
            master_message = master_message
            indent = new_indent ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


METHOD DIFF_TABLES.

    " computes delta of 2 tables
    DATA:
      actual_table_kind   TYPE abaptype,      " hash/std/sorted
      expected_table_kind TYPE abaptype.      " see describe
    DATA:
      new_indent TYPE taunit_indent.
    FIELD-SYMBOLS:
      <actual_layout>   TYPE any,    " cast template for line type
      <expected_layout> TYPE any.    " cast template for line type

    me->f_count_steps = me->f_count_steps + 1.
    new_indent = indent + 1.
    " initialize & precheck, tables cannot be empty due the field-symbol
    " technique used later, requires a least one line within each table
    IF ( actual_table[] IS  INITIAL  ).
      master_message->analysis_act_table_initial( indent = new_indent ).
      RETURN.
    ENDIF.

    IF ( expected_table[] IS  INITIAL  ).
      master_message->analysis_exp_table_initial( indent = new_indent ).
      RETURN.
    ENDIF.

    LOOP AT actual_table[] ASSIGNING <actual_layout>.
      EXIT. " loop
    ENDLOOP.
    LOOP AT expected_table[] ASSIGNING <expected_layout>.
      EXIT. " loop
    ENDLOOP.

    DESCRIBE TABLE actual_table[]        KIND actual_table_kind.
    DESCRIBE TABLE expected_table[]      KIND expected_table_kind.

    IF ( actual_table_kind   EQ sydes_kind-hashed OR
         expected_table_kind EQ sydes_kind-hashed ).
      diff_hash_tables(
        actual_table =          actual_table[]
        expected_table =        expected_table[]
        actual_line_layout =    <actual_layout>
        expected_line_layout =  <expected_layout>
        master_message =        master_message
        indent =                indent ).
    ELSE.
      diff_index_tables(
        actual_table =          actual_table[]
        expected_table =        expected_table[]
        actual_line_layout =    <actual_layout>
        expected_line_layout =  <expected_layout>
        master_message =        master_message
        indent =                indent ).
    ENDIF.

  ENDMETHOD.


METHOD IS_KEY_SAME_AT_INDEX.

    DATA:
      table_buffer TYPE REF TO data.
    FIELD-SYMBOLS:
      <buffered_table> TYPE INDEX TABLE,
      <exp_entry>      TYPE data.

    READ TABLE exp_table INDEX act_index ASSIGNING <exp_entry>.
    IF ( 0 NE sy-subrc ).
      RETURN.
    ENDIF.

    CREATE DATA table_buffer LIKE !exp_table.
    ASSIGN table_buffer->* TO <buffered_table>.
    INSERT <exp_entry> INTO TABLE <buffered_table>.

    READ TABLE <buffered_table> FROM act_entry TRANSPORTING NO FIELDS.
    IF ( 0 EQ sy-subrc ).
      result = abap_true.
    ENDIF.
    RETURN.

  ENDMETHOD.


METHOD read_diff_structs.

    DATA: ls_analysis_field TYPE taunit_descr,
          ls_analysis_value TYPE taunit_descr,
          ls_diff           TYPE zst_comparator_diff,
          lv_tabix_value    TYPE sytabix,
          lv_tabix_value_2  TYPE sytabix.


    LOOP AT io_master_message->f_analysis INTO ls_analysis_field
      WHERE id EQ 'FA32'.

      lv_tabix_value = sy-tabix + 1.

      READ TABLE io_master_message->f_analysis
        INTO ls_analysis_value INDEX lv_tabix_value.
      IF sy-subrc EQ 0 AND ls_analysis_value-id EQ 'FA04'.

        CLEAR: ls_diff.
        READ TABLE ls_analysis_field-params INTO ls_diff-fieldname INDEX 1. " ID Campo
        READ TABLE ls_analysis_value-params INTO ls_diff-value_2 INDEX 1.   " Valor estructura 2
        READ TABLE ls_analysis_value-params INTO ls_diff-value_1 INDEX 2.   " Valor estructura 1
        APPEND ls_diff TO rt_diff.

      ELSEIF sy-subrc EQ 0 AND ls_analysis_value-id EQ 'FA02'. " Solo contiene el valor nuevo

        CLEAR: ls_diff.
        READ TABLE ls_analysis_field-params INTO ls_diff-fieldname INDEX 1. " ID Campo
        READ TABLE ls_analysis_value-params INTO ls_diff-value_2 INDEX 1.
        lv_tabix_value_2 = lv_tabix_value + 1.
        READ TABLE io_master_message->f_analysis
          INTO ls_analysis_value INDEX lv_tabix_value_2.
        READ TABLE ls_analysis_value-params INTO ls_diff-value_1 INDEX 1.
        APPEND ls_diff TO rt_diff.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD structures.

    DATA: lo_comparator TYPE REF TO zcl_util_comparator.

    CREATE OBJECT lo_comparator EXPORTING ignore_htab_seq = abap_true.

    lo_comparator->mo_result = cl_aunit_failure_assert_c=>create_failure(
                                    user_msg =  'DUMMY'
                                    level    =   0
                                    caller   =  'DUMMY' ).

    lo_comparator->diff_structs(
                expected_struct = is_struc_2
                actual_struct   = is_struc_1
                master_message  = lo_comparator->mo_result
                indent          = 0 ).

    CLEAR: rt_diff.
    rt_diff = lo_comparator->read_diff_structs(
                io_master_message = lo_comparator->mo_result ).

  ENDMETHOD.
ENDCLASS.
