*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS conversion_svc DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      get_typename
        IMPORTING
          typedescr     TYPE REF TO cl_abap_typedescr
        RETURNING
          VALUE(result) TYPE string,

      convert_to_string
        IMPORTING
          i_value       TYPE any
        RETURNING
          VALUE(result) TYPE string.

  PRIVATE SECTION.

    CONSTANTS:
      c_max_rec_depth        TYPE i VALUE 5,
      c_total_length         TYPE i VALUE 1024,
      c_level_1_length       TYPE i VALUE 512,
      c_level_2_length       TYPE i VALUE 256,
      c_comp_length          TYPE i VALUE 48,
      c_comp_length_m3       TYPE i VALUE 45,
      c_comp_name_length     TYPE i VALUE 16,
      c_comp_value_length    TYPE i VALUE 32,
      c_comp_value_length_m3 TYPE i VALUE 29.

    CLASS-DATA:
      fg_text_buffer TYPE c LENGTH c_total_length.

    TYPES:
      ty_data_references   TYPE HASHED TABLE OF REF TO data WITH UNIQUE KEY table_line,
      ty_object_references TYPE HASHED TABLE OF REF TO object WITH UNIQUE KEY table_line.


    CLASS-METHODS:
      convert_to_string_rec
        IMPORTING
          i_value                    TYPE any
          i_rec_depth                TYPE i
          VALUE(i_object_references) TYPE   ty_object_references
          VALUE(i_data_references)   TYPE   ty_data_references
        RETURNING
          VALUE(result)              TYPE string,

      convert_object_to_string_rec
        IMPORTING
          i_object                   TYPE REF TO object
          i_rec_depth                TYPE i
          i_max_length               TYPE i
          VALUE(i_object_references) TYPE   ty_object_references
          VALUE(i_data_references)   TYPE   ty_data_references
        RETURNING
          VALUE(result)              TYPE string,

      convert_struc_to_string_rec
        IMPORTING
          i_structure                TYPE data
          i_type                     TYPE REF TO cl_abap_typedescr
          i_rec_depth                TYPE i
          i_max_length               TYPE i
          VALUE(i_object_references) TYPE   ty_object_references
          VALUE(i_data_references)   TYPE   ty_data_references
        RETURNING
          VALUE(result)              TYPE string,

      convert_table_to_string
        IMPORTING
          i_table       TYPE ANY TABLE
          i_type        TYPE REF TO cl_abap_typedescr
        RETURNING
          VALUE(result) TYPE string,

      convert_number_to_string
        IMPORTING
          value         TYPE simple
        RETURNING
          VALUE(result) TYPE string.

ENDCLASS.

CLASS conversion_svc IMPLEMENTATION.


  METHOD convert_to_string.
    DATA:
      object_references TYPE ty_object_references,
      data_references   TYPE ty_data_references.

    result =
      convert_to_string_rec(
        i_value             =  i_value
        i_rec_depth         =  0
        i_object_references =  object_references
        i_data_references   =  data_references ).
  ENDMETHOD.


  METHOD convert_to_string_rec.
    DATA:
      rtti_type    TYPE REF TO cl_abap_typedescr,
      typename     TYPE string,
      object_descr TYPE REF TO cl_abap_objectdescr,
      object       TYPE REF TO object,
      attv         TYPE string,
      act_line_txt TYPE c LENGTH 64,
      buffer       TYPE string,
      sep          TYPE c LENGTH 1,
      max_len      TYPE i,
      olen         TYPE i.

    FIELD-SYMBOLS:
      <value>      TYPE any,
      <attr_descr> TYPE abap_attrdescr.

    IF ( i_rec_depth > c_max_rec_depth ).
      result = '{Nesting Too Deep}'(102).
      RETURN.
    ELSE.
      CASE i_rec_depth.
        WHEN 0.
          max_len = c_total_length.
        WHEN 1.
          max_len = c_level_1_length.
        WHEN 2.
          max_len = c_level_2_length.
        WHEN OTHERS.
          max_len = c_comp_length.
      ENDCASE.
    ENDIF.

    " upon data type recursively render the content
    rtti_type = cl_abap_typedescr=>describe_by_data( i_value ).

    CASE rtti_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_table.
        result = convert_table_to_string(
          i_table = i_value
          i_type =  rtti_type ).

      WHEN cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_char.
        result = i_value.

      WHEN cl_abap_typedescr=>typekind_dref.
        IF ( i_value IS INITIAL ).
          result = '->{Initial Reference}'(110).
          RETURN.
        ELSEIF ( i_value IS NOT BOUND ).
          result = '->{Invalid Reference}'(100).
          RETURN.
        ENDIF.

        ASSIGN i_value->* TO <value>.
        INSERT i_value INTO TABLE i_data_references.
        IF ( 0 NE sy-subrc ).
          result =  '->{Cycle}'(120).
          RETURN.
        ENDIF.

        result =
          convert_to_string_rec(
            i_value = <value>
            i_rec_depth = i_rec_depth + 1
            i_object_references = i_object_references
            i_data_references =   i_data_references ).
        CONCATENATE '->{' result '}' INTO result.


      WHEN cl_abap_typedescr=>typekind_oref.
        IF ( i_value IS INITIAL ).
          result = '[Initial Reference]'(111).
          RETURN.
        ELSEIF ( i_value IS NOT BOUND ).
          result = '[Invalid Reference]'(101).
          RETURN.
        ENDIF.

        object = i_value.
        INSERT object INTO TABLE i_object_references.
        IF ( 0 NE sy-subrc ).
          result =  '[Cycle]'(121).
          RETURN.
        ENDIF.
        result = convert_object_to_string_rec(
          i_object = object
          i_rec_depth = i_rec_depth
          i_max_length = max_len
          i_object_references = i_object_references
          i_data_references =   i_data_references ).


      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.
        result = convert_struc_to_string_rec(
          i_structure = i_value
          i_type = rtti_type
          i_rec_depth = i_rec_depth
          i_max_length = max_len
          i_object_references = i_object_references
          i_data_references =   i_data_references ).

      WHEN cl_abap_typedescr=>typekind_date.
        fg_text_buffer+0(4) = i_value+0(4).
        fg_text_buffer+4(1) = '-'.
        fg_text_buffer+5(2) = i_value+4(2).
        fg_text_buffer+7(1) = '-'.
        fg_text_buffer+8(2) = i_value+6(2).
        result = fg_text_buffer(10).

      WHEN cl_abap_typedescr=>typekind_time.
        fg_text_buffer+0(2) = i_value+0(2).
        fg_text_buffer+2(1) = ':'.
        fg_text_buffer+3(2) = i_value+2(2).
        fg_text_buffer+5(1) = ':'.
        fg_text_buffer+6(2) = i_value+4(2).
        result = fg_text_buffer(8).

      WHEN cl_abap_typedescr=>typekind_float
        OR cl_abap_typedescr=>typekind_packed
        OR cl_abap_typedescr=>typekind_int1
        OR cl_abap_typedescr=>typekind_int2
        OR cl_abap_typedescr=>typekind_int
        OR '8'. " cl_abap_typedescr=>typekind_int8.
        result = convert_number_to_string( i_value ).

      WHEN cl_abap_typedescr=>typekind_decfloat16
        OR cl_abap_typedescr=>typekind_decfloat34.
        result = convert_number_to_string( i_value ).
        CONDENSE result.

      WHEN OTHERS.
        DESCRIBE FIELD i_value OUTPUT-LENGTH olen.
        IF olen = 0.
          CONCATENATE '{????(' rtti_type->type_kind ')}' INTO result.
        ELSE.
          WRITE i_value TO fg_text_buffer USING NO EDIT MASK.
          " preserve output length / trailing spaces
          SUBTRACT 1 FROM olen.
          IF ( fg_text_buffer+olen IS INITIAL ).
            fg_text_buffer+olen = 'X'.
            result = fg_text_buffer.
            REPLACE 'X' IN SECTION OFFSET olen OF result WITH ` `.
          ELSE.
            result = fg_text_buffer.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD convert_object_to_string_rec.
    DATA:
      aunit_object    TYPE REF TO if_aunit_object,
      count_attrib    TYPE i,
      object_descr    TYPE REF TO cl_abap_objectdescr,
      typename        TYPE string,
      type_descr      TYPE REF TO cl_abap_typedescr,
      value_as_string TYPE string,
      total_length    TYPE i.
    FIELD-SYMBOLS:
      <attr_value> TYPE any,
      <attr_descr> TYPE abap_attrdescr.

    TRY.
        aunit_object ?= i_object.
        result = aunit_object->convert_to_string( ).
        total_length = strlen( result ).
        IF ( i_max_length < total_length + 2 ).
          total_length = i_max_length - 2.
          CONCATENATE '[' result(total_length) ']' INTO result.
        ELSE.
          CONCATENATE '[' result ']' INTO result.
        ENDIF.
        RETURN.
      CATCH cx_sy_move_cast_error.
        CLEAR aunit_object.
    ENDTRY.

    object_descr ?= cl_abap_objectdescr=>describe_by_object_ref( i_object ).
    typename = get_typename( object_descr ).
    CONCATENATE typename `[ ` INTO result.

    LOOP AT object_descr->attributes ASSIGNING <attr_descr>
      WHERE
        is_constant EQ abap_false AND
        is_class    EQ abap_false AND
        is_virtual  EQ abap_false AND
        visibility  EQ cl_abap_classdescr=>public AND
        alias_for   EQ '' AND
        type_kind   NE cl_abap_typedescr=>typekind_oref AND
        type_kind   NE cl_abap_typedescr=>typekind_dref.
      IF ( count_attrib > 12 OR i_max_length <= total_length ).
        EXIT.
      ENDIF.

      ASSIGN i_object->(<attr_descr>-name) TO <attr_value>.
      IF ( 0 NE sy-subrc ).
        CONTINUE.
      ENDIF.

      count_attrib = count_attrib + 1.
      value_as_string =
        convert_to_string_rec(
          i_value = <attr_value>
          i_rec_depth = i_rec_depth + 1
          i_object_references = i_object_references
          i_data_references =   i_data_references ).
      IF ( c_comp_value_length < strlen( value_as_string ) ).
        CONCATENATE value_as_string(28) ` ..` INTO value_as_string.
      ENDIF.
      CONCATENATE
        result <attr_descr>-name(c_comp_name_length) '=' value_as_string ` `
        INTO result.
      total_length = strlen( result ).
    ENDLOOP.

    LOOP AT object_descr->attributes ASSIGNING <attr_descr>
      WHERE
        is_constant EQ abap_false AND
        is_class    EQ abap_false AND
        is_virtual  EQ abap_false AND
        visibility  EQ cl_abap_classdescr=>public AND
        alias_for   EQ '' AND
        (
          type_kind   EQ cl_abap_typedescr=>typekind_oref OR
          type_kind   EQ cl_abap_typedescr=>typekind_dref
        ).

      IF ( count_attrib > 12 OR i_max_length <= total_length ).
        EXIT.
      ENDIF.

      ASSIGN i_object->(<attr_descr>-name) TO <attr_value>.
      IF ( 0 NE sy-subrc ).
        CONTINUE.
      ENDIF.

      count_attrib = count_attrib + 1.
      value_as_string =
        convert_to_string_rec(
          i_value = <attr_value>
          i_rec_depth = i_rec_depth + 1
          i_object_references = i_object_references
          i_data_references =   i_data_references ).
      IF ( c_comp_value_length < strlen( value_as_string ) ).
        CONCATENATE value_as_string(28) ` ..` INTO value_as_string.
      ENDIF.
      CONCATENATE
        result <attr_descr>-name(c_comp_name_length) '=' value_as_string ` `
        INTO result.
      total_length = strlen( result ).
    ENDLOOP.


    IF ( i_max_length <= total_length ).
      total_length = i_max_length - 3.
      CONCATENATE result(total_length) `..]` INTO result.
    ELSE.
      CONCATENATE result `]` INTO result.
    ENDIF.
  ENDMETHOD.


  METHOD convert_struc_to_string_rec.
    DATA:
      comp_descr      TYPE REF TO data,
      value_as_string TYPE string,
      struc_descr     TYPE REF TO cl_abap_structdescr,
      total_length    TYPE i.

    FIELD-SYMBOLS:
      <value>      TYPE any,
      <comp_descr> TYPE abap_compdescr.

    " upon data type recursively render the content
    struc_descr ?= i_type.

    LOOP AT struc_descr->components ASSIGNING <comp_descr> TO 64.
      ASSIGN COMPONENT <comp_descr>-name OF STRUCTURE i_structure TO <value>.
      IF ( 0 NE sy-subrc ).
        value_as_string = <comp_descr>-name.
      ELSE.
        value_as_string =
          convert_to_string_rec(
            i_value =     <value>
            i_rec_depth = i_rec_depth + 1
            i_object_references = i_object_references
            i_data_references =   i_data_references ).
      ENDIF.
      IF ( c_comp_length < strlen( value_as_string ) ).
        CONCATENATE
          value_as_string(c_comp_length_m3) ` ..` INTO value_as_string.
      ENDIF.

      IF ( result IS INITIAL ).
        CONCATENATE `[` value_as_string INTO result.
      ELSE.
        CONCATENATE result `|` value_as_string INTO result.
      ENDIF.
      total_length = strlen( result ).
      IF ( total_length >= i_max_length ).
        total_length = i_max_length - 4.
        CONCATENATE result(total_length) ` ..]` INTO result.
        RETURN.
      ENDIF.
    ENDLOOP.
    CONCATENATE result `]` INTO result.
  ENDMETHOD.


  METHOD convert_number_to_string.
    DATA:
      olen      TYPE i,
      !text     TYPE c LENGTH 100,
      pos_dot   TYPE i,
      pos_comma TYPE i.

    DESCRIBE FIELD value OUTPUT-LENGTH olen.
    IF olen = 0.
      result = '{????( NO_DISPLAY )}'.
      RETURN.
    ELSEIF ( olen > 100 ).
      olen = 99.
    ENDIF.
    WRITE value TO text(olen) USING NO EDIT MASK.

    " decimals are formatted to 'nnn,nnn.ddd'. ( decimals
    " are delimited by dot ) !
    IF ( text CS ',' ).
      pos_comma = sy-fdpos.
    ENDIF.
    IF ( text CS '.' ).
      pos_dot = sy-fdpos.
    ENDIF.

    IF ( pos_dot LT pos_comma ).
      REPLACE ALL OCCURRENCES OF '.' IN text WITH ':'.
      REPLACE ALL OCCURRENCES OF ',' IN text WITH '.'.
      REPLACE ALL OCCURRENCES OF ':' IN text WITH ','.
    ENDIF.

    " strange acrobatics to preserve trailing blanks :-(
    text+olen = '#'.
    olen = olen + 1.
    result = text(olen).
    olen = olen - 1.
    result = result(olen).
  ENDMETHOD.


  METHOD convert_table_to_string.
    DATA:
      table_descr   TYPE REF TO cl_abap_tabledescr,
      line_descr    TYPE REF TO cl_abap_typedescr,
      line_typename TYPE string,
      lines_as_text TYPE c LENGTH 9,
      size_as_text  TYPE c LENGTH 9.

    table_descr ?= i_type.
    line_descr = table_descr->get_table_line_type( ).

    lines_as_text = |{ lines( i_table ) }|.
    size_as_text = |{ line_descr->length }|.
    line_typename = get_typename( line_descr ).
    CONCATENATE
      table_descr->table_kind
      '-Table[' lines_as_text 'x' size_as_text `] of `         ##no_Text
      line_typename INTO result.
  ENDMETHOD.


  METHOD get_typename.
    DATA:
      first_chars TYPE c LENGTH 5,
      spec        TYPE string,
      len         TYPE string,
      elemdescr   TYPE REF TO cl_abap_elemdescr,
      tabledescr  TYPE REF TO cl_abap_tabledescr.

    result = typedescr->absolute_name.

    " if technical name, try better:
    IF strlen( result ) > 9 AND result(9) EQ '\TYPE=%_T'.

      CASE typedescr->kind.
        WHEN typedescr->kind_elem. " Elemental type
          CASE typedescr->type_kind.
            WHEN typedescr->typekind_string.
              result = 'TYPE=String'.

            WHEN typedescr->typekind_num
              OR typedescr->typekind_char
              OR typedescr->typekind_time
              OR typedescr->typekind_date.
              elemdescr ?= typedescr.
              len = elemdescr->output_length. CONDENSE len.
              CONCATENATE
                'TYPE=' typedescr->type_kind '('  len ')' INTO result.

            WHEN OTHERS.
              len = typedescr->length. CONDENSE len.
              CONCATENATE
                'TYPE=' typedescr->type_kind '('  len ')' INTO result.
              IF typedescr->type_kind EQ typedescr->typekind_packed
              AND typedescr->decimals GT 0.
                len = typedescr->decimals. CONDENSE len.
                CONCATENATE
                  result 'DECIMALS' len INTO result SEPARATED BY space.
              ENDIF.

          ENDCASE.

        WHEN typedescr->kind_table. " table type
          tabledescr ?= typedescr.
          CASE tabledescr->table_kind.
            WHEN cl_abap_tabledescr=>tablekind_any.
              result  = `TYPE=Any Table` ##no_Text.
            WHEN cl_abap_tabledescr=>tablekind_std.
              result  = `TYPE=Standard Table` ##no_Text.
            WHEN cl_abap_tabledescr=>tablekind_index.
              result  = `TYPE=Index Table` ##no_Text.
            WHEN cl_abap_tabledescr=>tablekind_hashed.
              result  = `TYPE=Hashed Table` ##no_Text.
            WHEN cl_abap_tabledescr=>tablekind_sorted.
              result  = `TYPE=Sorted Table` ##no_Text.
          ENDCASE.

        WHEN typedescr->kind_struct.
          CASE typedescr->type_kind.
            WHEN typedescr->typekind_struct1.
              result = `TYPE=Flat Structure` ##no_Text.
            WHEN typedescr->typekind_struct2.
              result = `TYPE=Deep Structure` ##no_Text.
          ENDCASE.

        WHEN OTHERS.
          CASE typedescr->kind.
            WHEN cl_abap_typedescr=>kind_ref.
              result = `TYPE=Reference` ##no_Text.
            WHEN cl_abap_typedescr=>kind_class.
              result = `TYPE=Class` ##no_Text.
            WHEN cl_abap_typedescr=>kind_intf.
              result = `TYPE=Interface` ##no_Text.
            WHEN OTHERS.
              result = `TYPE=(TechName)` ##no_Text.
          ENDCASE.

      ENDCASE.
    ENDIF.
    first_chars = result.
    IF ( '\TYPE' = first_chars ).
      result = result+1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
