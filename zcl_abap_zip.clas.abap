"! ZIP Utility
"!
"! By default and for backward compatibility, CL_ABAP_ZIP uses codepage 437 (the old DOS codepage) for the filenames.
"! If the filenames can contain arbitrary characters (in particular, characters from other than West-European languages),
"! it may be preferable to use Unicode filenames by setting the attribute support_unicode_names:
"! DATA(lo_zip) = NEW cl_abap_zip( ).
"! lo_zip->support_unicode_names = abap_true.
class ZCL_ABAP_ZIP definition
  public
  final
  create public .

public section.
  type-pools ABAP .
  type-pools IHTTP .

  types:
    YT_RAW4 type standard table of raw4 .
  types:
    "! Type of file directory entry
    BEGIN OF T_FILE,
           name TYPE string,
           date TYPE d,
           time TYPE t,
           size TYPE i,
         END OF T_FILE .
  types:
    "! Directory table type
    T_FILES TYPE STANDARD TABLE OF T_FILE .
  types:
    "! Type for splice entry
    BEGIN OF T_SPLICE_ENTRY,
           name       TYPE string,
           offset     TYPE i,
           length     TYPE i,
           compressed TYPE i,
         END OF T_SPLICE_ENTRY .
  types:
    "! Splice table type
    T_SPLICE_ENTRIES TYPE STANDARD TABLE OF T_SPLICE_ENTRY WITH DEFAULT KEY .
  types:
   "! Type for pair of date/time as timestamp
    begin of t_file_timestamp,
      date type d,
      time type t,
   end of t_file_timestamp .

  "! Directory
  data FILES type T_FILES read-only .
  "! Feature initially disabled because of compatibility
  data SUPPORT_UNICODE_NAMES type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  "! Return code
  class-data SPLICE_RC type I .

  "! Load ZIP file
  methods LOAD
    importing
      !ZIP type XSTRING
      !CHECK_HEADER type ABAP_BOOL default ABAP_TRUE
    exceptions
      ZIP_PARSE_ERROR .
  "! Create ZIP file
  methods SAVE
    importing
      value(IF_PASSWORD) type STRING optional
    returning
      value(ZIP) type XSTRING .
  "! Read file from ZIP folder
  "!
  "! @parameter name    | Name of file (case sensitive)
  "! @parameter index   | Alternate  Index of file
  "! @parameter content | Data of file
  methods GET
    importing
      !NAME type STRING optional
      value(INDEX) type I default 0
    exporting
      !CONTENT type XSTRING
    exceptions
      ZIP_INDEX_ERROR
      ZIP_DECOMPRESSION_ERROR .
  "! Add filed to ZIP folder
  "!
  "! @parameter name           | Name of file
  "! @parameter compress_level | Compression level (1-9)
  "! @parameter FILE_TIMESTAMP | Timestamp of file to be used in archive as pair of date and time
  methods ADD
    importing
      !NAME type STRING
      !CONTENT type XSEQUENCE
      !COMPRESS_LEVEL type I default 6
      !FILE_TIMESTAMP type T_FILE_TIMESTAMP optional .
  "! Delete file from ZIP folder
  "!
  "! @parameter name  | Name (case sensitive)
  "! @parameter index | Alternate index of file
  methods DELETE
    importing
      !NAME type STRING optional
      value(INDEX) type I default 0
    exceptions
      ZIP_INDEX_ERROR .
  "! Calculate CRC32 for data
  "!
  "! @parameter content  | Data
  "! @parameter crc32    | CRC32 value
  class-methods CRC32
    importing
      !CONTENT type XSTRING
    returning
      value(CRC32) type I .
  "! Load and split ZIP file
  "!
  "! @parameter ZIP             | Content of ZIP file
  "! @parameter support_unicode | Flag for UNICODE support (UTF8)
  class-methods SPLICE
    importing
      !ZIP type XSTRING
      !SUPPORT_UNICODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(ENTRIES) type T_SPLICE_ENTRIES .
  methods UPDATE_KEYS
    importing
      !IS_HEADER type CHAR1 default SPACE
    changing
      !IF_DATA type XSTRING
      !KEY0 type RAW4
      !KEY1 type RAW4
      !KEY2 type RAW4 .
  methods ADD_HEADER
    importing
      !CRC type I
    changing
      value(KEY0) type RAW4
      value(KEY1) type RAW4
      value(KEY2) type RAW4
    returning
      value(HEADER) type DB2INSTNO .
  methods MULTIPLY_X
    importing
      !A type RAW4
      !B type RAW4
    returning
      value(RESULT) type RAW4 .
  methods ADD_X
    importing
      !A type RAW4
      !B type RAW4
    returning
      value(RESULT) type RAW4 .
  methods LEFT_SHIFT_X
    importing
      !VALUE type RAW4
      !POSITIONS type I
    returning
      value(RETURNING) type RAW4 .
  methods CONSTRUCTOR .
  methods UNSIGNED_RIGHT_SHIFT_X
    importing
      !VALUE type RAW4
      !POSITIONS type I
    returning
      value(RETURNING) type RAW4 .
  methods RIGHT_SHIFT_X
    importing
      !VALUE type RAW4
      !POSITIONS type I
    returning
      value(RETURNING) type RAW4 .
protected section.
*"* protected components of class ZCL_ABAP_ZIP
*"* do not include other source files here!!!
  constants:
    H_00000001 type X LENGTH 4 value '00000001'. "#EC NOTEXT
  constants:
    H_3FFFFFFF type X LENGTH 4 value '3FFFFFFF'. "#EC NOTEXT
  constants:
    H_40000000 type X length 4 value '40000000'. "#EC NOTEXT
  constants:
    H_7FFFFFFF type X length 4 value '7FFFFFFF'. "#EC NOTEXT
  constants:
    H_80000000 type X length 4 value '80000000'. "#EC NOTEXT
    class-data:
    POWER_OF_2 type STANDARD TABLE OF I .
private section.
*"* private components of class ZCL_ABAP_ZIP
*"* do not include other source files here!!!

  types:
    BEGIN OF T_EXT,
           min_extract_version TYPE i,
           gen_flags           TYPE i,
           compressed          TYPE i,
           compsize            TYPE i,
           crc32(4)            TYPE x,
           filename_len        TYPE i,
           filename            TYPE xstring,
           extra_len           TYPE i,
           extra               TYPE xstring,
           content             TYPE xstring,
         END OF T_EXT .
  types:
    T_EXTS TYPE STANDARD TABLE OF T_EXT .

  data EXTS type T_EXTS .
  class-data CRC32_MAP type XSTRING .
ENDCLASS.



CLASS ZCL_ABAP_ZIP IMPLEMENTATION.


METHOD ADD.

  FIELD-SYMBOLS: <file> TYPE t_file,
                 <ext>  TYPE t_ext.

  APPEND INITIAL LINE TO files ASSIGNING <file>.
  APPEND INITIAL LINE TO exts  ASSIGNING <ext>.

  <file>-name = name.
  if file_timestamp is supplied.
    <file>-date = file_timestamp-date.
    <file>-time = file_timestamp-time.
  else.
    <file>-date = sy-datum.
    <file>-time = sy-uzeit.
  endif.

  <file>-size = XSTRLEN( content ).
* general purpose flag bit 11 (Language encoding flag (EFS)
  CONSTANTS: gen_flags_unicode(2) TYPE x VALUE '0800'.
* see: http://www.pkware.com/documents/casestudies/APPNOTE.TXT, APPENDIX D
* zip normaly used IBM Code Page 437 mapped to SAP Printer EPESCP IBM 437

  DATA: conv       TYPE REF TO cl_abap_conv_out_ce,
        conv_cp437 TYPE REF TO cl_abap_conv_out_ce,
        conv_utf8  TYPE REF TO cl_abap_conv_out_ce,
        cp437      TYPE abap_encoding value '1107', " IBM 437
        utf8       TYPE abap_encoding value '4110'. " UTF-8
  if support_unicode_names = abap_true.
     conv = cl_abap_conv_out_ce=>create( encoding = utf8
                                      ignore_cerr = abap_true
                                      replacement = '#' ).
  else.
     conv = cl_abap_conv_out_ce=>create( encoding = cp437
                                      ignore_cerr = abap_true
                                      replacement = '#' ).
  endif.
  conv->convert( EXPORTING data = <file>-name IMPORTING buffer = <ext>-filename ).
  <ext>-filename_len        = XSTRLEN( <ext>-filename ).
  <ext>-extra_len           = 0.
  <ext>-extra               = ''.
  <ext>-min_extract_version = 20.
  if support_unicode_names  = abap_true.
     <ext>-gen_flags        = gen_flags_unicode.
  else.
     <ext>-gen_flags        = 0.
  endif.

  IF <file>-size > 0.
*    <ext>-compressed        = 8. " gzip Deflate
    <ext>-compressed        = 0.
    <ext>-crc32             = crc32( content ).
*    <ext>-crc32             = 0.
    <ext>-content = content.

*    cl_abap_gzip=>compress_binary(
*      EXPORTING raw_in         = content
*                raw_in_len     = <file>-size
*                compress_level = compress_level
*      IMPORTING gzip_out     = <ext>-content
*                gzip_out_len = <ext>-compsize ).
  ELSE. " folder
    <ext>-compressed        = 0. " gzip Stored
    <ext>-crc32             = 0.
    <ext>-compsize          = 0.
  ENDIF.

ENDMETHOD.


  method ADD_HEADER.
   CONSTANTS:  magic_nr(4)  TYPE x VALUE 'EDB88320',
             mFFFFFFFF(4) TYPE x VALUE 'FFFFFFFF',
             m7FFFFFFF(4) TYPE x VALUE '7FFFFFFF',
             m00FFFFFF(4) TYPE x VALUE '00FFFFFF',
             m000000FF(4) TYPE x VALUE '000000FF',
             m000000(3)   TYPE x VALUE '000000'.
    data:    m0000(2) type x value '0000'.
    data: salt(12) type x value '34346F6A3388F7A91451C1CC'.
    data: v_crc16 type raw4,
          v_crc32 type raw4.
    data: v_salt(12) type x.
    data: slt type x, sltx type xstring.
    data: temp1 type raw4, temp2 type raw4, temp3 type raw4, temp4 type raw4, temp5 type raw4, temp3x type xstring, temp2i type int8.
    data: one type raw4, two type raw4.
    data: a type x.
    data: CRC32_MAP type xstring.
    data: key0tmp type raw4, key1tmp type raw4, key1i type int8, key0tmpi type int8, m8088405i type int8, key0b(1) type x.
    DATA: len TYPE i, n TYPE i. "#EC *
    DATA: x4(4) TYPE x, idx(4) TYPE x.
    DATA: crcstr type string.
    data: sltidx type i.

*    v_crc16 = crc.
*    v_crc16 = v_crc16 / 256.
     v_crc32 = crc.
    call method RIGHT_SHIFT_X exporting value = v_crc32 positions = 16 receiving returning = v_crc16.
    v_crc16 = v_crc16 / 256.
    move salt to v_salt.
*    move v_crc16+1(1) to v_salt+11(1).
    move v_crc16+3(1) to v_salt+11(1).

    IF XSTRLEN( crc32_map ) = 0.
    DO 256 TIMES.
      DATA: c(4) TYPE x, low_bit(4) TYPE x.
      c = sy-index - 1.
      DO 8 TIMES.
        low_bit = '00000001'. low_bit = c BIT-AND low_bit.   " c  & 1
        c = c DIV 2. c = c BIT-AND m7FFFFFFF. " c >> 1 (top is zero, but in ABAP signed!)
        IF low_bit IS NOT INITIAL.
          c = c BIT-XOR magic_nr.
        ENDIF.
      ENDDO.
      CONCATENATE crc32_map c INTO crc32_map IN BYTE MODE.
    ENDDO.
    ENDIF.
    do 12 times.
      sltidx = sy-index - 1.
      slt = v_salt+sltidx(1).
      one = 1.
      two = 2.
      temp1 = key2 bit-or two.
      temp2 = temp1 bit-xor one.
*     temp3 = temp1 * temp2i.
      call method multiply_x exporting a = temp1 b = temp2 receiving result = temp3.
*      temp4 = temp3 / 256.
      call method RIGHT_SHIFT_X exporting value = temp3 positions = 8 receiving returning = temp4.
      temp4 = temp4 BIT-AND m00FFFFFF.
      a = temp4+3(1).
      temp5 = slt bit-xor a.
      header+sltidx(1) = temp5(1).
      sltx = slt.
      call method update_keys exporting is_header = 'X' changing if_data = sltx key0 = key0 key1 = key1 key2 = key2 .

  enddo.

  endmethod.


  method ADD_X.
      data carry type x length 4.
      carry = a bit-and b.
      result = a bit-xor b.
      while carry <> 0.
        data shiftedcarry type x length 4.
        shiftedcarry = left_shift_x( value = carry positions = 1 ).
        carry = result bit-and shiftedcarry.
        result = result bit-xor shiftedcarry.
      endwhile.
  endmethod.


  method CONSTRUCTOR.
      append 2 to power_of_2.
  append 4 to power_of_2.
  append 8 to power_of_2.
  append 16 to power_of_2.
  append 32 to power_of_2.
  append 64 to power_of_2.
  append 128 to power_of_2.
  append 256 to power_of_2.
  append 512 to power_of_2.
  append 1024 to power_of_2.
  append 2048 to power_of_2.
  append 4096 to power_of_2.
  append 8192 to power_of_2.
  append 16384 to power_of_2.
  append 32768 to power_of_2.
  append 65536 to power_of_2.
  append 131072 to power_of_2.
  append 262144 to power_of_2.
  append 524288 to power_of_2.
  append 1048576 to power_of_2.
  append 2097152 to power_of_2.
  append 4194304 to power_of_2.
  append 8388608 to power_of_2.
  append 16777216 to power_of_2.
  append 33554432 to power_of_2.
  append 67108864 to power_of_2.
  append 134217728 to power_of_2.
  append 268435456 to power_of_2.
  append 536870912 to power_of_2.
  append 1073741824 to power_of_2.
  endmethod.


METHOD CRC32.

* Let us ask our friendly neighbour whether there is a CRC32 in the kernel (thanks guys!)
*  IF cl_http_utility=>is_ict_system_call_implemented( ihttp_scid_crc32_checksum ) IS INITIAL.
*    SYSTEM-CALL ict                        "#EC CI_SYSTEMCALL
*      DID
*        ihttp_scid_crc32_checksum
*      PARAMETERS
*        content                            " > xstr
*        crc32.                             " < unsigned int
*    RETURN.
*  ENDIF.

* Do the calculations by hand. This is going to be slow. This is going to be a pain.
* What is a man to do?

  CONSTANTS: magic_nr(4)  TYPE x VALUE 'EDB88320',
             mFFFFFFFF(4) TYPE x VALUE 'FFFFFFFF',
             m7FFFFFFF(4) TYPE x VALUE '7FFFFFFF',
             m00FFFFFF(4) TYPE x VALUE '00FFFFFF',
             m000000FF(4) TYPE x VALUE '000000FF',
             m000000(3)   TYPE x VALUE '000000'.

  IF XSTRLEN( crc32_map ) = 0.
    DO 256 TIMES.
      DATA: c(4) TYPE x, low_bit(4) TYPE x.
      c = sy-index - 1.
      DO 8 TIMES.
        low_bit = '00000001'. low_bit = c BIT-AND low_bit.   " c  & 1
        c = c DIV 2. c = c BIT-AND m7FFFFFFF. " c >> 1 (top is zero, but in ABAP signed!)
        IF low_bit IS NOT INITIAL.
          c = c BIT-XOR magic_nr.
        ENDIF.
      ENDDO.
      CONCATENATE crc32_map c INTO crc32_map IN BYTE MODE.
    ENDDO.
  ENDIF.

  DATA: len TYPE i, n TYPE i. "#EC *
  DATA: crc(4) TYPE x VALUE mFFFFFFFF, x4(4) TYPE x, idx(4) TYPE x.

  len = XSTRLEN( content ).
  DO len TIMES.
    n = sy-index - 1.
    CONCATENATE m000000 content+n(1) INTO idx IN BYTE MODE.
    idx = ( crc BIT-XOR idx ) BIT-AND m000000FF.
    idx = idx * 4.
    x4  = crc32_map+idx(4).
    crc = crc DIV 256. crc = crc BIT-AND m00FFFFFF. " c >> 8
    crc = x4 BIT-XOR crc.
  ENDDO.
  crc = crc BIT-XOR mFFFFFFFF.

  crc32 = crc.

ENDMETHOD.


METHOD DELETE.

  IF index = 0.
    READ TABLE files TRANSPORTING NO FIELDS WITH KEY name = name.
    IF sy-subrc IS NOT INITIAL.
      RAISE zip_index_error.  "#EC RAISE_OK
    ENDIF.
    index = sy-tabix.
  ENDIF.

  IF index < 1 OR index > LINES( files ).
    RAISE zip_index_error.  "#EC RAISE_OK
  ENDIF.

  DELETE files INDEX index.
  DELETE exts  INDEX index.

ENDMETHOD.


METHOD GET.

  FIELD-SYMBOLS: <ext> TYPE t_ext.

  IF index IS INITIAL.
    READ TABLE files TRANSPORTING NO FIELDS WITH KEY name = name.
    IF sy-subrc IS NOT INITIAL.
      RAISE zip_index_error.  "#EC RAISE_OK
    ENDIF.
    index = sy-tabix.
  ENDIF.

  IF index < 1 OR index > LINES( files ).
    RAISE zip_index_error.  "#EC RAISE_OK
  ENDIF.

  READ TABLE exts INDEX index ASSIGNING <ext>.

  IF <ext>-compressed IS INITIAL.
    content = <ext>-content.
  ELSE.
    try.
      cl_abap_gzip=>decompress_binary(
        EXPORTING gzip_in     = <ext>-content
                  gzip_in_len = <ext>-compsize
        IMPORTING raw_out     = content ).
   catch cx_parameter_invalid_range
         cx_sy_buffer_overflow
         cx_sy_compression_error.
     raise zip_decompression_error.
   endtry.
  ENDIF.

  IF crc32( content ) <> <ext>-crc32.
    RAISE zip_decompression_error.  "#EC RAISE_OK
  ENDIF.

ENDMETHOD.


  method LEFT_SHIFT_X.
     data positions_to_shift type i.
  positions_to_shift = positions mod 32.
  if positions_to_shift > 0.
    data result type x length 4.
    try.
        " First try regular multiplication
        data a type i.
        read table power_of_2 into a index positions_to_shift.
        result = value * a.
      catch cx_sy_arithmetic_overflow.
        " Overflow occured, perform bitwise multiplication
        data calc_value type x length 4.
        calc_value = value.
        data b type x length 4.
        do positions_to_shift times.
          b = calc_value bit-and h_40000000.
          calc_value = calc_value bit-and h_3fffffff.
          calc_value = calc_value * 2.
          if b <> 0.
            calc_value = calc_value bit-or h_80000000.
          endif.
        enddo.
        result = calc_value.
    endtry.
    returning = result.
  else.
    returning = value.
  endif.
  endmethod.


METHOD LOAD.

* Documentation from: http://www.pkware.com/company/standards/appnote/appnote.txt

* Start to decode new ZIP file
  CLEAR:   files, exts.
  REFRESH: files, exts.

* Global offset for moving through file
  DATA: offset  TYPE i.
  DATA: l_temp_offset TYPE I.
  DATA: l_current_local_header type I.


  DEFINE next.   " move offset
    offset = offset + &1.
  END-OF-DEFINITION.

* DATA: l1(1) TYPE x, h1(1) TYPE x, l2(1) TYPE x, h2(1) TYPE x, xstr TYPE xstring.
  DATA: w2(2) TYPE x, w4(4) TYPE x, xstr TYPE xstring.
  DEFINE read2.  " read two bytes as integer and move offset
*    l1 = zip+offset(1). offset = offset + 1.
*    h1 = zip+offset(1). offset = offset + 1.
*    CONCATENATE h1 l1 INTO xstr IN BYTE MODE.
    w2     = zip+offset(2).
    offset = offset + 2.
    CONCATENATE w2+1(1) w2+0(1) INTO xstr IN BYTE MODE.
    &1     = xstr.
  END-OF-DEFINITION.

  DEFINE read4.  " read four bytes as integer and move offset
*    l1 = zip+offset(1). offset = offset + 1.
*    h1 = zip+offset(1). offset = offset + 1.
*    l2 = zip+offset(1). offset = offset + 1.
*    h2 = zip+offset(1). offset = offset + 1.
*    CONCATENATE h2 l2 h1 l1 INTO xstr IN BYTE MODE.
    w4     = zip+offset(4).
    offset = offset + 4.
    CONCATENATE w4+3(1) w4+2(1) w4+1(1) w4+0(1) INTO xstr IN BYTE MODE.
    &1     = xstr.
  END-OF-DEFINITION.

  CONSTANTS: gen_flags_encrypted(2) TYPE x VALUE '0001',
             gen_flags_data_descriptor(2) TYPE x VALUE '0008', " general purpose flag bit 3
             gen_flags_unicode(2)   TYPE x VALUE '0800'. " general purpose flag bit 11
  DATA:      gen_flags(2) TYPE x.

* We convert all names from xstring into string
* see: http://www.pkware.com/documents/casestudies/APPNOTE.TXT, APPENDIX D
* zip normaly used IBM Code Page 437 mapped to SAP Printer EPESCP IBM 437
  DATA: conv  TYPE REF TO cl_abap_conv_in_ce,
        conv_cp437 TYPE REF TO cl_abap_conv_in_ce,
        conv_utf8  TYPE REF TO cl_abap_conv_in_ce,
        cp437      TYPE abap_encoding value '1107', " IBM 437
        utf8       TYPE abap_encoding value '4110'. " UTF-8

  conv_cp437 = cl_abap_conv_in_ce=>create( encoding = cp437
                                        ignore_cerr = abap_true
                                        replacement = '#' ).
  conv_utf8  = cl_abap_conv_in_ce=>create( encoding = utf8
                                        ignore_cerr = abap_true
                                        replacement = '#' ).

* The maximum length of the ZIP file for scanning.
  DATA: max_length TYPE i.
  max_length = XSTRLEN( zip ) - 4.

* Extract information about all files.
  DATA: msdos_date TYPE i, msdos_time TYPE i, file_no TYPE i VALUE 0.
  FIELD-SYMBOLS:  <file> TYPE t_file,
                  <ext>  TYPE t_ext.

* strip 0000 buffer from some zips
  WHILE offset < max_length AND zip+offset(1) = '00'.
    next 1.
  ENDWHILE.

  WHILE offset < max_length AND zip+offset(4) = '504B0304'.  " local file header signature

    l_current_local_header = offset.
    file_no = file_no + 1.
    APPEND INITIAL LINE TO files ASSIGNING <file>.
    APPEND INITIAL LINE TO exts  ASSIGNING <ext>.

    next  4.                          " local file header signature
    read2 <ext>-min_extract_version.  " version needed to extract = 2.0 - File is compressed using Deflate
    read2 <ext>-gen_flags.            " general purpose bit flag
    read2 <ext>-compressed.           " compression method: deflated
    read2 msdos_time.                 " last mod file time
    read2 msdos_date.                 " last mod file date
    read4 <ext>-crc32.                " crc-32
    read4 <ext>-compsize.             " compressed size
    read4 <file>-size.                " uncompressed size
    read2 <ext>-filename_len.         " file name length
    read2 <ext>-extra_len.            " extra field length

    gen_flags = <ext>-gen_flags.
    gen_flags = gen_flags BIT-AND gen_flags_unicode. " bit 11: Language encoding flag
    if gen_flags <> 0 AND support_unicode_names = abap_true.
       conv = conv_utf8.  " utf-8 filename extension
    else.
       conv = conv_cp437. " IBM CP437
    endif.

    <ext>-filename = zip+offset(<ext>-filename_len).
    conv->convert( EXPORTING input = <ext>-filename IMPORTING data = <file>-name ).
    next <ext>-filename_len.
    try.
        <ext>-extra = zip+offset(<ext>-extra_len).
      catch cx_sy_range_out_of_bounds.
        raise zip_parse_error.
    endtry.
    next <ext>-extra_len.

    gen_flags = <ext>-gen_flags.
    gen_flags = gen_flags BIT-AND gen_flags_data_descriptor. " bit 3: Data Descriptor
    IF gen_flags = 0.

      try.
          <ext>-content = zip+offset(<ext>-compsize).
        catch cx_sy_range_out_of_bounds.
          raise zip_parse_error.
      endtry.
      next <ext>-compsize.

    ELSE.

      DATA   result_tab TYPE match_result_tab.
      FIELD-SYMBOLS <match> LIKE LINE OF result_tab.
      FIND ALL OCCURRENCES OF <ext>-filename IN zip RESULTS result_tab IN BYTE MODE.
* --- start of modification:
* The following modification was necessary to handle zip-archives containing files
* where the name of one file is a sub-string of the name of another file

* --- deleted code:
*     Loop till the end of the result_tab to get the entry from the Central Directory
*      LOOP at result_tab ASSIGNING <match>.
*      ENDLOOP .
*      DATA: cached_offset TYPE i. cached_offset = offset. offset = <match>-offset - 30.

      DATA: cached_offset TYPE I. cached_offset = offset.
      DATA: l_filename_length TYPE I.
      SORT result_tab BY offset DESCENDING.
      LOOP AT result_tab ASSIGNING <match>.
        " verify <match>-offset: correct record should match signature and filename length
        offset = <match>-offset - 46.
        check offset > 0.
        if zip+offset(4) <> '504B0102'. " central directory header record's signature
          continue.
        endif.
        ADD 42 TO offset.
        read4 l_temp_offset.
        IF l_temp_offset = l_current_local_header.
          EXIT.
        ENDIF.
      ENDLOOP .

      offset = <match>-offset - 30.
* --- end of modification


      read4 <ext>-crc32.
      read4 <ext>-compsize.
      read4 <file>-size.
      next 18.
      offset = cached_offset.
      try.
          <ext>-content = zip+offset(<ext>-compsize).
        catch cx_sy_range_out_of_bounds.
          raise zip_parse_error.
      endtry.
      next <ext>-compsize.
      next 16. " I032850

    ENDIF.

    <file>-time = msdos=>from_time( msdos_time ).
    <file>-date = msdos=>from_date( msdos_date ).

    gen_flags = <ext>-gen_flags.
    gen_flags = gen_flags BIT-AND gen_flags_encrypted.

    if check_header = abap_true and NOT ( <ext>-min_extract_version <= 20 ).
       RAISE zip_parse_error.  "#EC RAISE_OK
    endif.

    IF  ( gen_flags = gen_flags_encrypted )
    OR NOT ( <ext>-compressed = 0 OR <ext>-compressed = 8 ).
      RAISE zip_parse_error.  "#EC RAISE_OK
    ENDIF.

*   strip 0000 buffer from some zips
    WHILE offset < max_length AND zip+offset(1) = '00'.
      next 1.
    ENDWHILE.

  ENDWHILE.

ENDMETHOD.


  method MULTIPLY_X.
      data calc_a type x length 4.
      data calc_b type x length 4.
      calc_a = a.
      calc_b = b.
      result = 0.
      while calc_b <> 0.
        data calc_c type x length 4.
        calc_c = calc_b bit-and h_00000001.
        if calc_c <> 0.
          result = add_x( a = result b = calc_a ).
        endif.
        calc_a = left_shift_x( value = calc_a positions = 1 ).
        calc_b = unsigned_right_shift_x( value = calc_b positions = 1 ).
      endwhile.

  endmethod.


  method RIGHT_SHIFT_X.
      data positions_to_shift type i.
  positions_to_shift = positions mod 32.
  if positions_to_shift = 31.
    if value < 0.
      returning = -1.
      return.
    else.
      returning = 0.
      return.
    endif.
  elseif positions_to_shift > 0.
    data a type i.
    read table power_of_2 into a index positions_to_shift.
    returning = value div a.
  else.
    returning = value.
  endif.
  endmethod.


METHOD SAVE.

* Documentation from: http://www.pkware.com/company/standards/appnote/appnote.txt

  DATA: x2(2) TYPE x, x4(4) TYPE x, x1(1) type x.

  DEFINE writeX4.    " write xstring
    x4 = &2.
    CONCATENATE &1 x4 INTO &1 IN BYTE MODE.
  END-OF-DEFINITION.

   DEFINE writeX2.    " write xstring
    x2 = &2.
    CONCATENATE &1 x2 INTO &1 IN BYTE MODE.
  END-OF-DEFINITION.

   DEFINE writeX1.    " write xstring
    x1 = &2.
    CONCATENATE &1 x1 INTO &1 IN BYTE MODE.
  END-OF-DEFINITION.


  DEFINE write2.  " write two bytes from integer
    x2 = &2.
    CONCATENATE &1 x2+1(1) x2+0(1) INTO &1 IN BYTE MODE.
  END-OF-DEFINITION.

  DEFINE write4.  " write four bytes from integer
    x4 = &2.
    CONCATENATE &1 x4+3(1) x4+2(1) x4+1(1) x4+0(1) INTO &1 IN BYTE MODE.
  END-OF-DEFINITION.

* Process all files. We write in parallel the zip and the central directory to use later
  DATA: msdos_date TYPE i, msdos_time TYPE i.
  FIELD-SYMBOLS:  <file>  TYPE t_file,
                  <ext>   TYPE t_ext.
  DATA: dir TYPE xstring, start_offset(4) TYPE x.

*enc/dec header
  data: v_password type xstring.
  data: v_crc16(2) type x, v_crci type i, v_crc32i type i.
  data: v_key0 type raw4 value '12345678', v_key1 type raw4 value '23456789', v_key2 type raw4 value '34567890'.
  data: v_header(12) type x.
  data: xpwd type xstring.

  if if_password is not initial.
     CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
       EXPORTING
         text = if_password " variable type string
       IMPORTING
        buffer = v_password. " variable type xstring

*erdata do hlavicky
   endif.

  LOOP AT files ASSIGNING <file>.
    READ TABLE exts INDEX sy-tabix ASSIGNING <ext>.
    start_offset = XSTRLEN( zip ).

    msdos_time = msdos=>to_time( <file>-time ).
    msdos_date = msdos=>to_date( <file>-date ).

*enc file data
  if if_password is not initial.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
       EXPORTING
        text = if_password
       IMPORTING
     buffer = xpwd.
*    move <ext>-crc32(2) to v_crc16.
    v_crc32i = <ext>-crc32.
    call method update_keys exporting is_header = 'X' changing if_data = xpwd key0 = v_key0 key1 = v_key1 key2 = v_key2 .
    call method add_header exporting crc = v_crc32i changing key0 = v_key0 key1 = v_key1 key2 = v_key2 receiving header = v_header.
    call method update_keys changing if_data = <ext>-content key0 = v_key0 key1 = v_key1 key2 = v_key2 .
   endif.

*   zip data stream
    writeX4  zip '504B0304'.                  " local file header signature
    write2   zip <ext>-min_extract_version.   " version needed to extract = 2.0 - File is compressed using Deflate
    if if_password is not initial.
      <ext>-gen_flags = 1.
    endif.
    write2   zip <ext>-gen_flags.             " general purpose bit flag
    write2   zip <ext>-compressed.            " compression method: deflated
    write2   zip msdos_time.                  " last mod file time
    write2   zip msdos_date.                  " last mod file date
    write4   zip <ext>-crc32.                 " crc-32
*    write4   zip '00000000'.
    if if_password is not initial.
      <ext>-compsize = <file>-size + 12.
    endif.
    write4   zip <ext>-compsize.              " compressed size
    write4   zip <file>-size.                 " uncompressed size
    write2   zip <ext>-filename_len.          " file name length
    write2   zip <ext>-extra_len.             " extra field length

    if if_password is not initial.
      CONCATENATE zip <ext>-filename <ext>-extra v_header <ext>-content INTO zip IN BYTE MODE.
    else.
      CONCATENATE zip <ext>-filename <ext>-extra <ext>-content INTO zip IN BYTE MODE.
    endif.
*   central directory stream (which has a lare duplicate sequence of zip header)
    DATA: dup_offset TYPE i. dup_offset = start_offset + 4.
    writeX4  dir '504B0102'.                  " central file header signature
    write2   dir 19.                          " version made by (== pkzip 2.04g)
    CONCATENATE dir zip+dup_offset(26) INTO dir IN BYTE MODE.  " part which matches exactly zip header
    write2   dir  0.                          " file comment length
    write2   dir  0.                          " disk number start
    write2   dir  0.                          " internal file attributes
    write4   dir  0.                          " external file attributes
    write4   dir  start_offset.               " relative offset of local header
    CONCATENATE dir <ext>-filename  <ext>-extra INTO dir IN BYTE MODE.  " file name + extra info

  ENDLOOP.

* Write Central Directory
  DATA: lines_files TYPE i.    lines_files = LINES( files ).
  DATA: xstrlen_dir TYPE i.    xstrlen_dir = XSTRLEN( dir ).
  DATA: offset_dir  TYPE i.    offset_dir  = XSTRLEN( zip ).

  CONCATENATE zip dir INTO zip IN BYTE MODE.
  writeX4  zip '504B0506'.                    " End of central directory
  write2   zip  0.                            " number of this disk
  write2   zip  0.                            " number of the disk with the start of the central directory
  write2   zip  lines_files.                  " total number of entries in the central directory on this disk
  write2   zip  lines_files.                  " total number of entries in the central directory
  write4   zip  xstrlen_dir.                  " size of the central directory
  write4   zip  offset_dir.                   " offset of start of central directory
  write2   zip  0.                            " ZIP file comment length

ENDMETHOD.


METHOD SPLICE.

* ZIP format: http://www.pkware.com/company/standards/appnote/appnote.txt

  data: w2(2) type x, w4(4) type x, xstr type xstring.
  define read2.   "#EC NEEDED read two bytes as integer and move offset
    w2     = zip+offset(2).
    offset = offset + 2.
    concatenate w2+1(1) w2+0(1) into xstr in byte mode.
    &1     = xstr.
  end-of-definition.

  define read4.  "#EC NEEDED read four bytes as integer and move offset
    w4     = zip+offset(4).
    offset = offset + 4.
    concatenate w4+3(1) w4+2(1) w4+1(1) w4+0(1) into xstr in byte mode.
    &1     = xstr.
  end-of-definition.

  DEFINE next.   " move offset
    offset = offset + &1.
  END-OF-DEFINITION.

* Global offset for moving through file
  data: offset  type i.
* The maximum length of the ZIP file for scanning.
  data: max_length type i.

  field-symbols:
        <entry>      like line of entries.
  data: filename_len type i,
        extra_len    type i,
        filename     type xstring.

  constants: gen_flags_data_descriptor(2) type x value '0008', " general purpose flag bit 3
             gen_flags_unicode(2) type x value '0800', " general purpose flag bit 11
             gen_flags_null(2) type x value '0000'.

  data:      gen_flags(2) type x.

  data   result_tab type match_result_tab.
  field-symbols <match> like line of result_tab.
  data: cached_offset type i.
  data: l_filename_length type i.

* We convert all filenames from xstring into string
* zip normally uses IBM Code Page 437 mapped to SAP Printer EPESCP IBM 437
* utf-8 is supported and detected automatically
  data: conv  type ref to cl_abap_conv_in_ce,
        conv_cp437 type ref to cl_abap_conv_in_ce,
        conv_utf8  type ref to cl_abap_conv_in_ce,
        cp437      type abap_encoding value '1107', " IBM 437
        utf8       type abap_encoding value '4110'. " UTF-8

  DATA: l_temp_offset TYPE I.
  DATA: l_current_local_header type I.

  conv_cp437 = cl_abap_conv_in_ce=>create( encoding = cp437
                                        ignore_cerr = abap_true
                                        replacement = '#' ).
  conv_utf8  = cl_abap_conv_in_ce=>create( encoding = utf8
                                     ignore_cerr = abap_true
                                     replacement = '#' ).

* Start to decode new ZIP file
  clear:   entries, splice_rc.
  refresh: entries.
  if xstrlen( zip ) <= 4. splice_rc = 4. return. endif.
  max_length = xstrlen( zip ) - 4.

* strip 0000 buffer from some zips
  while offset < max_length and zip+offset(1) = '00'.
    next 1.
  endwhile.

* Extract information about all files.
  while zip+offset(4) = '504B0304'.  " local file header signature

    l_current_local_header = offset.
    append initial line to entries assigning <entry>.

    offset = offset + 6.       " next 4=(header). read2 <ext>-min_extract_version.
    read2 gen_flags.
    read2 <entry>-compressed.  " compression method: deflated
    offset = offset + 8.       " read2 msdos_time. read2 msdos_date. read4 <ext>-crc32.
    read4 <entry>-length.      " compressed size
    offset = offset + 4.       " uncompressed size
    read2 filename_len.        " file name length
    read2 extra_len.           " extra field length

*   check encoding of filename
    if ( gen_flags bit-and gen_flags_unicode ) <> gen_flags_null and support_unicode = abap_true.
      conv = conv_utf8.  " utf-8 filename extension
    else.
      conv = conv_cp437. " IBM CP437
    endif.

    filename = zip+offset(filename_len).
    conv->convert( exporting input = filename importing data = <entry>-name ).
    next: filename_len, extra_len.
    <entry>-offset = offset.

*   check data descriptor flag
    if ( gen_flags bit-and gen_flags_data_descriptor ) <> gen_flags_null.
*     correct size must be read from central directory entry
      cached_offset = offset.
      find all occurrences of filename in zip results result_tab in byte mode.
      sort result_tab by offset descending.
      loop at result_tab assigning <match>.
        offset = <match>-offset - 18.
"        read2 l_filename_length.
"        if l_filename_length = xstrlen( filename ).
"          exit.
"        endif.
        ADD 14 TO offset.
        read4 l_temp_offset.
        IF l_temp_offset = l_current_local_header.
          EXIT.
        ENDIF.
      endloop .

      offset = <match>-offset - 26.
      read4 <entry>-length.

*     reset position to local file header and data area
      offset = cached_offset.
      next <entry>-length.

*     skip data descriptor
      try.
          if zip+offset(4) = '504B0708'. "optional signature field
            next 16.
          else.
            next 12.
          endif.
        catch cx_sy_range_out_of_bounds.
          splice_rc = 4.
          clear <entry>.
          exit.
      endtry.
    else.
      next <entry>-length.
    endif.

*   strip 0000 buffer from some zips
    while offset < max_length and zip+offset(1) = '00'.
      next 1.
    endwhile.

  endwhile.

  delete entries where length = 0.

ENDMETHOD.


  method UNSIGNED_RIGHT_SHIFT_X.
      data positions_to_shift type i.
  positions_to_shift = positions mod 32.
  if positions_to_shift > 0.
    data calc_value type x length 4.
    data a type x length 4.
    data b type x length 4.
    calc_value = value.
    a = calc_value bit-and h_7fffffff.
    a = right_shift_x( value = a positions = positions_to_shift ).
    b = calc_value bit-and h_80000000.
    b = right_shift_x( value = b positions = positions_to_shift ).
    returning = a - b.
  else.
    returning = value.
  endif.
  endmethod.


method UPDATE_KEYS.
  CONSTANTS:  magic_nr(4)  TYPE x VALUE 'EDB88320',
             mFFFFFFFF(4) TYPE x VALUE 'FFFFFFFF',
             m7FFFFFFF(4) TYPE x VALUE '7FFFFFFF',
             m00FFFFFF(4) TYPE x VALUE '00FFFFFF',
             m000000FF(4) TYPE x VALUE '000000FF',
             m000000(3)   TYPE x VALUE '000000',
             m8088405(4) type x value '08088405'.

 data: key0tmp type raw4, key1tmp type raw4, key1i type int8, key0tmpi type int8, m8088405i type int8, key0b(1) type x.
 data: CRC32_MAP type xstring.
 data: datax type x.
 data: temp1 type raw4, temp2 type raw4, temp3 type raw4, temp4 type raw4, temp5 type raw4, temp3x type xstring, temp2i type int8.
 data: one type raw4, two type raw4.
 data: a type x.
 data: if_dataout type xstring.
 data: key0init type raw4, key1init type raw4, key2init type raw4.

  IF XSTRLEN( crc32_map ) = 0.
    DO 256 TIMES.
      DATA: c(4) TYPE x, low_bit(4) TYPE x.
      c = sy-index - 1.
      DO 8 TIMES.
        low_bit = '00000001'. low_bit = c BIT-AND low_bit.   " c  & 1
        c = c DIV 2. c = c BIT-AND m7FFFFFFF. " c >> 1 (top is zero, but in ABAP signed!)
        IF low_bit IS NOT INITIAL.
          c = c BIT-XOR magic_nr.
        ENDIF.
      ENDDO.
      CONCATENATE crc32_map c INTO crc32_map IN BYTE MODE.
    ENDDO.
  ENDIF.

  DATA: len TYPE i, n TYPE i. "#EC *
  DATA: crc(4) TYPE x, x4(4) TYPE x, idx(4) TYPE x.
  DATA: crcstr type string.

  len = XSTRLEN( if_data ).
  DO len TIMES.
    n = sy-index - 1.
    move if_data+n(1) to datax.
    if is_header ne 'X'.
      one = 1.
      two = 2.
      temp1 = key2 bit-or two.
      temp2 = temp1 bit-xor one.
*     temp3 = temp1 * temp2i.
      call method multiply_x exporting a = temp1 b = temp2 receiving result = temp3.
*      temp4 = temp3 / 256.
      call method RIGHT_SHIFT_X exporting value = temp3 positions = 8 receiving returning = temp4.
      temp4 = temp4 BIT-AND m00FFFFFF.
      a = temp4+3(1).
      temp5 = datax bit-xor a.
      concatenate if_dataout temp5(1) into if_dataout in byte mode.
    endif.
    CONCATENATE m000000 datax INTO idx IN BYTE MODE.
    idx = ( key0 BIT-XOR idx ) BIT-AND m000000FF.
    idx = idx * 4.
    x4  = crc32_map+idx(4).
    key0 = key0 DIV 256. key0 = key0 BIT-AND m00FFFFFF. " c >> 8
    key0 = x4 BIT-XOR key0.
    key0tmp = key0.
  crcstr = key0.
  write: / 'key0', crcstr.
    key0tmp = key0tmp BIT-AND m000000FF.
    key0tmpi = key0tmp.
    key0b = key0tmpi.
    key1i = key1.
    key1i = key1i + key0tmpi.
    key1 = key1i.
    m8088405i = 134775813.
*    key1 = key1 * m8088405 + 1.
    key1i = key1 * m8088405i + 1.
    key1 = key1i.
*    call method multiply_x exporting a = key1 b = m8088405 receiving result = key1.
  crcstr = key1.
  write: / 'key1', crcstr.
    call method RIGHT_SHIFT_X exporting value = key1 positions = 24 receiving returning = key1tmp.
*    key1tmp = key1 / 16777216.
    CONCATENATE m000000 key1tmp+3(1) INTO idx IN BYTE MODE.
    idx = ( key2 BIT-XOR idx ) BIT-AND m000000FF.
    idx = idx * 4.
    x4  = crc32_map+idx(4).
    key2 = key2 DIV 256. key2 = key2 BIT-AND m00FFFFFF. " c >> 8
    key2 = x4 BIT-XOR key2.
  crcstr = key2.
  write: / 'key2', crcstr.
  ENDDO.
  if is_header ne 'X'.
    move if_dataout to if_data.
  endif.
endmethod.
ENDCLASS.
