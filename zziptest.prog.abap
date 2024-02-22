REPORT ZZIPTEST.
    DATA: lo_abap_zip TYPE REF TO zcl_abap_zip.
    DATA: lv_in                TYPE xstring,
          lv_file_name          TYPE string,
          lv_zip_file           TYPE xstring.
    DATA: passwd type string value 'xxxxxxxx'.
    data: dsn(60).
    data: lv_crc32 type i.


    move 'in.xxx' to dsn.
    move dsn to lv_file_name.
    open dataset dsn for input in binary mode.
    read dataset dsn into lv_in.
    close dataset dsn.

    CREATE OBJECT lo_abap_zip.
           lo_abap_zip->add(
          EXPORTING
            name           = lv_file_name
            content        = lv_in
        ).

lv_zip_file = lo_abap_zip->save(
         EXPORTING
           if_password = passwd
        ).

move 'out.xxx' to dsn.
open dataset dsn for output in binary mode.
 transfer lv_zip_file to dsn.
close dataset dsn.
