
CLASS msdos IMPLEMENTATION.


  METHOD from_date. " IMPORTING msdos_date TYPE i RETURNING value(date) TYPE d

*   MS-DOS format for date:
*     Bits 15:9 = year - 1980
*     Bits 8:5 = month of year
*     Bits 4:0 = day of month

    CONSTANTS: mFE00(2) TYPE x VALUE 'FE00',
               m01E0(2) TYPE x VALUE '01E0',
               m007F(2) TYPE x VALUE '007F',
               m001F(2) TYPE x VALUE '001F',
               m000F(2) TYPE x VALUE '000F'.

    DATA: x(2)  TYPE x,
          year  TYPE i,
          month TYPE i,
          day   TYPE i,
          c4(4) TYPE c,
          str   TYPE string.

*   Bits 15:9 = year - 1980
    x     = msdos_date.
    x     = x BIT-AND mFE00.
    x     = x DIV 512. " >> 9
    x     = x BIT-AND m007F.
    year  = x.
    year  = year + 1980.
    WRITE year TO c4 USING EDIT MASK 'RR____'.
    CONCATENATE str c4 INTO str.

*     Bits 8:5 = month of year
    x     = msdos_date.
    x     = x BIT-AND m01E0.
    x     = x DIV 32. " >> 5
    x     = x BIT-AND m000F.
    month = x.
    WRITE month TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*     Bits 4:0 = day of month
    x     = msdos_date.
    x     = x BIT-AND m001F.
    day   = x.
    WRITE day TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Build date
    TRANSLATE str USING ' 0'.
    date = str.

  ENDMETHOD.


  METHOD from_time. " IMPORTING msdos_time TYPE i RETURNING value(time) TYPE t.

*   MS-DOS format for time:
*     Bits 15:11 = hour   (24-hour clock)
*     Bits 10:5 = minute
*     Bits 4:0 = second/2

    CONSTANTS: mF100(2) TYPE x VALUE 'F100',
               m07E0(2) TYPE x VALUE '07E0',
               m003F(2) TYPE x VALUE '003F',
               m001F(2) TYPE x VALUE '001F'.

    DATA: x(2)  TYPE x,
          hour  TYPE i,
          min   TYPE i,
          c4(4) TYPE c,
          str   TYPE string.

*   Bits 15:11 = hour (24-hour clock)
    x     = msdos_time.
    x     = x BIT-AND mF100.
    x     = x DIV 2048. " >> 11
    x     = x BIT-AND m001F.
    hour  = x.
    WRITE hour TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Bits 10:5 = minute
    x     = msdos_time.
    x     = x BIT-AND m07E0.
    x     = x DIV 32. " >> 5
    x     = x BIT-AND m003F.
    min   = x.
    WRITE min TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Bits 4:0 = second/2
    CONCATENATE str '00' INTO str.

*   Build time
    TRANSLATE str USING ' 0'.
    time = str.

  ENDMETHOD.


  METHOD to_date. " IMPORTING date TYPE d RETURNING value(msdos_date) TYPE i.

*   MS-DOS format for date:
*     Bits 15:9 = year - 1980
*     Bits 8:5 = month of year
*     Bits 4:0 = day of month

    DATA: xdate(2) TYPE x,
          x(2)     TYPE x,
          year     TYPE i,
          month    TYPE i,
          day      TYPE i.

*   Bits 15:9 = year - 1980
    year  = date+0(4).
    x     = year - 1980.
    x     = x * 512. " << 9
    xdate = xdate BIT-OR x.

*   Bits 8:5 = month of year
    month = date+4(2).
    x     = month.
    x     = x * 32. " << 5
    xdate = xdate BIT-OR x.

*   Bits 4:0 = day of month
    day   = date+6(2).
    x     = day.
    xdate = xdate BIT-OR x.

    msdos_date = xdate.

  ENDMETHOD.


  METHOD to_time. " IMPORTING time TYPE t RETURNING value(msdos_time) TYPE i.

*   MS-DOS format for time:
*     Bits 15:11 = hour   (24-hour clock)
*     Bits 10:5 = minute
*     Bits 4:0 = second/2

    DATA: xtime(2) TYPE x,
          x(2)     TYPE x,
          hour     TYPE i,
          min      TYPE i,
          sec      TYPE i.

*   Bits 15:11 = hour (24-hour clock)
    hour  = time+0(2).
    x     = hour.
    x     = x * 2048. " << 11
    xtime = xtime BIT-OR x.

*   Bits 10:5 = minute
    min   = time+2(2).
    x     = min.
    x     = x * 32. " << 5
    xtime = xtime BIT-OR x.

*   Bits 4:0 = seconds
    sec   = time+4(2).
    x     = sec / 2.
    xtime = xtime BIT-OR x.

    msdos_time = xtime.

  ENDMETHOD.


ENDCLASS.
