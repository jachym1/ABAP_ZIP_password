
CLASS msdos DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:   to_date IMPORTING       date TYPE d RETURNING value(msdos_date) TYPE i.
    CLASS-METHODS:   to_time IMPORTING       time TYPE t RETURNING value(msdos_time) TYPE i.
    CLASS-METHODS: from_date IMPORTING msdos_date TYPE i RETURNING value(date)       TYPE d.
    CLASS-METHODS: from_time IMPORTING msdos_time TYPE i RETURNING value(time)       TYPE t.
ENDCLASS.
