CLASS ycl_hash_tables DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF voter,
             name TYPE string,
           END OF voter.
    TYPES tt_voter TYPE HASHED TABLE OF voter WITH UNIQUE KEY name. "Immer eindeutiger Schlüssel
    METHODS check_voter IMPORTING name          TYPE string
                        RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA voter_list TYPE tt_voter.

    METHODS is_in_list IMPORTING name          TYPE string
                       RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS YCL_HASH_TABLES IMPLEMENTATION.


  METHOD check_voter.
    result = COND #( WHEN is_in_list( name ) = abap_false THEN 'Let them vote!'
                     ELSE 'Kick them out!' ).
  ENDMETHOD.


  METHOD is_in_list.
    TRY.
        result = xsdbool( voter_list[ name = name ] IS NOT INITIAL ).
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE voter( name = name ) INTO TABLE voter_list. " Add name to Hash Table (converts name to index)
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
