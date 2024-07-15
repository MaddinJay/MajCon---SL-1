CLASS ycl_binary_search DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS process IMPORTING list          TYPE string_t
                              item          TYPE string
                    RETURNING VALUE(result) TYPE int4.
ENDCLASS.

CLASS ycl_binary_search IMPLEMENTATION.

  METHOD process.
    DATA(low)  = 1.
    DATA(high) = lines( list ).

    WHILE low <= high.
      DATA(mid) = (  low + high ) / 2.
      DATA(guess) = list[ mid ].

      IF guess = item.
        result = mid.
        RETURN.
      ENDIF.

      IF guess > item.
        high = mid - 1.
      ENDIF.

      IF guess <= item.
        low = mid + 1.
      ENDIF.
    ENDWHILE.
    result = -1.

  ENDMETHOD.

ENDCLASS.
