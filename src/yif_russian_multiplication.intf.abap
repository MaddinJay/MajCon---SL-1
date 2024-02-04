INTERFACE yif_russian_multiplication
  PUBLIC .
  METHODS:
    calculate
      IMPORTING
                iv_value_x    TYPE int4
                iv_value_y    TYPE int4
      RETURNING VALUE(rv_sum) TYPE int4.
ENDINTERFACE.
