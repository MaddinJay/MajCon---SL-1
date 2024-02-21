INTERFACE yif_column_right
  PUBLIC .
  METHODS double IMPORTING is_countable TYPE abap_bool.
  METHODS sum_up RETURNING VALUE(result) TYPE int4.
ENDINTERFACE.
