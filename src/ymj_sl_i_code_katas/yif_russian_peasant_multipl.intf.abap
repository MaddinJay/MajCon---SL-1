INTERFACE yif_russian_peasant_multipl
  PUBLIC .
  METHODS multiplicate IMPORTING iv_number_one    TYPE int4
                                 iv_number_two    TYPE int4
                       RETURNING VALUE(rv_result) TYPE int4
                       RAISING
                         ycx_russian_peasant_multipl.

  METHODS print IMPORTING iv_number TYPE int4.
ENDINTERFACE.
