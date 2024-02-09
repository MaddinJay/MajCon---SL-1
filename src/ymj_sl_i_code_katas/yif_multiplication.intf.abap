INTERFACE yif_multiplication
  PUBLIC .
  METHODS multiplicate IMPORTING number_one       TYPE int4
                                 number_two       TYPE int4
                       RETURNING VALUE(rv_result) TYPE int4
                       RAISING   ycx_russian_peasant_multipl.
ENDINTERFACE.
