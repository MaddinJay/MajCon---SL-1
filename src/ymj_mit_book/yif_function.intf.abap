INTERFACE yif_function
  PUBLIC .
  METHODS g IMPORTING x             TYPE f
                  RETURNING VALUE(result) TYPE f.
ENDINTERFACE.
