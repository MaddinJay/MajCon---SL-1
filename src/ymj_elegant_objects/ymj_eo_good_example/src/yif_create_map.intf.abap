INTERFACE yif_create_map
  PUBLIC .
  METHODS create IMPORTING geo_data      TYPE REF TO yif_geo_data " Decorator pattern -> create surface + calibrate
                           map_size      TYPE yif_map=>size
                           spy           TYPE REF TO yif_spy
                 RETURNING VALUE(result) TYPE REF TO yif_map.

ENDINTERFACE.
