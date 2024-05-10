CLASS ycl_geo_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_geo_data.

    METHODS constructor IMPORTING starting_point TYPE yif_coordinates=>coordinates " Is data holder with little logic
                                  cardinal_point TYPE yif_geo_data=>cardinal_point
                                  sequence_list  TYPE yif_sequence=>tt_list.

  PRIVATE SECTION.
    DATA cardinal_point TYPE yif_geo_data=>cardinal_point.
    DATA starting_point TYPE yif_coordinates=>coordinates.
    DATA sequence_list  TYPE yif_sequence=>tt_list.
ENDCLASS.

CLASS ycl_geo_data IMPLEMENTATION.

  METHOD constructor.
    me->cardinal_point = cardinal_point.
    me->starting_point = starting_point.
    me->sequence_list  = sequence_list.
  ENDMETHOD.

  METHOD yif_geo_data~read_cardinal_point.
    result = cardinal_point.
  ENDMETHOD.

  METHOD yif_geo_data~read_starting_point.
    result = starting_point.
  ENDMETHOD.

  METHOD yif_geo_data~read_direction.
    result = sequence_list[ index ]-direction.
  ENDMETHOD.

ENDCLASS.
