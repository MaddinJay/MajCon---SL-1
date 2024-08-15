CLASS ycl_knn_algorithm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_neighbour,
             name     TYPE string,
             distance TYPE int1,
           END OF ts_neighbour,
           tt_neighbour TYPE STANDARD TABLE OF ts_neighbour WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_feature,
             name    TYPE string,
             weather TYPE int1,
             day_off TYPE int1,
             game_on TYPE int1,
             loaves  TYPE int4,
           END OF ts_feature,
           tt_feature TYPE STANDARD TABLE OF ts_feature WITH DEFAULT KEY.

    METHODS constructor IMPORTING point    TYPE ts_feature
                                  features TYPE tt_feature
                                  knn      TYPE int1.

    METHODS process RETURNING VALUE(result) TYPE dec12_2.

  PRIVATE SECTION.
    DATA point    TYPE ts_feature.
    DATA features TYPE tt_feature.
    DATA knn      TYPE int1.

    DATA neighbours TYPE tt_neighbour.

    METHODS calculate_distances.
    METHODS calulate_regression RETURNING VALUE(result) TYPE dec12_2.

ENDCLASS.

CLASS ycl_knn_algorithm IMPLEMENTATION.

  METHOD constructor.
    me->point    = point.
    me->features = features.
    me->knn      = knn.
  ENDMETHOD.

  METHOD process.
    calculate_distances( ).
    result = calulate_regression( ).
  ENDMETHOD.

  METHOD calculate_distances.
    LOOP AT me->features INTO DATA(feature).
      APPEND INITIAL LINE TO me->neighbours ASSIGNING FIELD-SYMBOL(<neighbour>).
      <neighbour> = VALUE #( name     = feature-name
                             distance = ipow( base = ( feature-weather - point-weather ) exp = 2 ) +
                                        ipow( base = ( feature-day_off - point-day_off ) exp = 2 ) +
                                        ipow( base = ( feature-game_on - point-game_on ) exp = 2 ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD calulate_regression.
    SORT me->neighbours BY distance.

    LOOP AT me->neighbours INTO DATA(neighbour) TO me->knn.
      result = result + me->features[ name = neighbour-name ]-loaves.
    ENDLOOP.
    result = result / me->knn.
  ENDMETHOD.
ENDCLASS.
