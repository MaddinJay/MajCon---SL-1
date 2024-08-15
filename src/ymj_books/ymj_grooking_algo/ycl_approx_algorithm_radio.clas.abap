CLASS ycl_approx_algorithm_radio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_states,
             state TYPE string,
           END OF ts_states,
           tt_states TYPE HASHED TABLE OF ts_states WITH UNIQUE KEY state,
           BEGIN OF ts_stations,
             station TYPE string,
             states  TYPE tt_states,
           END OF ts_stations,
           tt_stations TYPE HASHED TABLE OF ts_stations WITH UNIQUE KEY station.
    METHODS constructor IMPORTING states_needed TYPE tt_states
                                  stations      TYPE tt_stations.
    METHODS process     RETURNING VALUE(result) TYPE tt_stations.

  PRIVATE SECTION.
    DATA states_needed TYPE tt_states.
    DATA stations      TYPE tt_stations.

    METHODS set_intersection IMPORTING states_needed  TYPE ycl_approx_algorithm_radio=>tt_states
                                       station_states TYPE ycl_approx_algorithm_radio=>tt_states
                             RETURNING VALUE(result)  TYPE tt_states.
    METHODS mininize_states_needed IMPORTING states_covered TYPE ycl_approx_algorithm_radio=>tt_states.
ENDCLASS.

CLASS ycl_approx_algorithm_radio IMPLEMENTATION.

  METHOD constructor.
    me->states_needed = states_needed.
    me->stations      = stations.
  ENDMETHOD.

  METHOD process.

    WHILE states_needed IS NOT INITIAL.
      DATA(best_station)   = VALUE ts_stations( ).
      DATA(states_covered) = VALUE tt_states( ).

      LOOP AT me->stations INTO DATA(station).
        DATA(covered) = set_intersection( states_needed  = states_needed
                                          station_states = station-states ).
        IF lines( covered ) > lines( states_covered ).
          best_station   = station.
          states_covered = covered.
        ENDIF.
      ENDLOOP.
      mininize_states_needed( states_covered ).
      INSERT best_station INTO TABLE result.
    ENDWHILE.

  ENDMETHOD.

  METHOD mininize_states_needed.
    LOOP AT states_covered INTO DATA(state_covered).
      DELETE TABLE states_needed FROM state_covered.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_intersection.
    LOOP AT states_needed INTO DATA(state_needed).
      TRY.
          DATA(state_found) = station_states[ state = state_needed-state ].
          INSERT state_found INTO TABLE result.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
