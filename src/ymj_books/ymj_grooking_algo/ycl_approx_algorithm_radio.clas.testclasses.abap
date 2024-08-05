CLASS ltcl_approx_algo DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_approx_algorithm_radio.

    METHODS create_instance4test RETURNING VALUE(result) TYPE REF TO ycl_approx_algorithm_radio.

    METHODS:
      " GIVEN: List of 5 needed states and 3 stations WHEN: Algo is executes THEN: ...
      should_find_station_1_and_5 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_approx_algo IMPLEMENTATION.

  METHOD should_find_station_1_and_5.
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_approx_algorithm_radio=>tt_stations( ( station = 'kone'  states = VALUE #( ( state = |id| )
                                                                                                                                   ( state = |nv| )
                                                                                                                                   ( state = |ut| ) ) )
                                                                                             ( station = 'kfive' states = VALUE #( ( state = |ca| )
                                                                                                                                   ( state = |az| ) ) ) )
                                        act = create_instance4test( )->process( ) ).
  ENDMETHOD.

  METHOD create_instance4test.
    result = NEW #( states_needed = VALUE ycl_approx_algorithm_radio=>tt_states( ( state = |id| )
                                                                                 ( state = |nv| )
                                                                                 ( state = |ut| )
                                                                                 ( state = |ca| )
                                                                                 ( state = |az| ) )
                 stations      = VALUE ycl_approx_algorithm_radio=>tt_stations( ( station = 'kone'  states = VALUE #( ( state = |id| )
                                                                                                                      ( state = |nv| )
                                                                                                                      ( state = |ut| ) ) )
                                                                                ( station = 'kfour' states = VALUE #( ( state = |nv| )
                                                                                                                      ( state = |ut| ) ) )
                                                                                ( station = 'kfive' states = VALUE #( ( state = |ca| )
                                                                                                                      ( state = |az| ) ) ) ) ).
  ENDMETHOD.

ENDCLASS.
