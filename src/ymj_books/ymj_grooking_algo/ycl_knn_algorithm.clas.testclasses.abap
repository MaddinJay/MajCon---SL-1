CLASS ltcl_knn_algorithm DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_knn_algorithm.

    METHODS:
      " GIVEN: List of Features and a point WHEN: Workflow is executed THEN: For point ...
      should_calculate_218_75_loaves FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_knn_algorithm IMPLEMENTATION.

  METHOD should_calculate_218_75_loaves.
    cut = NEW #( point    = VALUE #(   name = 'X' weather = 4 day_off = 1 game_on = 0 loaves = 0 )
                 features = VALUE #( ( name = 'A' weather = 5 day_off = 1 game_on = 0 loaves = 300 )
                                     ( name = 'B' weather = 3 day_off = 1 game_on = 1 loaves = 225  )
                                     ( name = 'C' weather = 1 day_off = 1 game_on = 0 loaves = 75 )
                                     ( name = 'D' weather = 4 day_off = 0 game_on = 1 loaves = 200 )
                                     ( name = 'E' weather = 4 day_off = 0 game_on = 0 loaves = 150 )
                                     ( name = 'F' weather = 2 day_off = 0 game_on = 0 loaves = 50  ) )
                 knn      = 4 ).
    cl_abap_unit_assert=>assert_equals( exp = '218.75'
                                        act = cut->process( ) ).
  ENDMETHOD.

ENDCLASS.
