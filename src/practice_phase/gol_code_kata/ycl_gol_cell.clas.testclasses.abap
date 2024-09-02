CLASS ltcl_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:

      " GIVEN: Living cell with zero neighbours WHEN: Processed THEN: ...
      cell_should_die                FOR TESTING,
      " GIVEN: Living cell with two neighbours WHEN: Processed THEN: ...
      cell_should_live               FOR TESTING,
      " GIVEN: Living cell with four neighbours WHEN: Processed THEN: ...
      cell_should_die_overpopulation FOR TESTING,
      " GIVEN: Dead cell with three neighbours WHEN: Processed THEN: ...
      cell_should_awake              FOR TESTING,

      create_neighbours_mock    IMPORTING number_of_living_neighbours TYPE int1
                                RETURNING VALUE(result)               TYPE REF TO yif_gol_neighbourhood,
      process_cell              IMPORTING number_of_living_neighbours TYPE int1
                                          cell_is_alive               TYPE abap_bool
                                RETURNING VALUE(result)               TYPE REF TO yif_gol_cell.
ENDCLASS.

CLASS ltcl_cell IMPLEMENTATION.

  METHOD cell_should_die.
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = process_cell( cell_is_alive               = abap_true
                                                            number_of_living_neighbours = 0         )->is_alive( ) ).
  ENDMETHOD.

  METHOD cell_should_live.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = process_cell( cell_is_alive               = abap_true
                                                            number_of_living_neighbours = 2         )->is_alive( ) ).
  ENDMETHOD.

  METHOD cell_should_die_overpopulation.
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = process_cell( cell_is_alive               = abap_true
                                                            number_of_living_neighbours = 4         )->is_alive( ) ).
  ENDMETHOD.

  METHOD cell_should_awake.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = process_cell( cell_is_alive               = abap_false
                                                            number_of_living_neighbours = 3         )->is_alive( ) ).
  ENDMETHOD.

  METHOD create_neighbours_mock.
    result ?= cl_abap_testdouble=>create( 'yif_gol_neighbourhood' ).
    cl_abap_testdouble=>configure_call( result )->returning( number_of_living_neighbours ).
    result->count_living( ).
  ENDMETHOD.

  METHOD process_cell.
    DATA(neighbours_mock) = create_neighbours_mock( number_of_living_neighbours ).

    result = NEW ycl_gol_cell( alive       = cell_is_alive
                               coordinates = VALUE #( x = 1 y = 1 ) ).
    result->set_neighbours( neighbourhood = neighbours_mock ).

    result->process( ).
  ENDMETHOD.

ENDCLASS.
