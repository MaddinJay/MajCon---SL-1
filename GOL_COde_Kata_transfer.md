```abap
CLASS ycl_gol_cell DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_cell.

    METHODS constructor IMPORTING alive      TYPE abap_bool
                                  neighbours TYPE REF TO yif_gol_neighbours.

  PRIVATE SECTION.
    DATA alive TYPE abap_bool.
    DATA neighbours TYPE REF TO yif_gol_neighbours.
    METHODS count_living_neighbours RETURNING VALUE(result) TYPE int1.

ENDCLASS.

CLASS ycl_gol_cell IMPLEMENTATION.

  METHOD constructor.
    me->alive = alive.
    me->neighbours = neighbours.
  ENDMETHOD.

  METHOD yif_gol_cell~is_alive.
    result = me->alive.
  ENDMETHOD.

  METHOD yif_gol_cell~process.
    me->alive = COND #( WHEN count_living_neighbours( ) < 2 THEN abap_false
                        WHEN count_living_neighbours( ) = 2
                        OR   count_living_neighbours( ) = 3 THEN abap_true
                        WHEN count_living_neighbours( ) > 3 THEN abap_false ).
  ENDMETHOD.

  METHOD count_living_neighbours.
    result = neighbours->count_living( ).
  ENDMETHOD.

ENDCLASS.
```

```abap
CLASS ltcl_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_gol_cell.
    METHODS:
      create_neighbourhood_mock   IMPORTING number_of_living_neighbours TYPE int1
                                  RETURNING VALUE(result)               TYPE REF TO yif_gol_neighbours,
      create_cell_with_neighbours IMPORTING number_of_living_neighbours TYPE int1
                                            cell_is_alive               TYPE abap_bool
                                  RETURNING VALUE(result)               TYPE REF TO yif_gol_cell,

      " GIVEN: Living cell with zero neighbours WHEN: Processed THEN: ...
      cell_should_die FOR TESTING,
      " GIVEN: Living cell with two neighbours WHEN: Processed THEN: ...
      cell_should_live FOR TESTING,
      " GIVEN: Living cell with four neighbours WHEN: Processed THEN: ...
      cell_should_die_overpopulation FOR TESTING,
      " GIVEN: Dead cell with three neighbours WHEN: Processed THEN: ...
      cell_should_awake FOR TESTING.

ENDCLASS.

CLASS ltcl_cell IMPLEMENTATION.

  METHOD cell_should_die.
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = create_cell_with_neighbours( cell_is_alive               = abap_true
                                                                           number_of_living_neighbours = 0         )->is_alive( ) ).
  ENDMETHOD.

  METHOD cell_should_live.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = create_cell_with_neighbours( cell_is_alive               = abap_true
                                                                           number_of_living_neighbours = 2         )->is_alive( ) ).
  ENDMETHOD.

  METHOD cell_should_die_overpopulation.
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = create_cell_with_neighbours( cell_is_alive               = abap_true
                                                                           number_of_living_neighbours = 4         )->is_alive( ) ).
  ENDMETHOD.

  METHOD cell_should_awake.
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = create_cell_with_neighbours( cell_is_alive               = abap_false
                                                                           number_of_living_neighbours = 3         )->is_alive( ) ).
  ENDMETHOD.

  METHOD create_neighbourhood_mock.
    DATA neighbours_mock TYPE REF TO yif_gol_neighbours.

    result ?= cl_abap_testdouble=>create( 'YIF_GOL_NEIGHBOURS' ).
    cl_abap_testdouble=>configure_call( result )->returning( number_of_living_neighbours ).
    result->count_living( ).
  ENDMETHOD.


  METHOD create_cell_with_neighbours.
    DATA(neighbours_mock) = create_neighbourhood_mock( number_of_living_neighbours ).

    result = NEW ycl_gol_cell( alive      = cell_is_alive
                               neighbours = neighbours_mock ).

    result->process( ).
  ENDMETHOD.

ENDCLASS.
```
