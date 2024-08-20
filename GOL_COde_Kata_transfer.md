```abap
CLASS ycl_gol_cell DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_cell.

    METHODS constructor IMPORTING alive         TYPE abap_bool
                                  neighbourhood TYPE REF TO yif_gol_neighbourhood.

  PRIVATE SECTION.
    DATA alive TYPE abap_bool.
    DATA neighbourhood TYPE REF TO yif_gol_neighbourhood.

    METHODS count_living_neighbours RETURNING VALUE(result) TYPE int1.

ENDCLASS.

CLASS ycl_gol_cell IMPLEMENTATION.

  METHOD constructor.
    me->alive         = alive.
    me->neighbourhood = neighbourhood.
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
    result = neighbourhood->count_living( ).
  ENDMETHOD.

ENDCLASS.
```

```abap
CLASS ltcl_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      create_neighbours_mock   IMPORTING number_of_living_neighbours TYPE int1
                               RETURNING VALUE(result)               TYPE REF TO yif_gol_neighbourhood,
      process_cell IMPORTING number_of_living_neighbours TYPE int1
                             cell_is_alive               TYPE abap_bool
                   RETURNING VALUE(result)               TYPE REF TO yif_gol_cell,

      " GIVEN: Living cell with zero neighbours WHEN: Processed THEN: ...
      cell_should_die                FOR TESTING,
      " GIVEN: Living cell with two neighbours WHEN: Processed THEN: ...
      cell_should_live               FOR TESTING,
      " GIVEN: Living cell with four neighbours WHEN: Processed THEN: ...
      cell_should_die_overpopulation FOR TESTING,
      " GIVEN: Dead cell with three neighbours WHEN: Processed THEN: ...
      cell_should_awake              FOR TESTING.

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

    result = NEW ycl_gol_cell( alive      = cell_is_alive
                               neighbourhood = neighbours_mock ).

    result->process( ).
  ENDMETHOD.

ENDCLASS.
```

```abap
CLASS ycl_gol_neighbourhood DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_neighbourhood.

    METHODS constructor IMPORTING neighbours TYPE yif_gol_neighbourhood=>tt_neighbours.

  PRIVATE SECTION.
    DATA neighbours TYPE yif_gol_neighbourhood=>tt_neighbours.

ENDCLASS.

CLASS ycl_gol_neighbourhood IMPLEMENTATION.

  METHOD constructor.
    me->neighbours = neighbours.
  ENDMETHOD.

  METHOD yif_gol_neighbourhood~count_living.
    result = REDUCE #( INIT x = 0
                       FOR <line> IN neighbours
                       NEXT x = x + COND #( WHEN <line>->is_alive( ) THEN 1 ) ).
  ENDMETHOD.

ENDCLASS.
```

```abap
CLASS ltcl_neighbours DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      " GIVEN: Neighbourhood with 3 living neighbours WHEN: Counting living neighbours THEN: ...
      should_have_3_living_neighbour FOR TESTING.

    METHODS:
      create_neighbourhood IMPORTING number_of_neighbours TYPE int1
                           RETURNING VALUE(result)        TYPE REF TO yif_gol_neighbourhood.
ENDCLASS.


CLASS ltcl_neighbours IMPLEMENTATION.

  METHOD should_have_3_living_neighbour.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = create_neighbourhood( number_of_neighbours = 3 )->count_living( ) ).
  ENDMETHOD.

  METHOD create_neighbourhood.
    DATA neighbours TYPE yif_gol_neighbourhood=>tt_neighbours.

    DO number_of_neighbours TIMES.
      APPEND NEW ycl_gol_cell( alive      = abap_true
                               neighbourhood = VALUE #( ) ) TO neighbours.
    ENDDO.

    result = NEW ycl_gol_neighbourhood( neighbours ).
  ENDMETHOD.

ENDCLASS.
```
