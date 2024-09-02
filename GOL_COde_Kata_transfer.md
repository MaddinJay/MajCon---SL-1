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

  METHOD yif_gol_cell~get_neighbours.
*    result = neighbourhood.
  ENDMETHOD.

ENDCLASS.

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
CLASS ycl_gol_grid DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_gol_grid.
    ALIASES grid FOR yif_gol_grid~grid.

  PRIVATE SECTION.
    METHODS create_row_entries IMPORTING size TYPE int1
                                         y    TYPE i.
    METHODS create_random_bool RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS ycl_gol_grid IMPLEMENTATION.

  METHOD yif_gol_grid~create.
    DATA(row) = 1.
    DO size TIMES.
      create_row_entries( size = size
                          y    = row ).
      row = row + 1.
    ENDDO.
  ENDMETHOD.

  METHOD create_row_entries.
    DATA(x) = 1.
    DO size TIMES.
      APPEND VALUE yif_gol_grid=>ts_grid( x = x y = y cell = NEW #( alive         = create_random_bool( )
                                                                    neighbourhood = VALUE #( ) ) ) TO grid.
      x = x + 1.
    ENDDO.
  ENDMETHOD.

  METHOD create_random_bool.
    " Create random integer and check if it is even -> ABAP_TRUE, else ABAP_FALSE
    result = xsdbool( cl_abap_random=>create( )->int( ) MOD 2 = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_gol_grid DEFINITION DEFERRED.
CLASS ycl_gol_grid DEFINITION LOCAL FRIENDS ltcl_gol_grid.

CLASS ltcl_gol_grid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE  REF TO yif_gol_grid.

    METHODS:
      "GIVEN: Size of grid is 3 WHEN: Creating the grid THEN: Grid ...
      should_have_nine_lines FOR TESTING,
      "GIVEN: Size of grid is 3 WHEN: Creating the grid THEN: Last line ...
      should_have_coordinates_3_3 FOR TESTING,

      create_cut IMPORTING size          TYPE int1
                 RETURNING VALUE(result) TYPE REF TO yif_gol_grid,
      get_last_line_of_3x3_grid RETURNING VALUE(result) TYPE yif_gol_grid=>ts_grid.
ENDCLASS.

CLASS ltcl_gol_grid IMPLEMENTATION.

  METHOD should_have_nine_lines.
    cl_abap_unit_assert=>assert_equals( exp = 9 act = lines( create_cut( size = 3 )->grid ) ).
  ENDMETHOD.

  METHOD should_have_coordinates_3_3.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_last_line_of_3x3_grid( )-x ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_last_line_of_3x3_grid( )-y ).
  ENDMETHOD.

  METHOD create_cut.
    result = NEW ycl_gol_grid( ).
    result->create( size ).
  ENDMETHOD.

  METHOD get_last_line_of_3x3_grid.
    DATA(cut) = create_cut( size = 3 ).
    result = cut->grid[ 9 ].
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

CLASS ltcl_gol_grid DEFINITION DEFERRED.
CLASS ycl_gol_grid DEFINITION LOCAL FRIENDS ltcl_gol_grid.

CLASS ltcl_gol_grid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE  REF TO yif_gol_grid.

    METHODS:
      "GIVEN: Size of grid is 3 WHEN: Creating the grid THEN: Grid ...
      should_have_nine_lines FOR TESTING,
      "GIVEN: Size of grid is 3 WHEN: Creating the grid THEN: Last line ...
      should_have_coordinates_3_3 FOR TESTING,

      create_cut IMPORTING size          TYPE int1
                 RETURNING VALUE(result) TYPE REF TO yif_gol_grid,
      get_last_line_of_3x3_grid RETURNING VALUE(result) TYPE yif_gol_grid=>ts_grid.
ENDCLASS.

CLASS ltcl_gol_grid IMPLEMENTATION.

  METHOD should_have_nine_lines.
    cl_abap_unit_assert=>assert_equals( exp = 9 act = lines( create_cut( size = 3 )->grid ) ).
  ENDMETHOD.

  METHOD should_have_coordinates_3_3.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_last_line_of_3x3_grid( )-x ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_last_line_of_3x3_grid( )-y ).
  ENDMETHOD.

  METHOD create_cut.
    result = NEW ycl_gol_grid( ).
    result->create( size ).
  ENDMETHOD.

  METHOD get_last_line_of_3x3_grid.
    DATA(cut) = create_cut( size = 3 ).
    result = cut->grid[ 9 ].
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

    DATA grid_size TYPE int1.

    METHODS upper_neighbour IMPORTING x             TYPE int1
                            RETURNING VALUE(result) TYPE i.
    METHODS downstairs_neighbour IMPORTING x             TYPE int1
                                 RETURNING VALUE(result) TYPE i.
    METHODS neighbour_left       IMPORTING y             TYPE int1
                                 RETURNING VALUE(result) TYPE i.
    METHODS neighbour_right      IMPORTING y             TYPE int1
                                 RETURNING VALUE(result) TYPE int1.

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

  METHOD yif_gol_neighbourhood~create.
    me->grid_size = sqrt( lines( grid ) ).
    neighbours = VALUE #( ( grid[ x = upper_neighbour( cell_coordinates-x )      y = neighbour_left( cell_coordinates-y ) ]-cell )
                          ( grid[ x = cell_coordinates-x                         y = neighbour_left( cell_coordinates-y ) ]-cell )
                          ( grid[ x = downstairs_neighbour( cell_coordinates-x ) y = neighbour_left( cell_coordinates-y ) ]-cell )

                          ( grid[ x = upper_neighbour( cell_coordinates-x )      y = cell_coordinates-y ]-cell )
                          ( grid[ x = downstairs_neighbour( cell_coordinates-x ) y = cell_coordinates-y ]-cell )

                          ( grid[ x = upper_neighbour( cell_coordinates-x )      y = neighbour_right( cell_coordinates-y ) ]-cell )
                          ( grid[ x = cell_coordinates-x                         y = neighbour_right( cell_coordinates-y ) ]-cell )
                          ( grid[ x = downstairs_neighbour( cell_coordinates-x ) y = neighbour_right( cell_coordinates-y ) ]-cell ) ).
  ENDMETHOD.

  METHOD neighbour_left.
    result = COND #( WHEN y - 1 = 0 THEN grid_size
                     ELSE y - 1 ).
  ENDMETHOD.

  METHOD downstairs_neighbour.
    result = COND #( WHEN x + 1 > grid_size THEN 1
                     ELSE x + 1 ).
  ENDMETHOD.

  METHOD upper_neighbour.
    result = COND #( WHEN x - 1 = 0 THEN grid_size
                     ELSE x - 1 ) .
  ENDMETHOD.

  METHOD yif_gol_neighbourhood~get_neighbours.
    result = neighbours.
  ENDMETHOD.

  METHOD neighbour_right.
    result = COND #( WHEN y + 1 > grid_size THEN 1
                     ELSE grid_size ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_neighbourhood DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_gol_neighbourhood.

    METHODS:
      " GIVEN: Neighbourhood with 8 living neighbours WHEN: Counting living neighbours THEN: ...
      should_have_8_living_neighbour FOR TESTING.

    METHODS:
      create_neighbourhood IMPORTING number_of_neighbours TYPE int1
                           RETURNING VALUE(result)        TYPE REF TO yif_gol_neighbourhood.
ENDCLASS.


CLASS ltcl_neighbourhood IMPLEMENTATION.

  METHOD should_have_8_living_neighbour.
    cl_abap_unit_assert=>assert_equals( exp = 8
                                        act = create_neighbourhood( number_of_neighbours = 3 )->count_living( ) ).
  ENDMETHOD.

  METHOD create_neighbourhood.
    DATA(grid_table) = VALUE yif_gol_grid=>tt_grid( ( x = 1 y = 1 cell = NEW ycl_gol_cell( alive = abap_false neighbourhood = VALUE #( )  ) )
                                                    ( x = 2 y = 1 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 3 y = 1 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 1 y = 2 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 2 y = 2 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 3 y = 2 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 1 y = 3 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 2 y = 3 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) )
                                                    ( x = 3 y = 3 cell = NEW ycl_gol_cell( alive = abap_true  neighbourhood = VALUE #( )  ) ) ).

    result = NEW ycl_gol_neighbourhood( neighbours = VALUE #( ) ).
    result->create( grid = grid_table
                    cell_coordinates = VALUE #( x    = 1
                                                y    = 1 ) ).
  ENDMETHOD.



ENDCLASS.
```

```abap

INTERFACE yif_gol_cell
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Play one round for the cell</p>
  "!
  METHODS process.
  "! <p class="shorttext synchronized" lang="en">Check if cell is alive</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">Is alive? YES = ABAP_TRUE</p>
  METHODS is_alive RETURNING VALUE(result) TYPE abap_bool.

  "! <p class="shorttext synchronized" lang="en">Read the neighbours of the cell</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_neighbours RETURNING VALUE(result) TYPE REF TO yif_gol_neighbourhood=>tt_neighbours.

ENDINTERFACE.

INTERFACE yif_gol_grid
  PUBLIC .

  TYPES: BEGIN OF ts_grid,
           x    TYPE int1,
           y    TYPE int1,
           cell TYPE REF TO ycl_gol_cell,
         END OF ts_grid,
         tt_grid TYPE STANDARD TABLE OF ts_grid WITH DEFAULT KEY.

  DATA grid TYPE yif_gol_grid=>tt_grid.


  "! <p class="shorttext synchronized" lang="en">Creating the grid with a given size</p>
  "!
  "! @parameter size | <p class="shorttext synchronized" lang="en">Size of grid (size x size)</p>
  METHODS create IMPORTING size TYPE int1.

ENDINTERFACE.

INTERFACE yif_gol_neighbourhood
  PUBLIC .
  TYPES tt_neighbours TYPE TABLE OF REF TO yif_gol_cell WITH DEFAULT KEY.
  TYPES: BEGIN OF ts_coordinates,
           x TYPE int1,
           y TYPE int1,
         END OF ts_coordinates.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Create neighbourhoud for a specific cell
  "! @parameter grid | <p class="shorttext synchronized" lang="en">Grid which is given</p>
  "! @parameter cell_coordinates | <p class="shorttext synchronized" lang="en">Coordinates of cell</p>
  METHODS create IMPORTING grid             TYPE yif_gol_grid=>tt_grid
                           cell_coordinates TYPE ts_coordinates.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Counting the living cells in the neighbourhood
  "! @parameter result | <p class="shorttext synchronized" lang="en">Number of living neighbours</p>
  METHODS count_living RETURNING VALUE(result) TYPE int1.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get neighbours of a cell
  "! @parameter result | <p class="shorttext synchronized" lang="en">Neighbours of cell</p>
  METHODS get_neighbours RETURNING VALUE(result) TYPE tt_neighbours.

ENDINTERFACE.
```


ENDCLASS.
```
