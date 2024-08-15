CLASS ltcl_dijktra_algo DEFINITION DEFERRED.
CLASS ycl_dijkstra_algorithm DEFINITION LOCAL FRIENDS ltcl_dijktra_algo.

CLASS ltcl_dijktra_algo DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_dijkstra_algorithm.

    METHODS:
      " GIVEN: Graph with shortest way START->B->A->FIN WHEN: Execute algorithm THEN: ...
      should_find_path_via_node_b_a FOR TESTING RAISING cx_static_check.
    METHODS create_cut4testing.
ENDCLASS.

CLASS ltcl_dijktra_algo IMPLEMENTATION.

  METHOD should_find_path_via_node_b_a.
    create_cut4testing( ).

    " Execute Algorithm
    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_dijkstra_algorithm=>tt_parents( ( node = 'A' parent = 'B' )
                                                                                        ( node = 'B' parent = 'START' )
                                                                                        ( node = 'FIN' parent = 'A' ) )
                                        act = cut->process( ) ).
  ENDMETHOD.

  METHOD create_cut4testing.
    cut = NEW #( graph   = VALUE #( ( node = 'START' neighbours = VALUE #( ( node = 'A' weight = 6 )
                                                                           ( node = 'B' weight = 2 ) ) )
                                    ( node = 'A'     neighbours = VALUE #( ( node = 'FIN' weight = 1 ) ) )
                                    ( node = 'B'     neighbours = VALUE #( ( node = 'A'   weight = 3 )
                                                                           (  node = 'FIN' weight = 5 ) ) )
                                    ( node = 'FIN' )
                                  )
                 costs   = VALUE #( ( node = 'A'   weight = 6 )
                                    ( node = 'B'   weight = 2 )
                                    ( node = 'FIN' weight = 255 ) ) " MAX value for int1
                 parents = VALUE #( ( node = 'A' parent = 'START' )
                                    ( node = 'B' parent = 'START' )
                                    ( node = 'FIN' ) ) ).

  ENDMETHOD.

ENDCLASS.
