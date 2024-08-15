CLASS ycl_dijkstra_algorithm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES node TYPE char5.
    TYPES: BEGIN OF ts_node,
             node TYPE node,
           END OF ts_node.

    TYPES: BEGIN OF ts_parents,
             node   TYPE node,
             parent TYPE node,
           END OF ts_parents,
           tt_parents TYPE HASHED TABLE OF ts_parents WITH UNIQUE KEY node.
    TYPES: BEGIN OF ts_weights,
             node   TYPE node,
             weight TYPE int1,
           END OF ts_weights,
           tt_weights TYPE HASHED TABLE OF ts_weights WITH UNIQUE KEY node.

    TYPES: BEGIN OF ts_graph,
             node       TYPE node,
             neighbours TYPE tt_weights,
           END OF ts_graph,
           tt_graph TYPE HASHED TABLE OF ts_graph WITH UNIQUE KEY node.

    METHODS constructor IMPORTING graph   TYPE tt_graph
                                  costs   TYPE tt_weights
                                  parents TYPE tt_parents.

    METHODS process RETURNING VALUE(result) TYPE tt_parents.

  PRIVATE SECTION.
    DATA processed_nodes TYPE HASHED TABLE OF ts_node WITH UNIQUE KEY node.
    DATA graph           TYPE tt_graph.
    DATA costs           TYPE tt_weights.
    DATA parents         TYPE tt_parents.

    METHODS node_not_processed_yet IMPORTING node          TYPE node
                                   RETURNING VALUE(result) TYPE abap_bool.
    METHODS find_lowest_cost_node IMPORTING costs         TYPE tt_weights
                                  RETURNING VALUE(result) TYPE node.
ENDCLASS.

CLASS ycl_dijkstra_algorithm IMPLEMENTATION.

  METHOD constructor.
    me->graph = graph.
    me->costs = costs.
    me->parents = parents.
  ENDMETHOD.

  METHOD process.
    DATA(node) = find_lowest_cost_node( costs ).

    WHILE node IS NOT INITIAL.
      DATA(cost)       = me->costs[ node = node ]-weight.
      DATA(neighbours) = graph[ node = node ]-neighbours.

      LOOP AT neighbours INTO DATA(neighbour).
        DATA(new_cost) = cost + neighbour-weight.
        IF costs[  node = neighbour-node ]-weight > new_cost.
          costs[   node = neighbour-node ]-weight = new_cost.
          parents[ node = neighbour-node ]-parent = node.
        ENDIF.

      ENDLOOP.
      INSERT VALUE #( node = node ) INTO TABLE processed_nodes.
      node = find_lowest_cost_node( costs ).
    ENDWHILE.

    result = me->parents.
  ENDMETHOD.

  METHOD find_lowest_cost_node.
    DATA(lowest_costs) = 255.

    LOOP AT costs INTO DATA(line).
      IF line-weight < lowest_costs AND
         node_not_processed_yet( line-node ).
        lowest_costs = line-weight.
        result       = line-node.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD node_not_processed_yet.
    TRY.
        DATA(dummy) = processed_nodes[ node = node ].
      CATCH cx_sy_itab_line_not_found.
        result = abap_true.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
