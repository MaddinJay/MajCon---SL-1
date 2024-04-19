CLASS ycl_neighbour_hood DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_neighbour_hood.
    ALIASES: add_neighbour  FOR yif_neighbour_hood~add_neighbour,
             scan_neighbour FOR yif_neighbour_hood~scan_neighbour,
             move2neighbour FOR yif_neighbour_hood~move2neighbour,
             go             FOR yif_neighbour_hood~go.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_neighbour,
             direction TYPE yif_neighbour_hood=>direction,
             neighbour TYPE REF TO ycl_square,
           END OF ty_neighbour,
           tt_neighbour TYPE STANDARD TABLE OF ty_neighbour.
    DATA list_of_neighbours TYPE tt_neighbour.

ENDCLASS.

CLASS ycl_neighbour_hood IMPLEMENTATION.

  METHOD yif_neighbour_hood~add_neighbour.
    APPEND VALUE #( direction = direction neighbour = neighbour ) TO list_of_neighbours.
  ENDMETHOD.

  METHOD yif_neighbour_hood~scan_neighbour.
    " Scan neighbour in direction x for obstacle
    result = list_of_neighbours[ direction = direction ]-neighbour->scan4obstacle( ).
  ENDMETHOD.

  METHOD yif_neighbour_hood~move2neighbour.
    result = list_of_neighbours[ direction = direction ]-neighbour.
  ENDMETHOD.

  METHOD yif_neighbour_hood~go.
    DATA(has_obstacle) = scan_neighbour( direction ).
    IF has_obstacle IS NOT INITIAL.
      ##TODO " Handle obstacle
      RETURN.
    ENDIF.
    result = move2neighbour( direction ).
  ENDMETHOD.

ENDCLASS.
