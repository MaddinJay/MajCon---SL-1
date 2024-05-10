CLASS ycl_create_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: yif_create_map.
    ALIASES: create FOR yif_create_map~create.

ENDCLASS.

CLASS ycl_create_map IMPLEMENTATION.

  METHOD yif_create_map~create.
    result = NEW ycl_map( geo_data = geo_data
                          map_size = map_size
                          spy      = spy ).
    result->create_surface( ).
    result->calibrate( ).
  ENDMETHOD.

ENDCLASS.
