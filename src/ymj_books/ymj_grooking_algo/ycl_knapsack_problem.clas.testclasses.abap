
CLASS ltcl_knapsack DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ycl_knapsack_problem.

    METHODS:
      " GIVEN: List with three items WHEN: Looking for best choice THEN: ...
      res_should_be_laptop_guitar FOR TESTING.
    METHODS create_cut_instance RETURNING VALUE(result) TYPE REF TO ycl_knapsack_problem.
ENDCLASS.


CLASS ltcl_knapsack IMPLEMENTATION.

  METHOD res_should_be_laptop_guitar.
    create_cut_instance( ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE ycl_knapsack_problem=>tt_items( ( name = 'Laptop' weight = 3 value = '2000.00' )
                                                                                    ( name = 'Guitar' weight = 1 value = '1500.00' )     )
                                        act = create_cut_instance( )->process( ) ).
  ENDMETHOD.

  METHOD create_cut_instance.
    result = NEW #( max_weight = 4
                    items      = VALUE ycl_knapsack_problem=>tt_items( ( name = 'Guitar' weight = 1 value = '1500.00' )
                                                                       ( name = 'Stereo' weight = 4 value = '3000.00' )
                                                                       ( name = 'Laptop' weight = 3 value = '2000.00' ) ) ).
  ENDMETHOD.

ENDCLASS.
