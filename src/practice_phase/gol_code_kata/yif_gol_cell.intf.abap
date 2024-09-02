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
  METHODS get_neighbours RETURNING VALUE(result) TYPE REF TO yif_gol_neighbourhood.
  METHODS set_neighbours IMPORTING neighbourhood TYPE REF TO yif_gol_neighbourhood.

ENDINTERFACE.
