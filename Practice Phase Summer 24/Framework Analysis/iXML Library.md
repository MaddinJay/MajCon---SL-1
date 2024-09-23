# iXML Library

Fokus dieser Analyse ist das iXML Framwork des SAP Standards.

## Aufbau

Hauptklasse der iXML Library ist die Klasse CL_IXML, mit welcher wir über die Methode CREATE eine Instanz erzeugen können.

```abap
  DATA(ixml) = cl_ixml=>create( ).
```

Der Typ der erzeugten Instanz wird dynmisch erzeugt und kann zur Laufzeit mit CL_IXML eingesehen werden. Die Klasse selbst existiert jedoch im ABAP Dictionary nicht, was die Übersichtlichkeit ein wenig trübt.

Die API Methoden der Klasse werden via Interface implementiert, was für den Aufrufer insofern komfortable ist, als dass er die Kernlogik der Library nicht verstehen muss. Die Methoden der Klasse sind dann wiederum 
via C++ Coding umgesetzt, welches über KERNEL MODULE eingebunden ist. Hier wird nicht weiter recherchiert.

Wie so oft im SAP System ist die Dokumentation der iXML library verbesserungsfähig. Die Anwendung der Klasse ergibt sich nicht intuitiv, was gewisse Experimente mit sich bringt, um die Anwendung zu verstehen und 
nachvollziehen zu können.

Beispielsweise lässt sich ein XML Dokument in Form eines XSTREAMS, gegeben einer Datei mit Excel-Tabelleninhalten wie folgt erzeugen:

```abap
  DATA(streamfactory) = ixml->create_stream_factory( ).

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      filename   = 'C:\Users\mjon\Test_IK1_12072024_before_change.xml'
      filetype   = 'BIN'
    IMPORTING
      filelength = xml_table_size
    TABLES
      data_tab   = xml_table
    EXCEPTIONS
      OTHERS     = 11.

  DATA(istream) = streamfactory->create_istream_itable(
                    size  = xml_table_size
                    table = xml_table
                  ).
```

Die lokalen Files müssen hier via FuBa gelesen werden, das Framework bietet hier keine API Funktionalität. 

## Analyse des Frameworks auf Basis des Wissens UP to PL

Das Framework lässt grundsätzliche sehr viele Aspekte von CleanCode vermissen. 

### Positiv fällt auf:
Falls man weiss, wie die einzelnen Klassen und Methoden miteinander arbeiten, kann es sehr komfortabel sein, mit der Library zu arbeiten. 

Es sind Ansätze der Trennung von Verantwortlichkeiten zu erkennen. 

Z.B. kann mit Hilfe des Objekts CL_IXML eine STREAM_FACTORY erzeugt werden, welche widerum für die Erzeugung des iStreams (XML File in XString Format) 
verantwortlich ist. Oder aber ein DOM Objekt, welches man für den Aufbau des XMLs verwenden kann (Knoten hinzufügen/löschen,...). Das macht das Handling schon einmal ein wenig angenehmer, siehe Coding oben.

```abap
DATA(document) = ixml->create_document( ).
```

### Negativ fällt auf:

#### Dokumentation:
Wie so oft ist die Dokumenation der Library sehr dünn. Es gibt gewisse DEMO Reports "DEMO_IXML*", mit deren Hilfe man das Framework verstehen kann. Dennoch muss man einmal mehr sich das Wissen selbst aneignen. 
Allein der Satz "Eine Einführung und Beispiele für die Verwendung der iXML-Bibliothek finden Sie in der ABAP-Schlüsselwortdokumentation." in der [offiziellen Dokumentation](https://help.sap.com/docs/ABAP_PLATFORM_NEW/7bfe8cdcfbb040dcb6702dada8c3e2f0/47b5413acdb62f70e10000000a114084.html) der SAP ist schon frech :).

Das Coding selbst ist nicht dokumentiert. Ein Grossteil des Codings ist in C++ umgesetzt, was die Nachvollzierhbarkeit weiter einschränkt. 

#### Interfaces / API:
Für die Hauptklassen werden Interfaces verwendet, was schon einmal ganz gut ist. Je Klasse gibt es jedoch nur ein Interface, so dass auch hier das Prinzip Separation of Concerns verletzt wird. Es könnten mehr 
Interfaces verwendet werden, um die Domainlogik weiter zu splitten. 
```abap
*"* components of interface IF_IXML
interface IF_IXML
  public .


  interfaces IF_IXML_UNKNOWN .

  constants CO_MULTIPLE_DOCUMENTS type I value 0. "#EC NOTEXT
  constants CO_SINGLE_DOCUMENT type I value 1. "#EC NOTEXT
  constants CO_SINGLE_LARGE_DOCUMENT type I value 3. "#EC NOTEXT
  constants CO_SINGLE_SMALL_DOCUMENT type I value 2. "#EC NOTEXT

  methods ACTIVATE .
  methods CREATE_DOCUMENT
    returning
      value(RVAL) type ref to IF_IXML_DOCUMENT .
  methods CREATE_ENCODING
    importing
      !BYTE_ORDER type I
      !CHARACTER_SET type STRING
    returning
      value(RVAL) type ref to IF_IXML_ENCODING .
  methods CREATE_PARSER
    importing
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !ISTREAM type ref to IF_IXML_ISTREAM
      !STREAM_FACTORY type ref to IF_IXML_STREAM_FACTORY
    returning
      value(RVAL) type ref to IF_IXML_PARSER .
  methods CREATE_RENDERER
    importing
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !OSTREAM type ref to IF_IXML_OSTREAM
    returning
      value(RVAL) type ref to IF_IXML_RENDERER .
  methods CREATE_STREAM_FACTORY
    returning
      value(RVAL) type ref to IF_IXML_STREAM_FACTORY .
  methods CREATE_TOKEN_PARSER
    importing
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !ISTREAM type ref to IF_IXML_ISTREAM
      !STREAM_FACTORY type ref to IF_IXML_STREAM_FACTORY
    returning
      value(RVAL) type ref to IF_IXML_TOKEN_PARSER .
  methods CREATE_TOKEN_RENDERER
    importing
      !OSTREAM type ref to IF_IXML_OSTREAM
    returning
      value(RVAL) type ref to IF_IXML_TOKEN_RENDERER .
  methods GET_VERSION
    returning
      value(RVAL) type I .
  methods HAS_FEATURE
    importing
      !FEATURE type STRING
      !VERSION type STRING
    returning
      value(RVAL) type BOOLEAN .
  methods SET_CONVERSION_BEHAVIOUR
    importing
      !CODEPAGE type STRING default '*'
      !FAIL_ON_ERROR type BOOLEAN default ' '
      !REPLACEMENT_CHAR type IXML_C1 default '#'
    returning
      value(RVAL) type I .
endinterface.
```
Auch hier ist die Arbeit beim Entwickler einmal mehr, zu verstehen, dass die Methoden alle machen und wie sie überhaupt aufrufbar sind. 

#### Patterns/Static Classes:
Die Erzeugung des Hauptobjektes der Klasse CL_IXML wird mit dem Factory Pattern durchgeführt. Im Constructor selbst wird ein TYPE deklariert. Diese Deklaration sollte in die DEFINITION gezogen, besser in ein Interface ausgelagert werden.
Grundsätzlich ist die Klasse wieder allumfassend. Bei näherer Betrachtung des Frameworks kann man diese sicherlich weiter splitten.
```abap
method create.
 if not the_iXML is initial.
    rval = the_iXML.
  else.
    data: begin of classes,
           iid_0  type i value 1  ,
           ref_0  type ref to cl_ixml_unknown,
           iid_1  type i value 10 ,
           ref_1  type ref to cl_ixml,
           iid_2  type i value 20 ,
           ref_2  type ref to cl_ixml_node,
           iid_3  type i value 30 ,
```

#### Testing:
Es sind keine Testklassen implementiert. 

#### Information Hiding:

In der Klasse CL_IXML werden die Attribute in PUBLIC SECTION deklariert, obschon sie mittels CREATE Methode nach aussen ausgegeben werden. 
Besser hier die Attribute PRIVATE setzen.

```abap
public section.

  interfaces IF_IXML .

  class-data THE_IXML type ref to CL_IXML .
  class-data THE_IXML_POINTEE type %_C_POINTER .
```

#### Naming:
Grundsätzlich ist das Naming nicht schlecht, dennoch könnte man über Verbesserungen diskutieren. Ein Interface und eine Klasse mit dem Suffix "UNKNOWN" zu versehen,
verwirrt doch mehr, als dass man darauf schliessen könnte, was das Objekt tut ( CL_IXML_UNKNOWN ).

## Analyse des Frameworks auf Basis des Wissens UP to SL

Das Framework lässt bereits sehr viele Aspekte von gutem Software Design UP to PL vermissen.

### Keine Vererbung / Keine statischen Methoden
Die Hauptklasse CL_IXML verletzt diese Ansätze. Besser wäre es, auf die Vererbung zu verzichten, zumal das Interface IF_IXML_UNKNOWN lediglich eine Methode besitzt. 

Die Erzeugung des iXML Objektes könnte man durch einen Wrapper re alisieren.








 