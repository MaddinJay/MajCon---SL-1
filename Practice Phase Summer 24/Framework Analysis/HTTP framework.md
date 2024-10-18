# HTTP Framework

The HTTP framework in SAP ABAP enables communication with web services and external systems through HTTP-based interactions. It includes the **HTTP Client** (`CL_HTTP_CLIENT`) for sending requests to external APIs and the **HTTP Server** (`IF_HTTP_EXTENSION`) for exposing ABAP functionality as HTTP services. It supports RESTful services and OData through the **RESTful ABAP Programming Model (RAP)**, ideal for data-centric applications. While the framework integrates well with SAP's ecosystem and provides essential security features like SSL/TLS, it can be limited in handling complex REST designs and high-throughput scenarios.

The HTTP client functionality, primarily built around the CL_HTTP_CLIENT class, allows ABAP to interact with external web services by managing HTTP request/response cycles. The analysis concentrates on this main class CL_HTTP_CLIENT.

## Analyse des Frameworks auf Basis des Wissens UP to PL

Das Framework besitzt Komponenten eines sauberen Frameworks.

### Positiv fällt auf, ...:

..., dass die Klasse CL_HTTP_CLIENT als API für die HTTP Calls fungiert. Die Klasse hat drei CREATE Methoden, welche jeweils ein Objekt von TYPE IF_HTTP_CLIENT erzeugt. In sich wirkt die Klasse vollständig und ein HTTP Call kann ausschliesslich mit dieser Klasse abgesetzt werden. Es werden keine weiteren Objekte zwingend benötigt.

### Dokumentation
Harzig. Die offizielle Dokumentation der SAP über ICF (https://help.sap.com/doc/saphelp_nw74/7.4.16/en-US/48/d40aca1904154ee10000000a421937/content.htm?no_cache=true, https://help.sap.com/doc/saphelp_snc700_ehp01/7.0.1/en-US/e5/4d350bc11411d4ad310000e83539c3/content.htm?no_cache=true, https://help.sap.com/docs/SAP_NETWEAVER_AS_ABAP_751_IP/753088fc00704d0a80e7fbd6803c8adb/0f5fb77942744afe94afafa78df57b70.html) geht zwar auf die Architektur und die Einbindung der Klasse CL_HTTP_CLIENT ein, jedoch werden praktikable und funktionierende Anwendungsbeispiele vermisst. Auch muss man sich mehrerer Dokumentationen bedienen, um einen guten Überblick über das Framework zu erhalten.

Das Coding selbst ist prozedural und es werden Kommentare für das Doing gesetzt. Hier wird nicht das WHY dokumentiert, sondern das WAS. Das COding ist Spaghetti. Es werden keine Methodennamen verwendet, um die Schritte zu dokumentieren. 

### Interfaces/API
Die Klasseninstanzen lassen sich jeweils über eine statische Constructor-Methode erzeugen (FACTORY method). Für den Caller ergibt sich die Schnittstelle aus den öffentlichen Attributen und Methoden der Super-Klasse und Sub-Klasse. Es werden keine Interfaces verwendet. Die öffentlichen Methoden sind symmetrisch deklariert über die Klassen hinweg, was die Lesbarkeit verbessert.

Die deklarierten Konstanten und Attribute sollten besser in ein Interface ausgelagert werden, wenn sie schon öffentlich zugänglich gamacht werden. Bei den Konstanten stellt sich die Frage, ob diese wirklich zwingend öffnentlich sein müssen oder nicht doch in der Superklasse als PROTECTED deklariert werden sollten. Hier verstehe ich das Konzept aber noch zu wenig, um dies final beurteilen zu können.

Die Konstanten könnten je Themenblock zusammengefasst werden mit Hilfe von Strukturen. Generell würde ich diese in ein separates Interface auslagern, falls sie Klassenübergreifend genutzt werden sollen.

#### Patterns/Static Classes:
Für die Instanziierung wird je Klasse eine FACTORY Methode implementiert, was ok ist. Für die Instanziierung des Objekts wird ein Event erzeugt, dessen Deklaration in der Super-Klasse erfolgt und dessen Implementation in der Klasse selbst ausgeprägt ist. Dies wird ein wenig überkandidiert. Ich würde die Events rausnehmen und die Instanziierung des Objekts direkt im Konstruktor machen.

#### Testing:
Es sind keine UNIT Tests implmentiert.

#### Information Hiding:
Die Attribute der Klassen sind teilweise öffentlich, so dass sie von aussen zugreifbar und änderbar sind. Diese Attribute sollten PRIVATE sein, der Zugriff hierauf über GETTER Methoden ermöglicht werden.

#### Naming:
Das Naming der Klassen und Methoden ist okay. Die Methoden sind lesbar. 

#### Methoden / Messaging:
Die Klassen besitzen sowohl Query- als auch Commands Methoden. Query Methoden werden vor allem da verwendet, wo auf C/C++ Implementation zugriffen oder aber System-Calls abgesetzt werden. 

Das Coding der Methoden ist nicht übersichtlich und nicht symmetrisch angeordnet. Es fehlt jegliche Dokumentation. Die Methoden sind jedoch einigermassen klein gehalten und tun meist auch nur eine Sache (SRP). Es existieren viele Getter-Methoden - vermutlich weil von aussen auf die Objektinfos zugegriffen werden soll, um mit dem Objekt arbeiten zu können. 

Grundsätzlich sind die Unterklassen klein gehalten. Eine weitere Kapselung in weitere Klassen wäre jedoch denkbar.

## Analyse des Frameworks auf Basis des Wissens UP to SL

Konzentrieren wir uns auf die Themen Atomic Design und Elegant Objects so fällt auf:

### STATISCHE OBJEKTE
Die Klassen verwenden das FACTORY PATTERN, um die Objekte zu instantiieren. Hier wäre ein Wrapper denkbar, welcher die Instanziierung der einzelnen Objekte übernimmt, so dass man die Statischen Methoden und Attribute los wird.

### CONSTRUCTORS
Im Konstruktor erfolgt die Instanziierung der Objekte via Event-Handling. Gemäss Elegant Objects sollte dies nicht erfolgen. Wir sollten lediglich Zuweisungen machen.

### VERERBUNG
Die Super-Klasse vererbt an die einzelnen Unterklassen, was das Konstrukt auf den ersten Blick unübersichtlich erscheinen lässt. Hier könnte man gemäss Elegant Objects besser mit Interfaces arbeiten und die Logik via Interfaces splitten. 

### ATOMIC DESIGN
Durch die Auslagerung der Domainlogik in KERNEL MODULES erfolgt eine gewisse Atomisierung. Die Logik in den einzelnen Methoden könnte jedoch weiter ausgelagert werden, in spearate Klassen. 

Z.B. könnte in der Klasse CL_ABAP_CLASSDESCR die Logik der Methode GET_FRIEND_TYPES in eine separate Klasse ausgelagert werden. 


## Links
- [SAP Doku inkl. Testreport](https://help.sap.com/docs/SUPPORT_CONTENT/abapobjects/3353526555.html)
- [Beispiele Codezentrale](https://codezentrale.de/abap-rtti-rttc-rtts-verwendung-von-typdescriptoren/)
- [Datenreferenzen](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abendata_reference_type.htm)
- [ABAP CheatSheet](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/06_Dynamic_Programming.md#runtime-type-services-rtts)
