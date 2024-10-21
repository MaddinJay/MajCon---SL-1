# HTTP Framework

The HTTP framework in SAP ABAP enables communication with web services and external systems through HTTP-based interactions. It includes the **HTTP Client** (`CL_HTTP_CLIENT`) for sending requests to external APIs and the **HTTP Server** (`IF_HTTP_EXTENSION`) for exposing ABAP functionality as HTTP services. It supports RESTful services and OData through the **RESTful ABAP Programming Model (RAP)**, ideal for data-centric applications. While the framework integrates well with SAP's ecosystem and provides essential security features like SSL/TLS, it can be limited in handling complex REST designs and high-throughput scenarios.

The HTTP client functionality, primarily built around the CL_HTTP_CLIENT class, allows ABAP to interact with external web services by managing HTTP request/response cycles. The analysis concentrates on this main class CL_HTTP_CLIENT.

## Analyse des Frameworks auf Basis des Wissens UP to PL

Das Framework besitzt Komponenten eines sauberen Frameworks.

### Positiv fällt auf, ...:

..., dass die Klasse CL_HTTP_CLIENT als API für die HTTP Calls fungiert. Die Klasse hat drei CREATE Methoden, welche jeweils ein Objekt von TYPE IF_HTTP_CLIENT erzeugt. In sich wirkt die Klasse vollständig und ein HTTP Call kann ausschliesslich mit dieser Klasse abgesetzt werden. Es werden keine weiteren Objekte zwingend benötigt.

### Dokumentation
Harzig. Die offizielle Dokumentation der SAP über [ICF](https://help.sap.com/doc/saphelp_nw74/7.4.16/en-US/48/d40aca1904154ee10000000a421937/content.htm?no_cache=true) geht zwar auf die Architektur und die Einbindung der Klasse CL_HTTP_CLIENT ein, jedoch werden praktikable und funktionierende Anwendungsbeispiele vermisst. Auch muss man sich mehrerer Dokumentationen bedienen, um einen guten Überblick über das Framework zu erhalten.

Das Coding selbst ist prozedural und es werden Kommentare für das Doing gesetzt. Hier wird nicht das WHY dokumentiert, sondern das WAS. Das COding ist Spaghetti. Es werden keine Methodennamen verwendet, um die Schritte zu dokumentieren. 

### Interfaces/API
Die Instanzen der Klasse CL_HTTP_CLIENT lassen sich über vier statische CREATE-Methode erzeugen (FACTORY method). Für den Caller ergibt sich die Schnittstelle primär aus dem Interface IF_HTTP_CLIENT. 

Die Klasse CL_HTTP_CLIENT besitzt keine öffentlichen Attribute. Hingegen das Interface IF_HTTP_CLIENT schon, welche essentiel für den Workflow zum Absetzen eines Requests und dem Erhalten der Response sind (Attribut REQUEST, RESPONSE). 

Die Klasse CL_HTTP_CLIENT besitzt GLOBAL FRIENDS, was die Übersichtlichkeit der Klasse sehr trübt. Es ist nicht ersichtlich, welche Methoden und Attribute der Klassen tatsächlich Verwendung finden und wie diese ggf. einzubinden sind. 

Die Klassen/Interfaces sind sehr mächtig. Ein Split in mehrere Klassen/Interfaces wäre sicherlich sinnvoll.

#### Patterns/Static Classes:
Für die Instanziierung wird je Klasse eine FACTORY Methode implementiert, was ok ist. Dies macht es dem Aufrufer leicht, ein entsprechendes HTTP Objekt zu erzeugen. Wie die Konstruktor arbeiten und wie ggf. die GLOBAL FRIENDS in der Klasse wirken, wird somit hinter Schnittstelle versteckt.

#### Testing:
Es sind keine UNIT Tests implmentiert.

#### Information Hiding:
Die Attribute der Klassen CL_HTTP_CLIENT, CL_HTTP_ENTITY (used for REQUEST and RESPONSE) werden über die eingebundenen Interfaces öffentlich. Diese sind mittels direkten Zugriff zugänglich. 
Hier wäre zu überlegen, ob die Attribute nicht in der Klasse CL_HTTP_CLIENT als PRIVATE aufgenommen werden sollten und der Zugriff via GETTER/SETTER ermöglicht werden sollte.

#### Naming:
Das Naming der Klassen und Methoden ist okay. Die Methoden sind lesbar und verständlich. 

#### Methoden / Messaging:
Queries und Commands werden nicht strikt von einander separiert. Die Klassen besitzen jedoch zumeist Commands, um den Zustand der Objekte oder die System Calls abzusetzen. 

Das Coding der Methoden ist nicht übersichtlich und nicht symmetrisch angeordnet. Es fehlt jegliche Dokumentation und die Aufteilung der Schritte in den Methoden erfolgt über WAS Kommentare. 

Die Methoden sind zum Teil sehr lang und daher sind sie meist nicht nur für eine Sache zuständig (SRP). 

Grundsätzlich sind die Unterklassen klein gehalten. Eine weitere Kapselung in weitere Klassen wäre jedoch denkbar.

Prinzipien wie "gutes Naming", IOSP, QCS sind nicht zu erkennen.

## Analyse des Frameworks auf Basis des Wissens UP to SL

Konzentrieren wir uns auf die Themen Atomic Design und Elegant Objects so fällt auf:

### STATISCHE OBJEKTE
Die Klassen verwenden das FACTORY PATTERN, um die Objekte zu instantiieren. Hier wäre ein Wrapper denkbar, welcher die Instanziierung der einzelnen Objekte übernimmt, so dass man die Statischen Methoden und Attribute los wird.

### CONSTRUCTORS
Im Konstruktor der Klasse CL_HTTP_CLIENT erfolgt die Instanziierung der Hilfobjekte für den Request und der Response, inklusive der ICT Calls. Gemäss Elegant Objects sollte dies nicht erfolgen. Wir sollten lediglich Zuweisungen machen.

### VERERBUNG
Es erfolgt eine Art Vererbung über das GLOBAL FRIENDS Prinzip. Es ist nicht ersichtlich, welche Objekte alle für den Workflow tatsächlich notwendig sind. Hier könnte man gemäss Elegant Objects besser mit Interfaces arbeiten und die Logik via Interfaces splitten. 

### ATOMIC DESIGN
Durch die Verwendung der System-Calls der ICT erfolgt eine gewisse Atomisierung. Die Logik in den einzelnen Methoden könnte jedoch weiter ausgelagert werden, in spearate Klassen, Interfaces. 

Auch die Methoden selbst sollten kürzer gehalten werden.

## Links
- [Client Architecture ](https://help.sap.com/doc/saphelp_snc700_ehp01/7.0.1/en-US/e5/4d350bc11411d4ad310000e83539c3/content.htm?no_cache=true)
- [REST Programming Tutorial](https://help.sap.com/docs/SAP_NETWEAVER_AS_ABAP_751_IP/753088fc00704d0a80e7fbd6803c8adb/0f5fb77942744afe94afafa78df57b70.html)
