https://LiaScript.github.io/course/?https://github.com/EliasTechnik/EnglishPresentation/blob/main/readme.md
# Introduction: Project LRTallyLights

A short presentation of a side project that I have been working on since December 2020. 

## Background  and Motivation:
Since the first lockdown in 2020 the need for live streams has gone up. The church in my hometown searched some people with enugth technical confidence to do it. Because it was interesting and I head enugth time to do it, I joined the team.

During the lockdown we streamed every sunday until summer. After that we streamed on special occasions like the christmas or at easter. In that time we leraned a lot and got more professional and the team did grow.

With a growing Team we where able to use more perspectives. Every non static camera needs an camera operator which is responible to react on things in front of the camera. 
We used an intercom to comunicate between the director (he switches between cameras and does quality control) and the camera operators but that wasn't very reliable. So I looked what bigger productions do. They use tally ligths. 

Tally Ligths are, mostly red and green, lights which are mounted on each camera. They signal the camera operator if he is live or not. They also let the person in front of the camera know in which camera they should talk. 

We used the Open Brodcaster Software (OBS) to stream so we needed a cheep tally system wich could be used with OBS. At the time I started working on a solution there wasn't any system available that suited our needs. 

## Parts of the Project

I thught about the project and how I can accomplish all the features in a practicable manner. For convinience I wanted to do the solution to be wireless but also without the hassle to connect to an existing network. The solution I came up with looks like that: 

<image overview.png>

### Client (Tallylight)

The actual lights consist of 6 addressable LEDs, five dip-switches and a ESP32 development board. The power is feed througth USB. Ether from an powerbank or a USB charger.
<image ClientInsight>
In the moment, everything is housed in a cheep, self crafted housing but I plan on creating a 3d-printed version of it. The tally has an antenna to improve the wifi-range and uses the flash-socket of the camera to mount on.
<image ClientOn Camera>

### Master (Basestation)

Because the 802.11 LR mode is specific for the ESP32 the master has to be an ESP32 also. It has to be connected to an USB port at the pc wich is running the control software. The clients connects on it's own with the master and the master routes the relevant information to each client. It should also detect the system health (like Tallys wich are offline or data inconsistencies). 
<image MasterClosed>
<image MasterOpened>

### Controler 
The Controler fullfills 3 roles. It connects with OBS and parses the neccesarry data for the system. It also communicates with the master to edit the tallys colors. As the brain of the system it also
provides a graphical user interface to configure the system, assign tallys to the coresponding scenes and let the user customize the colors and light intensity. The System has to be usefull in the sunny morning and in the dark church without distracting the audiance. The Controler must work reliable because it runs on the same system wich is live. Tecnicly it can be runn from any Device within the same network but for the better response time it should be run on the live system. 

<image ControlerUIConnected>

The Controler is programmed in ObjectPascal with the Lazarus IDE. Therefor it's mostly objectoriented designed and driven by events. The latest stable build of the controler is version 2.1, wich I am going to show you. It has the most importent fetures implemented and working plus some additional quality of life things. (It was also used in 3 different streams where I discovered more problems related to the reliability of the system and had the opurtunity to hear other opinions and experiences. )-->maybe remove...

The heavy lifting is done moastly by three classes: ttally, tscene and ttally_controler.
<image classdiagram>
As you can see the ttally_controler class is packt with a variety of functions. In the version 3.0, wich I am currently workin on, the ttally_controler gets reduced to its main functionality and the communication with the master gets outsourced. We will be concentrating on the init() procedure. (Pascal uses the word "procedure" in place of "void"); 

## Code Snippet
The init() procedure is called multiple times after the websocket connection with OBS is established. It gets the scene names, the current mode of OBS (normal or studio), the currently active (live) scene and, if the studio mode is used, the scene wich is currently in the preview. To do so it sends multiple request to OBS and decodes the returning JSON data. To prevent init() to block the programm while it waits for OBS to respond (that would make the UI non responsive for that timeframe) the procedure registers the next task -should OBS answer- into an tasklist.

The tasklist is like an internal todo list. The procedure check_tasklist() starts the function associated with the first task in the list and sets it as currenttask.

### tallycontroler.init()
init() reads the variable currenttask and decodes the response from OBS. For parsing the JSON I use then jsontools libary (https://github.com/sysrpl/JsonTools). It is simple to use: I create a tjsonnode (j:=tjsonnode.create;) and let it parse the received message. If that succeds I use j.Find('<keyword>') to get the value of that field.

Note that the order of requests matter. First I have to find out in wich mode OBS currently runs. After that I have to get a list of all scenes. The coresponding request delivers also the current output scene. After I have all the scenes created as objects and initialized I can request the preview scene, if the OBS runs the studio mode. The pointer of the current and the preview scene are  stored in extra variables for quick access.

The l.print() is a function of the tlog object. The controler is equiped with an log window wich I used for debugging. I show this later in the demo.

### demonstration of OBS connection
--Show how it connects and taht the information is correct
--Show the log
--Show some scene switching

## Conclusion of the Project

For now ther are more tallyligth solutions for OBS then it was at the start of the project but nothing similar to my solution. I am happy that the essential of the system work reliable enugth to be tested in producton. In the moment I redesign the serial communication between the Controler and the Master because I wasn't happy about the solution and the protocol design. As the project grows I extended the tlog libary with a commandline and autocompletion. Therfor debugging gets way easier because now I can trigger and test specific functions on it's own without reseting the programm or add extra elements to the UI.  

### Further improvements / ToDo's

The project does not end there. Like I mentioned befor, I want to add some more features and change specific things:
    -rewrite the Master firmware for better reliance and performance
    -implement ping feature and messure response time
    -dimm the light of each tally individualy
    -upgrade the Tallylights with a button to signal the director that the camera operator is ready to be live
    -update the TallyProtocol to transmit the pushbutton signal
    -design a proper housing for the ligths wich is easy to assemble
    -fix rendering issues in darkmode 
    -add signal and connection diagnosys to the tally system.
    -fix autocompletion bug on commandline
    -remove depricated functions and variables