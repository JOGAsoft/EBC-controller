# EBC-controller
A GUI linux software to control ZTE Tech EBC series
battery testers and electronic loads.

Written in Free Pascal/Lazarus. 
Needs TLazSerial and Jujibo packages.

The aim is to provide the same or more functionallity as the Windows software
from ZKE Tech. Supports the EBC-A05 and EBC-A10H loads by default.
Other devices probably work. The user can add the identification byte for
any device in the .conf file (run the program once first to auto-generate
the .conf file).

Added features over the original software:
* More versatile cut offs, such as current, capacity or energy
* A software Constant Resistance (CR) mode
* Better loop controls for cycling programs 
   (nestled loops, loops until capacity drops)
* Shows more parameters (dV, dA, etc)

Hints:
The default window size is optimized for netbooks.
To save area for the graph, the main menu is accessed by right click.
The console view can be used to decode the protocol or identification byte
for other devices than the A05 and A10H.
