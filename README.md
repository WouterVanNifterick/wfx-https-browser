# wfx-https-browser
HTTPS file system plugin for Total Commander (64-bit version)

Initial developer and sources: [Fabio Chelly](fabio@chelly.net).
HTTPS support: [Wouter van Nifterick](https://github.com/WouterVanNifterick)

HTTPS Browser is a plugin for Total Commander. It allows users to connect to websites via Total Commander and see links, pictures or any downloadable links as files. The plugin supports both HTTP and HTTPS protocols.

# Installation
The binary plugin archive comes with the setup script. Just enter the archive, and confirm installation.

# How to use it
1. Under Total Commander, access 'My Network places'
2. Double click on 'HTTPS Browser'
3. Click on 'Connect'
4. Enter the URL you want to browse and ENTER
5. The content of the page appears as a list of files
   - You can follow a link with a double click on it
   - You can download files with F5
   - you can preview pictures or pages with F3
   
User settings are stored in `httpbrowser.ini`. Check `httpbrowser.example.ini` for an example.

# Development
The plugin can be compiled with [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).