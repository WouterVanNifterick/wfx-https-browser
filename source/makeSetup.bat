:: This script creates a release (setup) package under project \bin subfolder using a prebuilt Delphi project.

@echo off

del /Q httpsbrowser.zip
copy /Y ..\res_text\*.* ..\bin
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::CreateFromDirectory('..\bin', 'httpsbrowser.zip'); }"
