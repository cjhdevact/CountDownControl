@echo off
echo [PACKOUT] 正在打包程序
if exist "%~dp0CountDownControl-Bin" rd /s /q "%~dp0CountDownControl-Bin"
md "%~dp0CountDownControl-Bin"
copy "%~dp0..\Src\CountDownControl\files\1-安装.bat" "%~dp0CountDownControl-Bin\1-安装.bat"
copy "%~dp0..\Src\CountDownControl\files\2-卸载.bat" "%~dp0CountDownControl-Bin\2-卸载.bat"
copy "%~dp0..\Src\CountDownControl\files\3-自动启动管理.bat" "%~dp0CountDownControl-Bin\3-自动启动管理.bat"
copy "%~dp0..\Src\CountDownControl\files\CountDownControlAdmxs.exe" "%~dp0CountDownControl-Bin\CountDownControlAdmxs.exe"
copy "%~dp0..\Src\CountDownControl\files\CountDownControl.adm" "%~dp0CountDownControl-Bin\CountDownControl.adm"
copy "%~dp0..\Src\CountDownControl\files\CountDownControl.xml" "%~dp0CountDownControl-Bin\CountDownControl.xml"
copy "%~dp0..\Src\CountDownControl\bin\Release\CountDownControl.exe" "%~dp0CountDownControl-Bin\CountDownControl.exe"
copy "%~dp0..\Src\CountDownControl\bin\x64\Release\CountDownControl.exe" "%~dp0CountDownControl-Bin\CountDownControl64.exe"
copy "%~dp0..\Src\CountDownControl\files\certmgr.exe" "%~dp0CountDownControl-Bin\certmgr.exe"
copy "%~dp0..\Src\CountDownControl\files\rootcert.cer" "%~dp0CountDownControl-Bin\rootcert.cer"
echo [PACKOUT] 打包完成
echo.