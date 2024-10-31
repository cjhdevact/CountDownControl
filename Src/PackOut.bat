@echo off
echo 任意键打包 倒计时小工具（CountDownControl）...
pause > nul
if exist "%~dp0CountDownControl-Bin" rd /s /q "%~dp0CountDownControl-Bin"
md "%~dp0CountDownControl-Bin"
copy "%~dp0CountDownControl\files\1-安装.bat" "%~dp0CountDownControl-Bin\1-安装.bat"
copy "%~dp0CountDownControl\files\2-卸载.bat" "%~dp0CountDownControl-Bin\2-卸载.bat"
copy "%~dp0CountDownControl\files\3-自动启动管理.bat" "%~dp0CountDownControl-Bin\3-自动启动管理.bat"
copy "%~dp0CountDownControl\files\CountDownControlAdmxs.exe" "%~dp0CountDownControl-Bin\CountDownControlAdmxs.exe"
copy "%~dp0CountDownControl\files\CountDownControl.adm" "%~dp0CountDownControl-Bin\CountDownControl.adm"
copy "%~dp0CountDownControl\files\CountDownControl.xml" "%~dp0CountDownControl-Bin\CountDownControl.xml"
copy "%~dp0CountDownControl\bin\Release\CountDownControl.exe" "%~dp0CountDownControl-Bin\CountDownControl.exe"
copy "%~dp0CountDownControl\bin\x64\Release\CountDownControl.exe" "%~dp0CountDownControl-Bin\CountDownControl64.exe"
echo.
echo 完成！
echo 任意键退出...
pause > nul