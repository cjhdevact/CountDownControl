::/*****************************************************\
::
::     CountDownControl - 2-卸载.bat
::
::     版权所有(C) 2024 CJH。
::
::     卸载批处理
::
::\*****************************************************/
@echo off
cls
title 倒计时小工具卸载程序
if "%1" == "/noadm" goto main
if "%1" == "/?" goto hlp
fltmc 1>nul 2>nul&& goto main
echo 正在获取管理员权限...
echo.
echo 可以使用 %0 /noadm 跳过Bat提权，但请手动以管理员身份运行
echo 如果当前窗口无限循环出现，或者未成功获取管理员权限，请注销当前用户或重启电脑，
echo 然后以管理员用户账号运行或手动以管理员身份运行。
if "%1" == "/mshtaadm" goto mshtaAdmin
if "%2" == "/mshtaadm" goto mshtaAdmin
if "%1" == "/psadm" goto powershellAdmin
if "%2" == "/psadm" goto powershellAdmin
ver | findstr "10\.[0-9]\.[0-9]*" >nul && goto powershellAdmin
:mshtaAdmin
rem 原理是利用mshta运行vbscript脚本给bat文件提权
rem 这里使用了前后带引号的%~dpnx0来表示当前脚本，比原版的短文件名%~s0更可靠
rem 这里使用了两次Net session，第二次是检测是否提权成功，如果提权失败则跳转到failed标签
rem 这有效避免了提权失败之后bat文件继续执行的问题
::Net session >nul 2>&1 || mshta vbscript:CreateObject("Shell.Application").ShellExecute("cmd.exe","/c ""%~dpnx0""","","runas",1)(window.close)&&exit
set parameters=
:parameter
@if not "%~1"=="" ( set parameters=%parameters% %~1& shift /1& goto :parameter)
set parameters="%parameters:~1%"
mshta vbscript:createobject("shell.application").shellexecute("%~dpnx0",%parameters%,"","runas",1)(window.close)&exit
cd /d "%~dp0"
Net session >nul 2>&1 || goto failed
goto main

:powershellAdmin
rem 原理是利用powershell给bat文件提权
rem 这里使用了两次Net session，第二次是检测是否提权成功，如果提权失败则跳转到failed标签
rem 这有效避免了提权失败之后bat文件继续执行的问题
Net session >nul 2>&1 || powershell start-process \"%0\" -argumentlist \"%1 %2\" -verb runas && exit
Net session >nul 2>&1 || goto failed
goto main

:failed
cls
echo.
echo 当前未以管理员身份运行。请手动以管理员身份运行本程序。
echo.
echo 任意键关闭... & pause > NUL
goto enda

:hlp
title 一键关闭课件小工具卸载程序
cls
echo.
echo ====================================================
echo                 倒计时小工具卸载程序
echo ====================================================
echo.
echo 你可以使用以下参数：
echo 2-卸载.bat [/noadm ^| /mshtaadm ^| /psadm]
echo.
echo /noadm 当检测到无管理员权限跳过自动提权。
echo /mshtaadm 强制使用mshta.exe自动提权。
echo /psadm 强制使用Powershell.exe自动提权。
echo.
goto enda

:main
cd /d "%~dp0"
title 倒计时小工具卸载程序
cls
echo.
echo ====================================================
echo                 倒计时小工具卸载程序
echo ====================================================
echo.
echo 卸载前建议关闭杀毒软件以及在UAC设置中设置UAC等级为最低，否则在卸载主程序以及自动启动项会被拦截导致卸载失败。
echo.
echo 任意键开始卸载... & pause >nul

cls
echo.
echo ====================================================
echo                 倒计时小工具卸载程序
echo ====================================================
echo.
echo 正在卸载中...
echo.
taskkill /f /im CountDownControl.exe
::ver|findstr "\<10\.[0-9]\.[0-9][0-9]*\>" > nul && (set netv=4)
ver|findstr "\<6\.[0-1]\.[0-9][0-9]*\>" > nul && (set netv=4c)
ver|findstr "\<6\.[2-9]\.[0-9][0-9]*\>" > nul && (set netv=4c)
ver|findstr "\<5\.[0-9]\.[0-9][0-9]*\>" > nul && (set netv=4c)

if "%PROCESSOR_ARCHITECTURE%"=="x86" goto x86
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" goto x64

:x86
Reg delete HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /f
del /q "%windir%\PolicyDefinitions\CountDownControl.admx"
del /q "%windir%\PolicyDefinitions\zh-CN\CountDownControl.adml"
del /q "%windir%\PolicyDefinitions\en-US\CountDownControl.adml"
del /q "%windir%\inf\CountDownControl.adm"
::Reg add "HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\Windows\Group Policy" /v OnlyUseLocalAdminFiles /t REG_DWORD /d 0 /f
::Reg delete "HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\Windows\Group Policy" /v OnlyUseLocalAdminFiles /f

rd /s /q "%windir%\CJH\CountDownControl"

rd /s /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\倒计时小工具"
::dir /a /s /b "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\CJH" | findstr . >nul && echo. || rd /s /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\CJH"

choice /C YN /T 5 /D N /M "是(Y)否(N)删除自定义设置（5秒后自动选择N）"
if errorlevel 1 set a=1
if errorlevel 2 set a=2
if "%a%" == "1" Reg delete HKCU\Software\CJH\CountDownControl /f

Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayIcon /f
Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayName /f
Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v Publisher /f
Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v UninstallString /f

cd /d "%programfiles%"
rd /s /q "%programfiles%\CJH\CountDownControl"
::dir /a /s /b "%windir%\CJH\" | findstr . >nul && echo. || rd /s /q "%windir%\CJH"

echo.
cls

echo.
echo ====================================================
echo                 倒计时小工具卸载程序
echo ====================================================
echo.
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo Windows XP 系统如果添加了倒计时小工具策略请手动在gpedit.msc（组策略）里的计算机管理-管理模板右键里的添加/删除模板里手动删除 %windir%\inf\CountDownControl.adm 策略文件。
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo.
echo 卸载完成！任意键退出... & pause > nul
goto enda

:x64
Reg delete HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /f
del /q "%windir%\PolicyDefinitions\CountDownControl.admx"
del /q "%windir%\PolicyDefinitions\zh-CN\CountDownControl.adml"
del /q "%windir%\PolicyDefinitions\en-US\CountDownControl.adml"
del /q "%windir%\inf\CountDownControl.adm"
::Reg add "HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\Windows\Group Policy" /v OnlyUseLocalAdminFiles /t REG_DWORD /d 0 /f
::Reg delete "HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\Windows\Group Policy" /v OnlyUseLocalAdminFiles /f

rd /s /q "%windir%\CJH\CountDownControl"

rd /s /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\倒计时小工具"
::dir /a /s /b "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\CJH" | findstr . >nul && echo. || rd /s /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\CJH"

choice /C YN /T 5 /D N /M "是(Y)否(N)删除自定义设置（5秒后自动选择N）"
if errorlevel 1 set a=1
if errorlevel 2 set a=2
if "%a%" == "1" Reg delete HKCU\Software\CJH\CountDownControl /f

Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayIcon /f
Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayName /f
Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v Publisher /f
Reg delete HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v UninstallString /f

cd /d "%programfiles%"
rd /s /q "%programfiles%\CJH\CountDownControl"
::dir /a /s /b "%windir%\CJH" | findstr . >nul && echo. || rd /s /q "%windir%\CJH"

echo.
cls

echo.
echo ====================================================
echo                 倒计时小工具卸载程序
echo ====================================================
echo.
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo Windows XP 系统如果添加了倒计时小工具策略请手动在gpedit.msc（组策略）里的计算机管理-管理模板右键里的添加/删除模板里手动删除 %windir%\inf\CountDownControl.adm 策略文件。
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo.
echo 卸载完成！任意键退出... & pause > nul
goto enda

:enda