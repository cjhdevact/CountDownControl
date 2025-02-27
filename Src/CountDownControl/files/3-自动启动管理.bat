::/*****************************************************\
::
::     CountDownControl - 3-自动启动管理.bat
::
::     版权所有(C) 2024-2025 CJH。
::
::     自动启动管理批处理
::
::\*****************************************************/
@echo off
cls
cd /d %~dp0
title 倒计时小工具自动启动管理
if "%1" == "/?" goto hlp
if "%1" == "/noadm" goto main
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
title 倒计时小工具自动启动管理
cls
echo.
echo ====================================================
echo                  倒计时小工具自动启动管理
echo ====================================================
echo.
echo 你可以使用以下参数：
echo 3-自动启动管理.bat [/noadm ^| /mshtaadm ^| /psadm]
echo.
echo /noadm 当检测到无管理员权限跳过自动提权。
echo /mshtaadm 强制使用mshta.exe自动提权。
echo /psadm 强制使用Powershell.exe自动提权。
echo.
goto enda

:main
cls
echo.
echo ====================================================
echo                  倒计时小工具自动启动管理
echo ====================================================
echo 请选择你的操作：
echo.
echo         1  添加自动启动
echo         2  删除自动启动
echo.
echo         3  添加自动启动（任务计划模式）
echo         4  删除自动启动（任务计划模式）
echo.
echo         5  退出
echo.
echo ========================================================
echo.
set /p chooice=请输入对应的数字以执行相应的操作：
if "%chooice%" == "1" goto ad1
if "%chooice%" == "2" goto de1
if "%chooice%" == "3" goto ad2
if "%chooice%" == "4" goto de12
if "%chooice%" == "5" goto enda
echo.
echo 无效的选项，任意键返回。 & pause >nul
goto main

:ad1
cls
echo ====================================================
echo                  倒计时小工具自动启动管理
echo ====================================================
Reg add HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /t REG_SZ /d "%programfiles%\CJH\CountDownControl\CountDownControl.exe" /f
echo 添加成功，任意键返回... & pause > nul
goto main

:ad2
cls
echo ====================================================
echo                  倒计时小工具自动启动管理
echo ====================================================
schtasks.exe /Delete /TN \CJH\CountDownControl /F
schtasks.exe /create /tn \CJH\CountDownControl /xml "%~dp0CountDownControl.xml"
echo 添加成功，任意键返回... & pause > nul
goto main

:de1
cls
echo ====================================================
echo                  倒计时小工具自动启动管理
echo ===================================================
Reg delete HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /f
echo 删除成功，任意键返回... & pause > nul
goto main

:de12
cls
echo ====================================================
echo                  倒计时小工具自动启动管理
echo ===================================================
schtasks.exe /Delete /TN \CJH\CountDownControl /F
echo 删除成功，任意键返回... & pause > nul
goto main

:enda
