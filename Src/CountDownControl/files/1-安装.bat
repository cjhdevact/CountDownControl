::/*****************************************************\
::
::     CountDownControl - 1-��װ.bat
::
::     ��Ȩ����(C) 2024 CJH��
::
::     ��װ������
::
::\*****************************************************/
@echo off
cls
title ����ʱС���߰�װ����
if "%1" == "/noadm" goto main
if "%1" == "/?" goto hlp
fltmc 1>nul 2>nul&& goto main
echo ���ڻ�ȡ����ԱȨ��...
echo.
echo ����ʹ�� %0 /noadm ����Bat��Ȩ�������ֶ��Թ���Ա�������
echo �����ǰ��������ѭ�����֣�����δ�ɹ���ȡ����ԱȨ�ޣ���ע����ǰ�û����������ԣ�
echo Ȼ���Թ���Ա�û��˺����л��ֶ��Թ���Ա������С�
if "%1" == "/mshtaadm" goto mshtaAdmin
if "%2" == "/mshtaadm" goto mshtaAdmin
if "%1" == "/psadm" goto powershellAdmin
if "%2" == "/psadm" goto powershellAdmin
ver | findstr "10\.[0-9]\.[0-9]*" >nul && goto powershellAdmin
:mshtaAdmin
rem ԭ��������mshta����vbscript�ű���bat�ļ���Ȩ
rem ����ʹ����ǰ������ŵ�%~dpnx0����ʾ��ǰ�ű�����ԭ��Ķ��ļ���%~s0���ɿ�
rem ����ʹ��������Net session���ڶ����Ǽ���Ƿ���Ȩ�ɹ��������Ȩʧ������ת��failed��ǩ
rem ����Ч��������Ȩʧ��֮��bat�ļ�����ִ�е�����
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
rem ԭ��������powershell��bat�ļ���Ȩ
rem ����ʹ��������Net session���ڶ����Ǽ���Ƿ���Ȩ�ɹ��������Ȩʧ������ת��failed��ǩ
rem ����Ч��������Ȩʧ��֮��bat�ļ�����ִ�е�����
Net session >nul 2>&1 || powershell start-process \"%0\" -argumentlist \"%1 %2\" -verb runas && exit
Net session >nul 2>&1 || goto failed
goto main

:failed
cls
echo.
echo ��ǰδ�Թ���Ա������С����ֶ��Թ���Ա������б�����
echo.
echo ������ر�... & pause > NUL
goto enda

:hlp
title һ���رտμ�С���߰�װ����
cls
echo.
echo ====================================================
echo                 ����ʱС���߰�װ����
echo ====================================================
echo.
echo �����ʹ�����²�����
echo 1-��װ.bat [/noadm ^| /mshtaadm ^| /psadm]
echo.
echo /noadm ����⵽�޹���ԱȨ�������Զ���Ȩ��
echo /mshtaadm ǿ��ʹ��mshta.exe�Զ���Ȩ��
echo /psadm ǿ��ʹ��Powershell.exe�Զ���Ȩ��
echo.
goto enda

:main
cd /d "%~dp0"
title ����ʱС���߰�װ����
cls
echo.
echo ====================================================
echo                 ����ʱС���߰�װ����
echo ====================================================
echo.
echo ��Ȩ����(C) 2024 CJH��
echo.
echo ��װǰ����ر�ɱ������Լ���UAC����������UAC�ȼ�Ϊ��ͣ������ڰ�װ����������ѡ��д���Զ�������ᱻ���ص��°�װʧ�ܡ�
echo.
echo �������ʼ��װ... & pause >nul

cls
echo.
echo ====================================================
echo                 ����ʱС���߰�װ����
echo ====================================================
echo.
echo ���ڰ�װ��...
echo.
taskkill /f /im CountDownControl.exe
::ver|findstr "\<10\.[0-9]\.[0-9][0-9]*\>" > nul && (set netv=4)
ver|findstr "\<6\.[0-1]\.[0-9][0-9]*\>" > nul && (set netv=4c)
ver|findstr "\<6\.[2-9]\.[0-9][0-9]*\>" > nul && (set netv=4c)
ver|findstr "\<5\.[0-9]\.[0-9][0-9]*\>" > nul && (set netv=4c)

if "%PROCESSOR_ARCHITECTURE%"=="x86" goto x86
if "%PROCESSOR_ARCHITECTURE%"=="AMD64" goto x64

:x86
echo.

Reg delete HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /f

if exist "%programfiles%\CJH\CountDownControl\CountDownControl.exe" del /q "%programfiles%\CJH\CountDownControl\CountDownControl.exe"

if not exist "%programfiles%\CJH\CountDownControl" md "%programfiles%\CJH\CountDownControl"
copy /y "%~dp0CountDownControl.exe" "%programfiles%\CJH\CountDownControl\CountDownControl.exe"
echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)Ҫ����Զ������5����Զ�ѡ��Y��"
if errorlevel 1 set aa=1
if errorlevel 2 set aa=2
if "%aa%" == "1" echo.
if "%aa%" == "1" echo �����ʱ��ͣ���ڴ˲����������Ƿ�ɱ��������ء�
if "%aa%" == "1" echo.
if "%aa%" == "1" Reg add HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /t REG_SZ /d "%programfiles%\CJH\CountDownControl\CountDownControl.exe" /f
echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)Ҫ��װ���Ե���ǰϵͳ����װ�����ʹ������Ա༭����ʱС���ߵĲ��ԣ���5����Զ�ѡ��Y��"
if errorlevel 1 set ac=1
if errorlevel 2 set ac=2
if "%ac%" == "1" if exist "%windir%\PolicyDefinitions\*.admx" call "%~dp0CountDownControlAdmxs.exe"
if "%ac%" == "1" if exist "%windir%\inf\*.adm" copy "%~dp0CountDownControl.adm" "%windir%\inf\CountDownControl.adm" /y
::if "%ac%" == "1" if exist "%windir%\inf\*.adm" Reg add "HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\Windows\Group Policy" /v OnlyUseLocalAdminFiles /t REG_DWORD /d 1 /f

echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)Ҫ������ݷ�ʽ����ʼ�˵���5����Զ�ѡ��Y��"
if errorlevel 1 set ad=1
if errorlevel 2 set ad=2
if "%ad%" == "1" if not exist "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����" md "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����"
if "%ad%" == "1" if exist "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС����.lnk" del /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС����.lnk"
if "%ad%" == "1" call mshta VBScript:Execute("Set a=CreateObject(""WScript.Shell""):Set b=a.CreateShortcut(""%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС����.lnk""):b.TargetPath=""%programfiles%\CJH\CountDownControl\CountDownControl.exe"":b.WorkingDirectory=""%programfiles%\CJH\CountDownControl"":b.Save:close")

copy /y "%~dp02-ж��.bat" "%programfiles%\CJH\CountDownControl\Uninstall.bat"

echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)���ж�س����б�5����Զ�ѡ��Y��"
if errorlevel 1 set ae=1
if errorlevel 2 set ae=2
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayIcon /t REG_SZ /d "%programfiles%\CJH\CountDownControl\CountDownControl.exe" /f
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayName /t REG_SZ /d "����ʱС���ߣ�CountDownControl��" /f
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v Publisher /t REG_SZ /d "CJH" /f
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v UninstallString /t REG_SZ /d "%programfiles%\CJH\CountDownControl\Uninstall.bat" /f

start "" "%programfiles%\CJH\CountDownControl\CountDownControl.exe"

echo.
cls

echo.
echo ====================================================
echo                 ����ʱС���߰�װ����
echo ====================================================
echo.
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo Windows XP ϵͳ���ֶ���gpedit.msc������ԣ���ļ��������-����ģ���Ҽ�������/ɾ��ģ�����ֶ���� %windir%\inf\CountDownControl.adm �����ļ���
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo.
echo ��װ��ɣ�������˳�... & pause > nul
goto enda

:x64
echo.
Reg delete HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /f

if exist "%programfiles%\CJH\CountDownControl\CountDownControl.exe" del /q "%programfiles%\CJH\CountDownControl\CountDownControl.exe"
if exist "%programfiles%\CJH\CountDownControl\x86\CountDownControl.exe" del /q "%programfiles%\CJH\CountDownControl\x86\CountDownControl.exe"

if not exist "%programfiles%\CJH\CountDownControl" md "%programfiles%\CJH\CountDownControl"
if not exist "%programfiles%\CJH\CountDownControl\x86" md "%programfiles%\CJH\CountDownControl\x86"
copy /y "%~dp0CountDownControl64.exe" "%programfiles%\CJH\CountDownControl\CountDownControl.exe"
copy /y "%~dp0CountDownControl.exe" "%programfiles%\CJH\CountDownControl\x86\CountDownControl.exe"
echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)Ҫ����Զ������5����Զ�ѡ��Y��"
if errorlevel 1 set aa=1
if errorlevel 2 set aa=2
if "%aa%" == "1" echo.
if "%aa%" == "1" echo �����ʱ��ͣ���ڴ˲����������Ƿ�ɱ��������ء�
if "%aa%" == "1" echo.
if "%aa%" == "1" Reg add HKLM\Software\Microsoft\Windows\CurrentVersion\run /v CountDownControl /t REG_SZ /d "%programfiles%\CJH\CountDownControl\CountDownControl.exe" /f
echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)Ҫ��װ���Ե���ǰϵͳ����װ�����ʹ������Ա༭����ʱС���ߵĲ��ԣ���5����Զ�ѡ��Y��"
if errorlevel 1 set ac=1
if errorlevel 2 set ac=2
if "%ac%" == "1" if exist "%windir%\PolicyDefinitions\*.admx" call "%~dp0CountDownControlAdmxs.exe"
if "%ac%" == "1" if exist "%windir%\inf\*.adm" copy "%~dp0CountDownControl.adm" "%windir%\inf\CountDownControl.adm" /y
::if "%ac%" == "1" if exist "%windir%\inf\*.adm" Reg add "HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\Windows\Group Policy" /v OnlyUseLocalAdminFiles /t REG_DWORD /d 1 /f

echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)Ҫ������ݷ�ʽ����ʼ�˵���5����Զ�ѡ��Y��"
if errorlevel 1 set ad=1
if errorlevel 2 set ad=2
if "%ad%" == "1" if not exist "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����" md "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����"
if "%ad%" == "1" if exist "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС����.lnk" del /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС����.lnk"
if "%ad%" == "1" if exist "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС���ߣ�32λ��.lnk" del /q "%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС���ߣ�32λ��.lnk"
if "%ad%" == "1" call mshta VBScript:Execute("Set a=CreateObject(""WScript.Shell""):Set b=a.CreateShortcut(""%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС����.lnk""):b.TargetPath=""%programfiles%\CJH\CountDownControl\CountDownControl.exe"":b.WorkingDirectory=""%programfiles%\CJH\CountDownControl"":b.Save:close")
if "%ad%" == "1" call mshta VBScript:Execute("Set a=CreateObject(""WScript.Shell""):Set b=a.CreateShortcut(""%systemdrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\����ʱС����\����ʱС���ߣ�32λ��.lnk""):b.TargetPath=""%programfiles%\CJH\CountDownControl\x86\CountDownControl.exe"":b.WorkingDirectory=""%programfiles%\CJH\CountDownControl\x86"":b.Save:close")

copy /y "%~dp02-ж��.bat" "%programfiles%\CJH\CountDownControl\Uninstall.bat"

echo.
choice /C YN /T 5 /D Y /M "��(Y)��(N)���ж�س����б�5����Զ�ѡ��Y��"
if errorlevel 1 set ae=1
if errorlevel 2 set ae=2
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayIcon /t REG_SZ /d "%programfiles%\CJH\CountDownControl\CountDownControl.exe" /f
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v DisplayName /t REG_SZ /d "����ʱС���ߣ�CountDownControl��" /f
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v Publisher /t REG_SZ /d "CJH" /f
if "%ae%" == "1" Reg add HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\CountDownControl /v UninstallString /t REG_SZ /d "%programfiles%\CJH\CountDownControl\Uninstall.bat" /f

start "" "%programfiles%\CJH\CountDownControl\CountDownControl.exe"

echo.
cls

echo.
echo ====================================================
echo                 ����ʱС���߰�װ����
echo ====================================================
echo.
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo Windows XP ϵͳ���ֶ���gpedit.msc������ԣ���ļ��������-����ģ���Ҽ�������/ɾ��ģ�����ֶ���� %windir%\inf\CountDownControl.adm �����ļ���
if "%ac%" == "1" if exist "%windir%\inf\*.adm" echo.
echo ��װ��ɣ�������˳�... & pause > nul
goto enda

:enda