@echo off
echo �������� ����ʱС���ߣ�CountDownControl��...
pause > nul
if exist "%~dp0CountDownControl-Bin" rd /s /q "%~dp0CountDownControl-Bin"
md "%~dp0CountDownControl-Bin"
copy "%~dp0CountDownControl\files\1-��װ.bat" "%~dp0CountDownControl-Bin\1-��װ.bat"
copy "%~dp0CountDownControl\files\2-ж��.bat" "%~dp0CountDownControl-Bin\2-ж��.bat"
copy "%~dp0CountDownControl\files\3-�Զ���������.bat" "%~dp0CountDownControl-Bin\3-�Զ���������.bat"
copy "%~dp0CountDownControl\files\CountDownControlAdmxs.exe" "%~dp0CountDownControl-Bin\CountDownControlAdmxs.exe"
copy "%~dp0CountDownControl\files\CountDownControl.adm" "%~dp0CountDownControl-Bin\CountDownControl.adm"
copy "%~dp0CountDownControl\files\CountDownControl.xml" "%~dp0CountDownControl-Bin\CountDownControl.xml"
copy "%~dp0CountDownControl\bin\Release\CountDownControl.exe" "%~dp0CountDownControl-Bin\CountDownControl.exe"
copy "%~dp0CountDownControl\bin\x64\Release\CountDownControl.exe" "%~dp0CountDownControl-Bin\CountDownControl64.exe"
echo.
echo ��ɣ�
echo ������˳�...
pause > nul