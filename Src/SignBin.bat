::Tips Set the CSIGNCERT as your path.
@echo off
path D:\ProjectsTmp\SignPack;%path%
echo �������� ����ʱС���ߣ�CountDownControl��...
pause > nul
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControl.exe"
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControl64.exe"
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControlAdmxs.exe"
echo.
echo ��ɣ�
echo ������˳�...
pause > nul