::Tips Set the CSIGNCERT as your path.
@echo off
path D:\ProjectsTmp\SignPack;%path%
echo 任意键打包 倒计时小工具（CountDownControl）...
pause > nul
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControl.exe"
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControl64.exe"
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControlAdmxs.exe"
echo.
echo 完成！
echo 任意键退出...
pause > nul