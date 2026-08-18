::Tips:
::Set the CSIGNCERT as your Certificate File path.
::Set the CSIGNTOOL as your SignPackPath
@echo off
path %CSIGNTOOL%;%path%
echo [SIGNTOOL] 正在进行数字签名
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControl.exe"
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControl64.exe"
cmd.exe /c signcmd.cmd "%CSIGNCERT%" "%~dp0CountDownControl-Bin\CountDownControlAdmxs.exe"
echo [SIGNTOOL] 数字签名结束
echo.