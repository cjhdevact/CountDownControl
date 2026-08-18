::Tips:
::Set the CSIGNCERT as your Certificate File path.
::Set the CSIGNTOOL as your SignPackPath
@echo off
path %CSIGNTOOL%;%path%
echo [GetSHA256] 正在获取 SHA-256 校验
cmd.exe /c gesha256.cmd "%~dp0CountDownControl-Bin.7z"
echo [GetSHA256] SHA-256 校验结束
echo.