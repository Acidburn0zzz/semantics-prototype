@echo off

nuget restore -PackagesDirectory .\packages

mkdir libs

pushd packages\FParsec.*\lib\net40-client
set FPARSEC=%CD%
popd

mklink /J libs\FParsec %FPARSEC%