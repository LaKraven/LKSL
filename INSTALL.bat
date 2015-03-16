@ECHO OFF

echo Setting Environment Variables for LKSL Paths

echo Registering LKSL_HOME
setx LKSL_HOME %~dp0
echo Registering LKSL_LIB
setx LKSL_LIB %~dp0Source\Lib
echo registering LKSL_PASCAL
setx LKSL_PASCAL %~dp0Source\Lib\Pascal
echo LKSL Paths added...
echo -------------------
echo The following Environment Variables have been registered:
echo LKSL_HOME		Points to the LKSL Root Directory
echo LKSL_LIB		Points to $(LKSL_HOME)\Source\Lib
echo LKSL_PASCAL		Points to $(LKSL_LIB)\Pascal

pause