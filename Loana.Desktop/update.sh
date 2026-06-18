dotnet tool uninstall -g Loana.Desktop
dotnet pack
dotnet tool install -g --add-source bin/Release Loana.Desktop
